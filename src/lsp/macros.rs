macro_rules! handle_request {
    (
        $conn:expr,
        $req:expr,
        $($req_type:ident => $params:ident -> $handler:expr),*
    ) => {
        {
            use lsp_types::request::Request;
            use lsp_server::Response;
            use lsp_server::ResponseError;
            use lsp_server::Message;

            match $req.method.as_str() {
            $(
                <$req_type as Request>::METHOD => {
                    let id = $req.id.clone();
                    let method = $req.method.clone();
                    let extract_params = serde_json::from_value($req.params).unwrap_or_else(|err| {
                        panic!("Invalid request\nMethod: {}\n error: {}", method, err)
                    });

                    let $params:<$req_type as Request>::Params = extract_params;
                    let result: Result<<$req_type as Request>::Result> = $handler;
                    match result {
                        Ok(result) => {
                            let result = serde_json::to_value(result).unwrap();
                            let response = Response {
                                id,
                                result: Some(result),
                                error: None,
                            };
                            $conn.sender.send(Message::Response(response))?;
                        }

                        Err(err) => {
                            let error = ResponseError {
                                code: -42000,
                                message: format!("{}", err),
                                data: None
                            };
                            let response = Response {
                                id,
                                result: None,
                                error: Some(error),
                            };
                            $conn.sender.send(Message::Response(response))?;
                        }
                    }
                }
            )*
        _ => ()
        }
    }
    };
}

macro_rules! handle_notification {
    (
        $not:expr,
        $($not_type:ident => $params:ident -> $handler:expr),*
    ) => {
        {
            use lsp_types::notification::Notification;

            match $not.method.as_str() {
            $(
                <$not_type as Notification>::METHOD => {
                    let method = $not.method.clone();
                    let params = serde_json::from_value($not.params).unwrap_or_else(|err| {
                        panic!("Invalid notification\nMethod: {}\n error: {}", method, err)
                    });

                    let $params:<$not_type as Notification>::Params = params;
                    $handler
                }
            )*
        _ => ()
        }
    }
    };
}
