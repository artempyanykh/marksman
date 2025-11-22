@ECHO OFF
SET rootDir=%~dp0..
make -C %rootDir% build
%rootDir%\Marksman\bin\Debug\net9.0\marksman.exe server -v=4