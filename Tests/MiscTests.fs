module Marksman.MiscTests

open Xunit

open Misc

module StringExtensionsTests =
    [<Fact>]
    let isSubSequenceOf_1 () =
        Assert.True("f".IsSubSequenceOf("fsharp"))
        
    [<Fact>]
    let isSubSequenceOf_2 () =
        Assert.True("fsharp".IsSubSequenceOf("fsharp"))
        
    [<Fact>]
    let isSubSequenceOf_3 () =
        Assert.True("fr".IsSubSequenceOf("fsharp"))
        
    [<Fact>]
    let isSubSequenceOf_4 () =
        Assert.True("".IsSubSequenceOf("fsharp"))
        
    [<Fact>]
    let isSubSequenceOf_5 () =
        Assert.False("Md".IsSubSequenceOf("fsharp"))
