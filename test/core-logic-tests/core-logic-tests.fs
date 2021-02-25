module CoreTests

open NUnit.Framework
open CoreLogic

[<SetUp>]
let Setup () =
    ()

[<Test>]
let helloTest () =
    let name = "test"
    let expected = sprintf "Hello %s" name
    let actual = Say.hello name
    Assert.That(actual, Is.EqualTo(expected))
