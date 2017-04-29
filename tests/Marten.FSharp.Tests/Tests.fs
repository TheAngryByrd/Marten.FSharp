module Tests


open Expecto

[<Tests>]
let tests =
  testList "samples" [
    testCase "Say hello all" <| fun _ ->
      let subject = "all"
      Expect.equal subject "Hello all" "You didn't say hello"
    
  ]