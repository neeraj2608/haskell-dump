module Main where
    import SimpleJSON -- provides JSONValue definitions
    import SimplePrint -- naive printing
    import Prettify -- provides Doc and conversion from JSONValue to Doc functionality
    import PrettyPrint -- provides width aware printing

    main = PrettyPrint.printJSON 100 (JSONObject [("helloString", JSONString "hello"), ("helloBool", JSONBool True), ("helloInt", JSONNumber 1)]) 
    --main = PrettyPrint.printJSON 50 (JSONObject [("f", JSONNumber 1), ("q", JSONBool True)]) 