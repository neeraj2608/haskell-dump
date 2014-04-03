module Main where
    import SimpleJSON
    import SimplePrint

    main = printJSON (JSONObject [("helloString", JSONString "hello"), ("helloBool", JSONBool True), ("helloInt", JSONNumber 1)]) 