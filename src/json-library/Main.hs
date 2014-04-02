module Main where
    import SimpleJSON
    import SimplePrint

    main = putStrLn (simplePrint (JSONObject [("helloString", JSONString "hello"), ("helloBool", JSONBool True), ("helloInt", JSONNumber 1)])) 