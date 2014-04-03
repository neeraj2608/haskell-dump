module PrettyPrint (printJSON) where
    import SimpleJSON -- provides JSONValue definitions
    import Prettify -- provides Doc and conversion from JSONValue to Doc functionality
    
    printJSON :: Int -> JSONValue -> IO()
    printJSON x = putStrLn . (prettyPrint x) . convertJSONValueToDoc
    
    -- this doesn't take care of a name-value combination being longer than width
    -- it only handles line breaks between *successive* pair-values
    -- e.g. for column width of 10, (JSONObject [("helloString", JSONString "hello"), ("helloBool", JSONBool True), ("helloInt", JSONNumber 1)]) gets printed as
    -- {"helloString": "hello"
    -- , 
    -- "helloBool": True
    -- , 
    -- "helloInt": 1.0
    -- }
    -- The function breaks successive pairs up into individual lines but it doesn't care if the name-value pair itself is longer than the allowed width 
    prettyPrint :: Int -> Doc -> String
    prettyPrint width doc = updateCol 0 [doc]
                            where updateCol _ [] = ""
                                  updateCol rightmostColWidth (x:xs) | x == Empty = updateCol rightmostColWidth xs
                                                       | x == Line = "\n" ++ updateCol 0 xs
                                                       | otherwise = case x of
                                                                       Text y -> y ++ (updateCol (rightmostColWidth + (length y)) xs)
                                                                       Concat y z -> updateCol rightmostColWidth (y:z:xs)
                                                                       Union y z -> chooseOptionThatFits rightmostColWidth (updateCol rightmostColWidth (y:xs)) (updateCol rightmostColWidth (z:xs))
                                                                                    where chooseOptionThatFits :: Int -> String -> String -> String
                                                                                          chooseOptionThatFits rightmostColWidth leftString rightString | leftString `fitsIn` (width-rightmostColWidth) = leftString
                                                                                                                                                        | otherwise = rightString
                                                                                          fitsIn :: String -> Int -> Bool
                                                                                          fitsIn string colWidthLeft | colWidthLeft < 0 = False
                                                                                          fitsIn "" _ = True
                                                                                          fitsIn ('\n':_) colWidthLeft = True
                                                                                          fitsIn (x:xs) colWidthLeft = fitsIn xs (colWidthLeft - 1)