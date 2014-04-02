module SimplePrint (simplePrint) where
    import SimpleJSON
    import Data.List (intercalate)
    
    simplePrint :: JSONValue -> String
    -- simple types
    simplePrint (JSONString x) = x
    simplePrint (JSONBool x) | x = "True"
                             | otherwise = "False"
    simplePrint (JSONNumber x) = show x
    simplePrint (JSONNull) = "NULL"
    
    -- compound types
    simplePrint (JSONArray x) = "[" ++ intercalate ", " (foldr f [] x) ++ "]"
                                                where f x acc = [simplePrint x] ++ acc
                                                
    simplePrint (JSONObject x) = "{" ++ intercalate ", " (foldr f [] x) ++ "}"
                                 where f x acc = ["\"" ++ (fst x) ++ "\"" ++ ": " ++ (simplePrint (snd x))] ++ acc  