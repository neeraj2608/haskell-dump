module SimpleJSON (
      JSONValue(..), -- .. indicates we also export the type constructors
    )where
    data JSONValue = JSONString String |
                     JSONBool Bool |
                     JSONNumber Double |
                     JSONNull |
                     JSONArray [JSONValue] |
                     JSONObject [(String, JSONValue)]
                     deriving (Show, Eq, Ord) 
           
    -- accessors          
    getString :: JSONValue -> (Maybe String)
    getString (JSONString y) = Just y
    getString _ = Nothing
    
    getBool :: JSONValue -> Maybe Bool
    getBool (JSONBool x) = Just x
    getBool _ = Nothing
    
    getDouble :: JSONValue -> Maybe Double
    getDouble (JSONNumber x) = Just x
    getDouble _ = Nothing
    
    getInt :: JSONValue -> Maybe Int
    getInt (JSONNumber x) = Just (truncate x)
    getInt _ = Nothing
    
    isNull :: JSONValue -> Bool
    isNull JSONNull = True
    isNull _ = False
    
    getArray :: JSONValue -> Maybe [JSONValue]
    getArray (JSONArray x) = Just x
    getArray _ = Nothing
    
    getObject :: JSONValue -> Maybe [(String, JSONValue)]
    getObject (JSONObject x) = Just x
    getObject _ = Nothing
                  
