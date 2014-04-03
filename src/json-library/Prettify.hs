-- creates intermediate Doc representation from a JSONValue
module Prettify (convertJSONValueToDoc, Doc) where
    import SimpleJSON
    import Data.List (intersperse)
    import Data.Maybe (fromJust)
    
    data Doc = Empty |
               Text String | --used for plain chars, numbers, special values such as "True", "False", "NULL"
               Line |
               Concat Doc Doc | --used for strings. Strings are Concats of chars are wrapped in Texts.
               Union Doc Doc
               deriving (Show, Eq)
           
    ----------------------------    
    -- Doc conversion functions
    ----------------------------    
    stringToDoc :: String -> Doc
    stringToDoc = enclose '"' '"' . concatDocArray . map charToDoc    
    
    -- no hex escaping for simplicity
    charToDoc :: Char -> Doc
    charToDoc x = case lookup x escapedMetaChars of
                    Just escapedMetaChar -> Text escapedMetaChar -- special metacharacter
                    Nothing -> Text [x] -- normal printable character
    
    -- helper for charToDoc
    -- meta characters and their escaped equivalents
    escapedMetaChars :: [(Char, String)]
    escapedMetaChars = zipWith f "\n\r\f\t\b\\/" "nrftb\\/"
                       where f :: Char -> Char -> (Char, String)
                             f x y = (x,['\\',y])
                             
    simpleEscapes    = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                       where ch a b = (a, ['\\',b])
    
    numberToDoc :: Double -> Doc
    numberToDoc = Text . show
               
    convertJSONValueToDoc :: JSONValue -> Doc
    convertJSONValueToDoc (JSONString x) = stringToDoc x
    convertJSONValueToDoc (JSONBool x) | x = Text "True"
                                       | otherwise = Text "False"
    convertJSONValueToDoc (JSONNumber x) = numberToDoc x
    convertJSONValueToDoc (JSONNull) = Text "NULL"
    
    convertJSONValueToDoc (JSONArray []) = Empty
    convertJSONValueToDoc (JSONArray x) = series '[' ']' ',' convertJSONValueToDoc x
    
    convertJSONValueToDoc (JSONObject x) = series '{' '}' ',' f x
                                           where f :: (String, JSONValue) -> Doc
                                                 f x = stringToDoc (fst x) <> charToDoc ':' <> charToDoc ' ' <> convertJSONValueToDoc (snd x) -- the space appears after the colon
    
    series :: Char -> Char -> Char -> (a -> Doc) -> [a] -> Doc
    series openChar closeChar separatorChar mapFn = enclose openChar closeChar . concatDocArrayWithJoinLines . intersperse (charToDoc separatorChar <> charToDoc ' ') . map mapFn -- add an extra space after the separator character
    
    ----------------------------    
    -- Doc utility functions
    ----------------------------    
    -- wraps a Doc in two chars (themselves wrapped in a doc) and makes a Doc out of it
    enclose :: Char -> Char -> Doc -> Doc
    enclose leftChar rightChar docIn = charToDoc leftChar <> docIn <> charToDoc rightChar 
    
    -- combine two Docs into one
    (<>) :: Doc -> Doc -> Doc
    (<>) Empty x = x
    (<>) x Empty = x
    (<>) x y = Concat x y -- this is the Concat type constructor
    
    -- convert a list of Docs into one single Doc
    concatDocArray :: [Doc] -> Doc
    concatDocArray x = foldr f Empty x
                       where f :: Doc -> Doc -> Doc
                             f x acc = x <> acc
                         
    -- convert a list of Docs into one single Doc. Replace Line with a space on the right of the Union.
    concatDocArrayWithJoinLines :: [Doc] -> Doc
    concatDocArrayWithJoinLines x = foldr f Empty x
                                    where f Line acc = Union (charToDoc ' ' <> acc) (Line <> acc) -- this line means the left hand of the Union can potentially be wider than the Right
                                          f x acc = Union (x <> acc) (x <> acc)
                                          
    -- compact data for transferring over the wire
    compact :: Doc -> String
    compact Empty = ""
    compact x = transform [x] 
                where transform :: [Doc] -> String
                      transform [] = ""
                      transform (x:xs) | x == Empty = transform xs
                                       | x == Line = "\n" ++ transform xs
                                       | otherwise = case x of
                                                       Text x -> x ++ transform xs
                                                       Concat y z -> transform (y:z:xs)
                                                       Union y z -> transform (z:xs) -- pick the right hand side
