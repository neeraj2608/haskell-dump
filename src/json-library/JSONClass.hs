{-# LANGUAGE FlexibleInstances, OverlappingInstances  #-}
-- FlexibleInstances lets us create instances of specialized polymorphic types e.g. instance JSON String where String = [Char] and [Char]
-- is a specialized form of the polymorphic type [a]
-- OverlappingInstances forces GHC to choose the more specific of multiple competing instance definitions
import Control.Arrow (second)
type JSONError = String

class JSON a where
  toJSONValue :: a -> JSONValue
  fromJSONValue :: JSONValue -> Either JSONError a

instance JSON JSONValue where
  toJSONValue = id
  fromJSONValue = Right
  
instance JSON Bool where
  toJSONValue = JSONBool
  fromJSONValue (JSONBool x) = Right x
  fromJSONValue _ = Left "not a JSONBool"

instance JSON String where
  toJSONValue = JSONString
  fromJSONValue (JSONString x) = Right x
  fromJSONValue _ = Left "not a JSONString"

doubleToNumber :: (Double -> a) -> JSONValue -> Either JSONError a
doubleToNumber f (JSONNumber x) = Right (f x)
doubleToNumber f _ = Left "not a JSONNumber"

instance JSON Int where
  toJSONValue = JSONNumber . realToFrac
  fromJSONValue = doubleToNumber round
  
instance JSON Integer where
  toJSONValue = JSONNumber . realToFrac
  fromJSONValue = doubleToNumber round
  
instance JSON Double where
  toJSONValue = JSONNumber
  fromJSONValue = doubleToNumber id

-- the following two lines cause an overlapping instance problem resolved with a pragma  
instance (JSON a) => JSON [a] where
  toJSONValue = undefined
  fromJSONValue = undefined 
  
instance (JSON a) => JSON [(String, a)] where
  toJSONValue = undefined
  fromJSONValue = undefined 
  
-- using newtypes to get around the overlapping instance problem
newtype JAry x = JAry { -- we will NOT export this data constructor in order that this type remains abstract. instead, we will export a special function that applies the constructor for us
                   fromJAry :: [x]
                 } deriving (Eq, Show, Ord)

-- this is the function that we will export                 
jary :: [a] -> JAry a
jary = JAry
                 
newtype JObj x = JObj {
                   fromJObj :: [(String, x)]
                 } deriving (Eq, Show, Ord)
                 
data JSONValue = JSONString String |
                 JSONBool Bool |
                 JSONNumber Double |
                 JSONNull |
                 JSONArray (JAry JSONValue) |
                 JSONObject (JObj JSONValue)
                 deriving (Show, Eq, Ord) 
                 
-- now we add JSON instances for the JAry and JObj types
instance (JSON a) => JSON (JAry a) where
  toJSONValue = fromJArytoJSONValue where
                  fromJArytoJSONValue :: (JSON a) => (JAry a) -> JSONValue
                  -- explanation
                  -- fromJAry first gives us back [a] from (JAry a)
                  -- to that list, we map toJSONValue
                  -- the resultant list we wrap in a JAry
                  -- that we pass to a JSONArray constructor to get the JSONValue 
                  fromJArytoJSONValue = JSONArray . jary . (map toJSONValue) . fromJAry
  fromJSONValue = jaryFromJSONValue
  
jaryFromJSONValue (JSONArray (JAry a)) =  whenRight JAry (mapEithers fromJSONValue a)
jaryFromJSONValue _ = Left "not a JSON Array"

whenRight :: (b->c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right x) = Right (f x)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                      Left err -> Left err
                      Right ys -> case f x of
                                  Left err -> Left err
                                  Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JObj a) where
  toJSONValue = JSONObject . JObj. map (second toJSONValue) . fromJObj
  fromJSONValue = jobjFromJSONValue
  
jobjFromJSONValue (JSONObject (JObj a)) = whenRight JObj (mapEithers unwrap a) where
                                          unwrap (k,v) = whenRight ((,) k) (fromJSONValue v)
jobjFromJSONValue _ = Left "not a JSON Object"