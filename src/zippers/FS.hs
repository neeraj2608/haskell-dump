{- Filesystem using Zippers, based on learn you a haskell - zippers
   We use monadic zippers here -}
module FS where

import Types
import Control.Monad

-------------------------------------------------------------------------------
main :: Maybe String
main = do
    test4 >>= (return . getName . snd)
    -- equivalent to
    -- liftM (getName . snd) test2

getName :: FSItem -> Name
getName (File x _) = x
getName (Folder x _) = x

test1 = Just ([],testDisk) >>= fsDown "pics" >>= fsDown "skull_man(scary).bm" -- will return a Nothing
test2 = Just ([],testDisk) >>= fsDown "pics" >>= fsDown "skull_man(scary).bmp" -- will return a Just "skull_man(scary).bmp"
test3 = Just ([],testDisk) >>= fsUp >>= fsUp >>= fsUp -- will stay at root
test4 = Just ([],testDisk) >>= fsUp >>= fsDown "pics" -- will return a Just "pics"

testDisk :: FSItem  
testDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]

-------------------------------------------------------------------------------
-- FS functionality
-------------------------------------------------------------------------------
fsUp :: FSZipper -> Maybe FSZipper
fsUp (((FSCrumb name before after):xs), y) = Just (xs, Folder name (before++[y]++after))
fsUp x@([], _) = Just x

fsDown :: Name -> FSZipper -> Maybe FSZipper
fsDown toName (xs, Folder name items) = case after of
    [] -> Nothing
    _ -> Just (y:xs, z)
    where
        z = head after
        y = FSCrumb name before (tail after)
        (before,after) = break matchName items

        matchName :: (FSItem -> Bool)
        matchName x = ((==) toName $ getName x)
        
fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename = undefined