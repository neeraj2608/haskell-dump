module FSTest where

import FS
import Types
import Test.QuickCheck
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
           quickCheck (isNothing test1)
           quickCheck (liftM (getName . snd) test2 == Just "skull_man(scary).bmp")
           quickCheck (liftM (getName . snd) test3 == Just "root")
           quickCheck (liftM (getName . snd) test4 == Just "pics")
           quickCheck (liftM (getName . snd) test5 == Just "blah")
           quickCheck (liftM (getName . snd) test6 == Just "test")
           quickCheck (liftM (getName . snd) test7 == Just "test")
           quickCheck prop_newfile_file
           quickCheck prop_down_file

prop_newfile_file :: FSZipper -> Property
prop_newfile_file x = isFile x ==> fsNewFile (File "test" "testdata") x == Just x

prop_down_file :: FSZipper -> Property
prop_down_file x = isFile x ==> fsDown "a" x == Just x

isFile :: FSZipper -> Bool
isFile (_, File _ _) = True
isFile _ = False

test1 :: Maybe FSZipper
test1 = Just ([],testDisk) >>= fsDown "pics" >>= fsDown "skull_man(scary).bm" -- will return a Nothing

test2 :: Maybe FSZipper
test2 = Just ([],testDisk) >>= fsDown "pics" >>= fsDown "skull_man(scary).bmp" -- will return a Just "skull_man(scary).bmp"

test3 :: Maybe FSZipper
test3 = Just ([],testDisk) >>= fsUp >>= fsUp >>= fsUp -- will stay at root

test4 :: Maybe FSZipper
test4 = Just ([],testDisk) >>= fsUp >>= fsDown "pics" -- will return a Just "pics"

test5 :: Maybe FSZipper
test5 = Just ([],testDisk) >>= fsUp >>= fsDown "pics" >>= fsRename "blah" -- will return a Just "blah"

test6 :: Maybe FSZipper
test6 = Just ([],testDisk) >>= fsUp >>= fsDown "pics" >>= fsNewFile (File "test" "testdata") >>= fsDown "test" -- will return a Just "test"

test7 :: Maybe FSZipper
test7 = Just ([],testDisk) >>= fsUp >>= fsDown "pics" >>= fsNewFile (Folder "test" []) >>= fsDown "test" -- will return a Just "test"

test8 :: Maybe FSZipper
test8 = Just ([],testDisk) >>= fsUp >>= fsDown "pope_time.avi" >>= fsNewFile (Folder "test" []) >>= fsDown "test" -- will return a Just "test"

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
