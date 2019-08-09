import Prelude hiding (replicate, take, repeat, map, filter)
import FoldableExercises
import Test.Hspec         (Spec, it, shouldReturn, shouldBe)
import Test.Hspec.Runner  (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do

    it "repeats things" $ do
        replicate 5 "hat" `shouldBe` ("hat" :| "hat" :| "hat" :| "hat" :| "hat" :| EndOfList)

    it "takes things" $ do
      take 2 ('a' :| 'b' :| 'c' :| 'd' :| EndOfList) `shouldBe` ('a' :| 'b' :| EndOfList)
      take 2 ('a' :| EndOfList) `shouldBe` ('a' :| EndOfList)

    it "counts down" $ do
      countDownFrom 3 `shouldBe` (3 :| 2 :| 1 :| 0 :| EndOfList)

    it "maps" $ do
      map (+1) (1 :| 3 :| 5 :| EndOfList) == (2 :| 4 :| 6 :| EndOfList)

    it "filter" $ do
      filter (/=3) (1 :| 3 :| 5 :| EndOfList) == (1 :| 5 :| EndOfList)
