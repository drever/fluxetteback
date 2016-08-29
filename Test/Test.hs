import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Model

import Control.Monad.Random

testsolutions :: [(Card, Card, Card)]
testsolutions = [ (Card Red One Diamond Full, Card Green Two Diamond Full, Card Blue Three Diamond Full)
                , (Card Blue One Diamond Half, Card Blue One Circle Empty, Card Blue One Box Full)]

testNonSolutions :: [(Card, Card, Card)]
testNonSolutions = [ (Card Red One Diamond Full, Card Red One Box Empty, Card Green Two Box Empty),
                     (Card Green One Diamond Full, Card Green One Diamond Empty, Card Green Two Diamond Empty) ]

triplesToCards :: [(a, a, a)] -> [a]
triplesToCards ((x, y, z):xs) = x:y:z:triplesToCards xs
triplesToCards [] = []

main :: IO ()
main = hspec $ do
  describe "solutionCards" $ do
    it ("This has has two solutions: " ++ show testsolutions) $ do
      length (filter isSolution testsolutions) `shouldBe` 2
    it "It also has two solutions with the function 'solutions'" $ do
      length (solutions (triplesToCards testsolutions)) `shouldBe` 2
    it ("This has no solution: " ++ show testNonSolutions) $ do
      length (filter isSolution testNonSolutions) `shouldBe` 0
    it "It also has no solutions with the function 'solutions'" $ do
      length (solutions (triplesToCards testNonSolutions)) `shouldBe` 0
    it "The inital game always contains a solution" $
      property $ \seed -> let g = evalRand initGame (mkStdGen seed)
                          in not . null $ solutions (gameDealt g)
    it "Shows games with solutions after removing three cards from the inital game" $
      property $ \seed -> let g = evalRand initGame (mkStdGen seed)
                          in let g' = removeCards (take 3 $ (gameDealt g)) g
                             in not . null $ solutions (gameDealt g')
    -- it "Serves solutions until there are no more solutions" $
    --   property $ \seed -> let (Game a d _) = evalRand (solveGame <$> initGame) (mkStdGen seed)
    --                       in null $ solutions (a ++ d)
  describe "removeCards" $ do
    it "Keeps the number of cards constant" $
      property $ \g@(Game a d c) -> let g'@(Game a' d' c') = removeCards (take 3 $ d) g
                                    in (length $ a ++ d ++ c) == (length $ a' ++ d' ++ c')



solveGame :: Game -> Game
solveGame g@(Game a d c) = do
  case solutions d of
    [] -> g
    (x1, x2, x3):xs -> solveGame $ removeCards [x1, x2, x3] g



