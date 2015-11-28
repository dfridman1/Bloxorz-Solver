module Generalization
       (
         buildGraph,
         Graph
       ) where


import qualified Data.Set as Set



type Memo  a = Set.Set a
type Graph a = [a]



{-
   In the following functions:
     * 'f' takes a state (type 'a') and returns an entity (type 'b') to be memorized
     * 'g' takes a state (type 'a') and returns its neighbors (type '[a]')
-}



buildGraph :: Ord b => (a -> b) -> (a -> [a]) -> a -> Graph a
buildGraph f g startState = build memo f g [startState]
    where memo = Set.singleton $ f startState



build :: Ord b => Memo b -> (a -> b) -> (a -> [a]) -> Graph a -> Graph a
build _ _ _ [] = []
build m f g xs  = let more    = concatMap (newNeighborsOnly m f g) xs
                      newMemo = m `Set.union` (Set.fromList $ map f more) in
                   xs ++ build newMemo f g more



newNeighborsOnly :: Ord b => Memo b -> (a -> b) -> (a -> [a]) -> a -> [a]
newNeighborsOnly m f g = filter (\x -> f x `Set.notMember` m) . g
