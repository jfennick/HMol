{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module BSP where

import Data.List hiding (insert)
import Geometry
import MainTypes
import MolTypes

{--class Bounds' rep where
  getBounds' :: obj -> rep
  withinBounds' :: obj -> rep -> Bool
  lessthanBounds' :: (Floating a, Ord a) => rep -> rep -> a -> Bool
  greaterthanBounds' :: (Floating a, Ord a) => rep -> rep -> a -> Bool
  subdividebounds' :: rep -> rep -> rep --}

class (Com objT repT) => SpacePart spT objT repT where
  isWithinBounds :: repT -> spT (objT repT) repT -> Bool
  insertOne :: spT (objT repT) repT -> objT repT -> spT (objT repT) repT
  insertAll :: [objT repT] -> spT (objT repT) repT
  getAllElems :: spT (objT repT) repT -> [objT repT]
  getSubParts :: spT (objT repT) repT -> [spT (objT repT) repT]
  elemsWithinDistCoord :: repT -> Double -> spT (objT repT) repT -> [objT repT]
  elemsWithinDistCoord = elemsWithinDistCoordWith getcom
  elemsWithinDistObj :: objT repT -> Double -> spT (objT repT) repT -> [objT repT]
  elemsWithinDistObj obj = elemsWithinDistCoord (getcom obj)
  elemsWithinDistCoordWith :: (objT repT -> repT) -> repT -> Double -> spT (objT repT) repT -> [objT repT]

instance SpacePart BSPTree Dat [Double] where
  isWithinBounds = isWithinBSP
  insertOne = insertBSP
  insertAll = foldl' insertBSP' (BSP 10 defaultbounds $ Right [])
  getAllElems = getAllElemsBSP
  getSubParts = getSubPartsBSP
  elemsWithinDistCoordWith = elemsWithinDistCoordWithBSP

instance SpacePart BSPTree Dat D3 where
  isWithinBounds = isWithinBSPD3
  insertOne = insertBSPD3'
  insertAll = foldl' insertBSPD3' (BSP 10 defaultboundsD3 $ Right [])
  getAllElems = getAllElemsBSP
  getSubParts = getSubPartsBSP
  elemsWithinDistCoordWith = elemsWithinDistCoordWithBSPD3

data Bounds repT = B repT repT deriving (Show)--mins maxes
defaultbounds :: Bounds [Double]
defaultbounds = B (replicate 3 (-60)) (replicate 3 60)
data BSPTree objT repT = BSP Int (Bounds repT) (Either [BSPTree objT repT]  [objT]) deriving (Show)
--nelems bounds Either [trees] [elems]

defaultboundsD3 :: Bounds D3
defaultboundsD3 = B (-60,-60,-60) (60,60,60)

isWithinBounds' :: Ord a => Bounds [a] -> [a] -> Bool
isWithinBounds' (B mins maxes) coords =
  foldl' (&&) True $ zipWith3 (\co mi ma -> (co > mi) && (co <= ma)) coords mins maxes

isWithinBoundsD3 :: (Floating a, Ord a) => Bounds (a, a, a) -> (a, a, a) -> Bool
isWithinBoundsD3 (B (x1,y1,z1) (x2,y2,z2)) (x3,y3,z3) =
  (x3 > x1 && x3 <= x2) && (y3 > y1 && y3 <= y2) && (z3 > z1 && z3 <= z2)

mindist, maxdist :: (Floating a, Ord a) => a -> a -> a -> a
mindist x1 x2 x3 | x3 < x1 = x1 - x3 | x3 > x2 = x3 - x2 | otherwise = 0.0
maxdist x1 x2 x3 | x3 < x1 = x2 - x3 | x3 > x2 = x3 - x1 | otherwise =  max (x2 - x3) (x3 - x1)

--returns True if the distance between coords and the *closest* corner of Bounds is <= dist
isCloserThanMinBounds :: (Floating a, Ord a) => Bounds [a] -> [a] -> a -> Bool
isCloserThanMinBounds (B mins maxes) coords dist =
  dist > magnitude (zipWith3 mindist mins maxes coords)

isCloserThanMinBoundsD3 :: (Floating a, Ord a) => Bounds (a, a, a) -> (a, a, a) -> a -> Bool
isCloserThanMinBoundsD3 (B (x1,y1,z1) (x2,y2,z2)) (x3,y3,z3) dist =
  let dx = mindist x1 x2 x3; dy = mindist y1 y2 y3; dz = mindist z1 z2 z3 in
  dist > sqrt (dx*dx + dy*dy + dz*dz)

--returns True if the distance between coords and the *furthest* corner of Bounds is <=  dist
isCloserThanMaxBounds :: (Floating a, Ord a) => Bounds [a] -> [a] -> a -> Bool
isCloserThanMaxBounds (B mins maxes) coords dist =
  dist > magnitude (zipWith3 maxdist mins maxes coords)

isCloserThanMaxBoundsD3 :: (Floating a, Ord a) => Bounds (a, a, a) -> (a, a, a) -> a -> Bool
isCloserThanMaxBoundsD3 (B (x1,y1,z1) (x2,y2,z2)) (x3,y3,z3) dist =
  let dx = maxdist x1 x2 x3; dy = maxdist y1 y2 y3; dz = maxdist z1 z2 z3 in
  dist > sqrt (dx*dx + dy*dy + dz*dz)

isWithinBSP :: Ord a => [a] -> BSPTree objT [a] -> Bool
isWithinBSP coords (BSP _ bounds _) = isWithinBounds' bounds coords

isWithinBSPD3 :: (Floating a, Ord a) => (a,a,a) -> BSPTree objT (a,a,a) -> Bool
isWithinBSPD3 coords (BSP _ bounds _) = isWithinBoundsD3 bounds coords

getmin, getmax :: (Floating a, Ord a) => a -> a -> a -> a
getmin x1 x2 x3 = let avg = (x2 + x1) / 2 in
  if x3 < avg then x1 else avg
getmax x1 x2 x3 = let avg = (x2 + x1) / 2 in
  if x3 <= avg then avg else x2

getmins, getmaxes :: (Floating a, Ord a) => [a] -> Bounds [a] -> [a]
getmins coords (B mins maxes) = zipWith3 getmin mins maxes coords
getmaxes coords (B mins maxes) = zipWith3 getmax mins maxes coords

getminsD3, getmaxesD3 :: (Floating a, Ord a) => (a,a,a) -> Bounds (a,a,a) -> (a,a,a)
getminsD3 (x3,y3,z3) (B (x1,y1,z1) (x2,y2,z2)) = (getmin x1 x2 x3, getmin y1 y2 y3, getmin z1 z2 z3)
getmaxesD3 (x3,y3,z3) (B (x1,y1,z1) (x2,y2,z2)) = (getmax x1 x2 x3, getmax y1 y2 y3, getmax z1 z2 z3)

subdivideBounds :: (Floating a, Ord a) => Bounds [a] -> [a] -> Bounds [a]
subdivideBounds bounds coords = B (getmins coords bounds) (getmaxes coords bounds)
subdivideBoundsD3 :: (Floating a, Ord a) => Bounds (a,a,a) -> (a,a,a) -> Bounds (a,a,a)
subdivideBoundsD3 bounds coords = B (getminsD3 coords bounds) (getmaxesD3 coords bounds)

newsubBSP :: (Floating a, Ord a, Com objT [a]) => Int -> Bounds [a] -> objT [a] -> BSPTree (objT [a]) [a]
newsubBSP nelems bounds obj = BSP nelems (subdivideBounds bounds $ getcom obj) $ Right [obj]

newsubBSPD3 :: (Floating a, Ord a, Com objT (a,a,a)) => Int -> Bounds (a,a,a) -> objT (a,a,a) -> BSPTree (objT (a,a,a)) (a,a,a)
newsubBSPD3 nelems bounds obj = BSP nelems (subdivideBoundsD3 bounds $ getcom obj) $ Right [obj]

insertBSP :: (Floating a, Ord a, Show a, Show (objT [a]), Com objT [a]) =>
     BSPTree (objT [a]) [a] -> objT [a] -> BSPTree (objT [a]) [a]
insertBSP tree obj =
  if isWithinBSP (getcom obj) tree
     then insertBSP' tree obj
     else  error "undefined" --need to increase size of box

insertSpacePart' :: (Show (objT repT), Show repT, Com objT repT) =>
     (repT -> BSPTree (objT repT) repT -> Bool)
     -> (BSPTree (objT repT) repT -> objT repT -> BSPTree (objT repT) repT)
     -> (Int -> Bounds repT -> objT repT -> BSPTree (objT repT) repT)
     -> (Int -> Bounds repT -> [BSPTree (objT repT) repT] -> objT repT -> [BSPTree (objT repT) repT])
     -> BSPTree (objT repT) repT -> objT repT -> BSPTree (objT repT) repT
insertSpacePart' withinfn insertfn newsubfn splitfn (BSP nelems bounds (Right elems)) obj =
  if length elems < nelems
    then BSP nelems bounds $ Right (obj:elems)
    else BSP nelems bounds $ Left (foldl' (splitfn nelems bounds) [newsubfn nelems bounds obj] elems)
insertSpacePart' withinfn insertfn newsubfn splitfn (BSP nelems bounds (Left trees)) obj =
  let (first,rst) = partition (withinfn $ getcom obj) trees in
    case first of
      [] -> BSP nelems bounds $ Left (newsubfn nelems bounds obj:rst)
      (x:xs) -> case xs of
        [] -> BSP nelems bounds $ Left (insertfn x obj:rst)
        _ -> error $ "xs in insertSpacePart' not []\n" ++ show obj ++ "\n" ++
             intercalate "\n" (map show (x:xs))

insertBSP' :: (Floating a, Ord a, Show (objT [a]), Show a, Com objT [a]) =>
     BSPTree (objT [a]) [a] -> objT [a] -> BSPTree (objT [a]) [a]
insertBSP' = insertSpacePart' isWithinBSP insertBSP' newsubBSP splitBSP

insertBSPD3' :: (Floating a, Ord a, Show (objT (a,a,a)), Show a, Com objT (a,a,a)) =>
     BSPTree (objT (a,a,a)) (a,a,a) -> objT (a,a,a) -> BSPTree (objT (a,a,a)) (a,a,a)
insertBSPD3' = insertSpacePart' isWithinBSPD3 insertBSPD3' newsubBSPD3 splitBSPD3

splitSpacePart :: (Show (objT repT), Show sp, Com objT repT) =>
     (repT -> sp -> Bool) -> (sp -> objT repT -> sp) -> (Int -> Bounds repT -> objT repT -> sp)
     -> Int -> Bounds repT -> [sp] -> objT repT -> [sp]
splitSpacePart withinfn insertfn newsubfn nelems bounds trees obj =
  let (first,rst) = partition (withinfn $ getcom obj) trees in
    case first of
      [] -> newsubfn nelems bounds obj:trees
      (x:xs) -> case xs of
        [] -> insertfn x obj:rst
        _ -> error $ "xs in splitSpacePart not []\n" ++ show obj ++ "\n" ++
             intercalate "\n" (map show (x:xs))

splitBSP :: (Floating a, Ord a, Show (objT [a]), Show a, Com objT [a]) =>
     Int -> Bounds [a] -> [BSPTree (objT [a]) [a]] -> objT [a] -> [BSPTree (objT [a]) [a]]
splitBSP = splitSpacePart isWithinBSP insertBSP' newsubBSP

splitBSPD3 :: (Floating a, Ord a, Show (objT (a,a,a)),Show a, Com objT (a,a,a)) =>
     Int -> Bounds (a,a,a) -> [BSPTree (objT (a,a,a)) (a,a,a)] -> objT (a,a,a) -> [BSPTree (objT (a,a,a)) (a,a,a)]
splitBSPD3 = splitSpacePart isWithinBSPD3 insertBSPD3' newsubBSPD3

getAllElemsBSP :: BSPTree objT repT -> [objT]
getAllElemsBSP (BSP _ _ (Left trees)) = concatMap getAllElemsBSP trees
getAllElemsBSP (BSP _ _ (Right elems)) = elems

getSubPartsBSP :: BSPTree objT repT -> [BSPTree objT repT]
getSubPartsBSP (BSP _ _ (Left trees)) = trees
getSubPartsBSP (BSP _ _ (Right elems)) = []

elemsWithinDistCoordWithBSP :: (Floating a, Ord a, Com objT [a]) =>
     (objT [a] -> [a]) -> [a] -> a -> BSPTree (objT [a]) [a] -> [objT [a]]
elemsWithinDistCoordWithBSP f coords dist (BSP _ _ (Right elems)) =
  filter ((< dist).euclidean coords.f) elems
elemsWithinDistCoordWithBSP f coords dist (BSP _ _ (Left trees)) =
  let trees' = filter (\(BSP _ b _) -> isCloserThanMinBounds b coords dist) trees in
  filter ((< dist).euclidean coords.f) $ concatMap (elemsWithinDistCoordWithBSP f coords dist) trees'

elemsWithinDistCoordWithBSPD3 :: (Floating a, Ord a, Com objT (a, a, a)) =>
     (objT (a,a,a) -> (a,a,a)) -> (a, a, a) -> a -> BSPTree (objT (a, a, a)) (a, a, a) -> [objT (a, a, a)]
elemsWithinDistCoordWithBSPD3 f coords dist (BSP _ _ (Right elems)) =
  filter ((< dist).euclideanD3 coords.f) elems
elemsWithinDistCoordWithBSPD3 f coords dist (BSP _ _ (Left trees)) =
  let trees' = filter (\(BSP _ b _) -> isCloserThanMinBoundsD3 b coords dist) trees in
  filter ((< dist).euclideanD3 coords.f) $ concatMap (elemsWithinDistCoordWithBSPD3 f coords dist) trees'

data Dat a = Dat a deriving (Show)

instance Com Dat [D] where
  com (Dat d) = (1,d)
  getcom (Dat d) = d
  distance (Dat d1) (Dat d2) = euclidean d1 d2

instance Com Dat D3 where
  com (Dat d) = (1,d)
  getcom (Dat d) = d
  distance (Dat d1) (Dat d2) = euclideanD3 d1 d2

lists :: [Dat [D]]
lists = [Dat [x,y,z] | x <- [(-5)..5],  y <- [(-5)..5], z <- [(-5)..5]]
tuples :: [Dat D3]
tuples = [Dat (x,y,z) | x <- [(-5)..5],  y <- [(-5)..5], z <- [(-5)..5]]

neighborsBrute :: (Floating a, Ord a, Com objT [a]) => [objT [a]] -> [a] -> a -> [objT [a]]
neighborsBrute objs point dist =
  filter ((< dist).euclidean point.getcom) objs
neighborsBruteD3 :: (Floating a, Ord a, Com objT (a,a,a)) => [objT (a,a,a)] -> (a,a,a) -> a -> [objT (a,a,a)]
neighborsBruteD3 objs point dist =
  filter ((< dist).euclideanD3 point.getcom) objs

nb1 = length.concat $ map (\c -> neighborsBrute lists (getcom c) 3.0) lists
nb2 = length.concat $ map (\c -> neighborsBruteD3 tuples (getcom c) 3.0) tuples

b1 :: Bounds [D]
b1 = B (replicate 3 (-10.1)) (replicate 3 10.1)
bsp1 :: BSPTree (Dat [D]) [D]
bsp1 = insertAll lists
nt1 = length.concat $ map (\c -> elemsWithinDistCoord (getcom c) 3.0 bsp1) lists

b2 :: Bounds D3
b2 = B (-10.1,-10.1,-10.1) (10.1,10.1,10.1)
bsp2 :: BSPTree (Dat D3) D3
bsp2 = insertAll tuples
nt2 = length.concat $ map (\c -> elemsWithinDistCoord (getcom c) 3.0 bsp2) tuples
