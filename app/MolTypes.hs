{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MolTypes where
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Geometry
import MainTypes
--import Data.Text hiding (filter, foldl', intercalate, map, zipWith)

data AtomSpec = AS {species :: String} deriving (Eq,Ord,Show)--charge
data PairSpec = PS {dmin :: Maybe D, dmax :: D,
                    omin :: Maybe [D], omax :: Maybe [D],
                    acc1 :: Maybe [Int], acc2 :: Maybe [Int]} deriving (Eq,Ord)--charges
makePS = PS {dmin = Nothing, dmax = 0.0, omin = Nothing, omax = Nothing, acc1 = Nothing, acc2 = Nothing}
--distance, angle, orientation
data TripleSpec = TS {dmin12 :: Maybe D, dmax12 :: Maybe D,
                      dmin23 :: Maybe D, dmax23 :: Maybe D,
                      amin :: Maybe D, amax :: Maybe D,
                      omin12 :: Maybe [D], omax12 :: Maybe [D],
                      omin23 :: Maybe [D], omax23 :: Maybe [D]} deriving (Eq,Ord,Show)
data MolSpec = AtomS AtomSpec -- | PairS PairSpec | TripleS TripleSpec
             | MolSpecGraph MolSpecMap [ConnSpec] deriving (Eq,Ord,Show)
type MolSpecMap = Map.Map Int MolSpec
data ConnSpec = Cs2 Int Int PairSpec
              | Cs3 (Int,Int,Int) TripleSpec deriving (Eq,Ord,Show)

data Mol repT = Atm (Atom repT) | MolGraph (MolMap repT) [Conn] deriving (Eq,Ord,Show)
type MolMap repT = Map.Map Int (Mol repT)
data Conn = C2 Int Int | C3 (Int,Int,Int) deriving (Eq,Ord,Show)

type MolDB repT = Map.Map MolSpec (Set.Set (Mol repT))

data Atom repT = A String repT D deriving (Ord,Show)
type XYZfile repT = (Integer, String, [Atom repT])

instance (Eq repT) => Eq (Atom repT) where
  (A s1 c1 m1) == (A s2 c2 m2) = s1 == s2 && c1 == c2 -- && m1 == m2

hideN :: (Eq a, Show a) => String -> Maybe a -> String
hideN n x = if isNothing x then "" else n ++ " = " ++ show x
fn1 x n f = hideN n $ f x
fn2 x n f = hideN n $ f x
instance Show PairSpec where
  show x = let f1 = zipWith (fn1 x) ["dmin"] [dmin]--["dmin", "dmax"] [dmin, dmax]
               f2 = zipWith (fn2 x) ["omin", "omax"] [omin, omax]
           in "PS {" ++ (intercalate ", " $ filter (/= "") $ f1 ++ f2) ++ "}"

class Com objT repT where
  com :: objT repT -> (D, repT)
  getcom :: objT repT -> repT
  distance :: objT repT -> objT repT -> D

instance Com Mol D3 where
  com (Atm (A _ (x,y,z) m)) = (m, (x*m,y*m,z*m))
  com (MolGraph molmap _) = let fn (masses,mcoords) mol = let (mass,coords) = com mol in
                                  (masses + mass,sumD3 mcoords coords)
                            in foldl' fn (0, (0,0,0)) $ Map.elems molmap
  getcom (Atm (A _ coords _)) = coords
  getcom mol = let (m, (x,y,z)) = com mol in
    (x/m,y/m,z/m)
  distance obj1 obj2 = euclideanD3 (getcom obj1) (getcom obj2)

instance Com Mol [D] where
  com (Atm (A _ coords m)) = (m,map (*m) coords)
  com (MolGraph molmap _) = let fn (masses,mcoords) mol = let (mass,coords) = com mol in
                                  (masses + mass,zipWith (+) mcoords coords)
                            in foldl' fn (0,[0,0,0]) $ Map.elems molmap
  getcom (Atm (A _ coords _)) = coords
  getcom mol = let (m,coords) = com mol in map (/m) coords
  distance obj1 obj2 = euclidean (getcom obj1) (getcom obj2)