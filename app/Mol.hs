{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Mol where
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Geometry
import MolTypes
import MainTypes
import BSP
--import Data.Text hiding (foldl, head, length, map, tail, zipWith)

makeMolSpecChain :: [String] -> [D] -> Bool -> MolSpec
makeMolSpecChain names dmaxs ring = 
  let conns = zipWith3 (\i1 i2 d -> Cs2 i1 i2 (makePS {dmax = d}))
              (iterate (+1) 1) (iterate (+1) 2) (if ring then init dmaxs else dmaxs)
      molspecmap = zipWith (\i n -> (i,AtomS $ AS n)) (iterate (+1) 1) names
      conns' = if ring then Cs2 1 (length names) (makePS {dmax = dmaxs !! (length names - 1)}):conns else conns
  in MolSpecGraph (Map.fromList molspecmap) conns'

makeMolSpecBranch :: [String] -> [D] -> MolSpec
makeMolSpecBranch names dmaxs =
  let molspecmap = zipWith (\i n ->(i,AtomS $ AS n)) (iterate (+1) 1) names
      conns = zipWith (\i d -> Cs2 1 i (makePS {dmax = d})) (iterate (+1) 2) dmaxs
  in MolSpecGraph (Map.fromList molspecmap) conns

insertMoltoMolDB :: (Ord repT) => MolDB repT -> MolSpec -> Mol repT -> MolDB repT
insertMoltoMolDB db mols mol =
  Map.insert mols (Set.insert mol set) db where
  set = fromMaybe Set.empty (Map.lookup mols db)

insertAtomtoMolDB :: (Ord repT) => MolDB repT -> Atom repT -> MolDB repT
insertAtomtoMolDB db atm = insertMoltoMolDB db (AtomS $ AS s) (Atm atm) where (A s _ _) = atm
createMolDB :: (Ord repT) => [Atom repT] -> MolDB repT
createMolDB = foldl insertAtomtoMolDB Map.empty

accessMol' :: (Show repT) => Mol repT -> Int -> Mol repT
accessMol' (MolGraph molmap _) i = unsafelookup i molmap
accessMol' (Atm atm) i = error $ "Cannot index into Atm " ++ show i ++ " " ++ show atm
accessMol :: (Show repT) => [Int] -> Mol repT -> Mol repT
accessMol (i:is) (MolGraph molmap _) = foldl' accessMol' (unsafelookup i molmap) is
accessMol is (Atm atm) = Atm atm --check if more is or ignore?

unsafelookup :: (Ord a, Show a, Show b) => a -> Map.Map a b -> b
unsafelookup k m = fromMaybe (error $ "Key not found in unsafelookup " ++ show k ++ " map " ++ show m)  (Map.lookup k m)

unsafelookupmspec :: Int -> MolSpecMap -> MolSpec
unsafelookupmspec = unsafelookup
unsafelookupdb :: (Show repT) => MolSpec -> MolDB repT -> Set.Set (Mol repT)
unsafelookupdb = unsafelookup

findpairs :: (Show repT, Com Mol repT) => [Mol repT] -> [Mol repT] -> PairSpec -> [(Mol repT,Mol repT)]
findpairs m1 m2 ps = let getx = case acc1 ps of Nothing -> id; Just is -> accessMol is
                         gety = case acc2 ps of Nothing -> id; Just is -> accessMol is
                     in [(x,y) | x <- m1, y <- m2, distance (getx x) (gety y) <= dmax ps]

findpairsBSPD3 :: [Mol D3] -> [Mol D3] -> PairSpec -> [(Mol D3,Mol D3)]
findpairsBSPD3 m1 m2 ps =
  let getx = case acc1 ps of Nothing -> getcom; Just is -> getcom.accessMol is
      gety = case acc2 ps of Nothing -> getcom; Just is -> getcom.accessMol is
      bspy :: BSPTree (Mol D3) D3
      bspy = insertAll m2
      fn x = map (\y-> (x,y)) $ elemsWithinDistCoordWith gety (getx x) (dmax ps) bspy
  in concatMap fn m1

instance SpacePart BSPTree Mol [Double] where
  isWithinBounds = isWithinBSP
  insertOne = insertBSP
  insertAll = foldl' insertBSP' (BSP 10 defaultbounds (Right []))
  getAllElems = getAllElemsBSP
  getSubParts = getSubPartsBSP
  elemsWithinDistCoordWith = elemsWithinDistCoordWithBSP

instance SpacePart BSPTree Mol D3 where
  isWithinBounds = isWithinBSPD3
  insertOne = insertBSPD3'
  insertAll = foldl' insertBSPD3' (BSP 10 defaultboundsD3 (Right []))
  getAllElems = getAllElemsBSP
  getSubParts = getSubPartsBSP
  elemsWithinDistCoordWith = elemsWithinDistCoordWithBSPD3

--getpairs :: (Show repT, Ord repT, Com Mol repT) => MolDB repT -> MolSpecMap -> ConnSpec -> [Mol repT]
getPairs db mspecm (Cs3 (i1,i2,i3) ts, mgraph1) mgraph2 = undefined
getpairs db mspecm (Cs2 i1 i2 ps) =
  let mspec1 = unsafelookupmspec i1 mspecm
      mspec2 = unsafelookupmspec i2 mspecm
      db1 = matchMols db mspec1
      db2 = matchMols db mspec2
      matches1 = Set.elems $ unsafelookupdb mspec1 db1
      matches2 = Set.elems $ unsafelookupdb mspec2 db2
      pairs = findpairsBSPD3 matches1 matches2 ps
      in map (\(m1,m2) -> MolGraph (Map.fromList [(i1,m1),(i2,m2)]) [C2 i1 i2]) pairs

--getMols :: (Show repT, Ord repT, Com Mol repT) => MolDB repT -> MolSpecMap -> ConnSpec -> (ConnSpec,[Mol repT])
getMols db mspecm cspec =
  case cspec of
    Cs2 i1 i2 ps -> (Cs2 i1 i2 ps, getpairs db mspecm cspec)
    Cs3 (i1,i2,i3) ts -> undefined

--combineMols :: (Show repT, Ord repT) => (ConnSpec,[Mol repT]) -> [Mol repT] -> (ConnSpec,[Mol repT])
combineMols (Cs3 (i1,i2,i3) ts, mgraph1) mgraph2 = undefined
combineMols (Cs2 i1 i2 ps, mgraph1) mgraph2 =
  let eqx = case acc1 ps of Nothing -> id; Just is -> accessMol is
      eqy = case acc2 ps of Nothing -> id; Just is -> accessMol is
      (MolGraph mmx _) = head mgraph1 --assume each Mol in [Mol] is of the same MolSpec
      (MolGraph mmy _) = head mgraph2 --otherwise, we cannot hoist this loop invariant
      ints1 = Set.fromList $ Map.keys mmx --mm1
      ints2 = Set.fromList $ Map.keys mmy --mm2
      equalindices = Set.intersection ints2 ints1
      indices = Set.elems equalindices
      diff1 = Set.elems $ Set.difference ints1 equalindices
      diff2 = Set.elems $ Set.difference ints2 equalindices
      size = length diff1 + length diff2
  in
  (Cs2 i1 i2 ps, [MolGraph (Map.union mm1 mm2) (mp1 ++ mp2)
  | MolGraph mm1 mp1 <- mgraph1, MolGraph mm2 mp2 <- mgraph2, --also need to pattern match on Atm
    let t1 = all (\int -> eqx (unsafelookup int mm1) == eqy (unsafelookup int mm2)) indices
        t2 = size == (Set.size $ Set.fromList $
                      map (\i -> eqx $ unsafelookup i mm1) diff1 ++
                      map (\i -> eqy $ unsafelookup i mm2) diff2)
    in t1 && t2])

--matchMols :: (Show repT, Ord repT, Com Mol repT) => MolDB repT -> MolSpec -> MolDB repT
matchMols db spec =
  case Map.lookup spec db of
       Just s -> Map.insert spec s Map.empty --return entire db?
       Nothing -> case spec of
         AtomS aspec -> error "AtomS1"
         MolSpecGraph _ [] -> error "MolSpecGraph1"
         MolSpecGraph mspecm cspecs ->
           let graphs = map (getMols db mspecm) cspecs
               graph = case length graphs of
                 1 -> snd $ head graphs
                 _ -> snd $ foldl combineMols (head cspecs,snd $ head graphs) (tail $ map snd graphs)
           in case graph of
             [] -> db
             _ -> Map.insert spec (Set.fromList graph) db