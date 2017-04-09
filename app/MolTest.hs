module MolTest where
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Dna
import Mol
import MolTypes
import MainTypes
--import Data.Text hiding (map, replicate)
pack = id

third :: (a,b,c) -> c
third (_,_,x) = x

dnaa :: [Atom D3]
dnaa = map (\(A s c d) -> A (pack s) c d) $ third dna
dnaadb :: MolDB D3
dnaadb = createMolDB dnaa

molsc3n2, molsc4n2, molsnh2, molsch3, molsadenine :: MolSpec
molsc3n2 = makeMolSpecChain (map pack ["C","N","C","N","C"]) (replicate 5 1.5) True
molsc4n2 = makeMolSpecChain (map pack ["C","C","C","N","C","N"]) (replicate 6 1.5) True
molsnh2 = makeMolSpecBranch (map pack ["N","H","H"]) (replicate 2 1.2)
molsch3 = makeMolSpecBranch (map pack ["C","H","H","H"]) (replicate 3 1.2)
molsadenine = MolSpecGraph (Map.fromList [(1,molsc4n2),(2,molsc3n2)])--,(3,molsnh2)])
              [Cs2 1 2 (makePS {dmax = 0.1, acc1 = Just [1], acc2 = Just [1]}),
               Cs2 1 2 (makePS {dmax = 0.1, acc1 = Just [2], acc2 = Just [5]})]
               --Cs2 1 3 (makePS {dmax = 0.1, acc1 = Just [3], acc2 = Just [1]})]

moldbc4n2, moldbc3n2, moldbadenine :: MolDB D3
moldbc4n2 = Map.difference (matchMols dnaadb molsc4n2) dnaadb
moldbc3n2 = Map.difference (matchMols dnaadb molsc3n2) dnaadb
moldbadenine = Map.difference (matchMols dnaadb molsadenine) dnaadb

--adeninematch :: Mol D3
--adeninematch = head.Set.elems $ (unsafelookup molsadenine moldbadenine)