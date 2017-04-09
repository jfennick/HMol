module Main (main) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Control.Applicative hiding ((<|>), optional, many)

import Graphics.UI.GLUT
import Data.IORef

--import Data.List.HT
import Data.List

--import BSP
import Geometry
import MolTypes
import MainTypes

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

ints :: (Integral a, Read a) => Parser [a]
ints = many (lexeme int)

ws :: Parser String
ws = many space --(oneOf " ")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

float :: GenParser Char st Double
float = do sign <- option 1 (do
                             s <- oneOf "+-"
                             return $ if s == '-' then -1.0 else 1.0)
           x <- P.float $ P.makeTokenParser emptyDef
           return $ sign * x

line :: Parser (Atom D3)
line = do
  s <- many1 upper
  fs <- many1 (lexeme float)
  return (A s (fs!!0,fs!!1,fs!!2) (case s of
             "H" -> 1.0
             "C" -> 12.0  
             "N" -> 14.0
             "O" -> 16.0
             "P" -> 30.0))

xyz :: Parser (XYZfile D3)
xyz = do
  i1 <- lexeme int
  oneOf "\r\n"
  s <- many $ noneOf "\r\n"
  oneOf "\r\n"
  xs <- many1 line
  return (i1,s,xs)
--end parsing

neighbors :: D3 -> D -> XYZfile D3 -> [Atom D3]
neighbors point maxdist (i1, str, atoms) =
  filter (\(A s coords m) -> euclideanD3 point coords <= maxdist) atoms

dna :: IO (Integer, String, [Atom D3])
dna = do fc <- readFile "/home/jfennick/HMol/xyz/dna.xyz"
         return $ case parse xyz "" fc of
           Left pe -> (0, show pe, [])
           Right x -> x

{-main :: IO()
main =
  do
        fc <- readFile "/home/jfennick/HMol/xyz/dna.xyz"
--        print $ lines fc
        let atoms = case parse xyz "" fc of
                         Left pe -> (0, show pe, [])
                         Right x -> x
--        print atoms
        print $ neighbors (1.0,1.0,1.0) 100.0 atoms-}
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  (_,_,atoms) <- dna
  scale (0.02::GLfloat) (0.02::GLfloat) (0.02::GLfloat)
  pos <- newIORef (0, 0)
  dpos <- newIORef (0, 0)
  --keyboardMouseCallback $= Just (keyboardMouse delta pos)
  motionCallback $= Just (motion pos dpos)
  mouseCallback $= Just (mouse pos)
  displayCallback $= display dpos atoms
  reshapeCallback $= Just reshape
  mainLoop

motion :: IORef (GLint, GLint) -> IORef (GLint, GLint) -> MotionCallback
motion prevpos dpos (Position x2 y2) =
  do (x1, y1) <- readIORef prevpos
     dpos $~! \(_,_) -> (x2-x1,y2-y1)
     prevpos $~! \(_,_) -> (x2,y2)
     postRedisplay Nothing

mouse :: IORef (GLint, GLint) -> MouseCallback
mouse prevpos LeftButton Down (Position x y) = prevpos $~! \(_,_) -> (x,y)
mouse _ _ _ _ = return ()

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

display :: IORef (GLint, GLint) -> [Atom D3] -> DisplayCallback
display dpos atoms = do
  (dphi, dtheta) <- readIORef dpos
  clear [ColorBuffer]
--  renderPrimitive Points $
--     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  mapM_ (\(A s (x, y, z) m) -> preservingMatrix $
                         do translate $ Vector3 (doubleToGLfloat x) (doubleToGLfloat y) (doubleToGLfloat z)
                            color $ Color3 (doubleToGLfloat x) (doubleToGLfloat y) (doubleToGLfloat z)
                            renderObject Solid (Sphere' 0.05 25 25)) atoms
  let dt = (intToInt dtheta) :: Double
      dp = (intToInt dphi) :: Double
      dx = (sin dt)*(cos dp)
      dy = (sin dt)*(sin dp)
      dz = (cos dt)
      (x',y',z') = crossD3 (0,0,1) (dx,dy,dz)
      angle = (*) 10 $ angleD3 (0,0,1) (dx,dy,dz) in
    --rotate (doubleToGLfloat angle) $ Vector3 (doubleToGLfloat x') (doubleToGLfloat y') (doubleToGLfloat z')
    do rotate (doubleToGLfloat dp) $ Vector3 1 0 0
       rotate (doubleToGLfloat dt) $ Vector3 0 1 0
  flush

intToInt :: (Integral a, Num b) => a -> b
intToInt = fromInteger.toInteger

realToReal :: (Real a, Fractional b) => a -> b
realToReal = fromRational . toRational

doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat = realToReal

glfloatToDouble :: GLfloat -> Double
glfloatToDouble = realToReal
