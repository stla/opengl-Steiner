module Steiner where

import Chains
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.IORef
import Graphics.Rendering.OpenGL.Capture (capturePPM)
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import System.Directory (doesDirectoryExist)
import System.IO.Unsafe
import Text.Printf

n :: [Int]
n = [3, 4, 5]

white, black, red :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
red = Color4 1 0 0 1

data Context = Context
  { contextRot1 :: IORef GLfloat
  , contextRot2 :: IORef GLfloat
  , contextRot3 :: IORef GLfloat
  , contextSpheres :: IORef [((Double, Double, Double), Double)]
  }

display :: Context -> IORef Double -> DisplayCallback
display context zoom = do
  clear [ColorBuffer, DepthBuffer]
  spheres <- get (contextSpheres context)
  zoom' <- get zoom
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  loadIdentity
  (_, size) <- get viewport
  resize zoom' size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_
    (\(center, radius) ->
       preservingMatrix $ do
         translate (toVector3 center)
         materialDiffuse Front $= red
         renderObject Solid $ Sphere' radius 60 60)
    spheres
  swapBuffers
  where
    toVector3 (x, y, z) = Vector3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w' / h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard ::
     IORef GLfloat
  -> IORef GLfloat
  -> IORef GLfloat -- rotations
  -> IORef GLdouble -- zoom
  -> IORef Double -- phi
  -> IORef Double -- shift
  -> IORef Bool -- animation
  -> IORef Bool -- save
  -> IORef Int -- delay
  -> IORef [((Double, Double, Double), Double)]
  -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom phi shift anim save delay spheres c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.25)
    'l' -> zoom $~! subtract 0.25
    'g' -> do
      phi' <- get phi
      shift $~! (+ 0.005)
      shift' <- get shift
      writeIORef spheres (steiner n phi' shift' True)
    'b' -> do
      phi' <- get phi
      shift $~! subtract 0.005
      shift' <- get shift
      writeIORef spheres (steiner n phi' shift' True)
    'd' -> do
      phi $~!
        (\x ->
           if x < 0.9
             then x + 0.1
             else x)
      phi' <- get phi
      shift' <- get shift
      writeIORef spheres (steiner n phi' shift' True)
    'c' -> do
      phi $~!
        (\x ->
           if x > -0.9
             then x - 0.1
             else x)
      phi' <- get phi
      shift' <- get shift
      writeIORef spheres (steiner n phi' shift' True)
    'q' -> leaveMainLoop
    'a' -> anim $~! not
    's' -> save $~! not
    'o' -> delay $~! (+5000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-5000)
    _ -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle ::
     IORef Bool
  -> IORef Bool
  -> IORef Int
  -> IORef Int
  -> IORef Double
  -> IORef Double
  -> IORef [((Double, Double, Double), Double)]
  -> IdleCallback
idle anim save delay snapshot phi shift spheres = do
  a <- get anim
  ss <- get snapshot
  s <- get save 
  when a $ do
    d <- get delay
    when (s && ppmExists && ss < 200) $ do
      let ppm = printf "ppm/pic%04d.ppm" ss
      (>>=) capturePPM (B.writeFile ppm)
      print ss
      snapshot $~! (+ 1)
    shift $~! (+ 0.005)
    shift' <- get shift
    phi' <- get phi
    writeIORef spheres (steiner n phi' shift' True)
    _ <- threadDelay d
    postRedisplay Nothing
  return ()


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hierarchical Steiner chain"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-500) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  let phi = 0.4
      shift = 0
      spheres = steiner n phi shift True
  n' <- newIORef n
  phi' <- newIORef phi
  shift' <- newIORef shift
  spheres' <- newIORef spheres
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 5000
  displayCallback $=
    display
      Context
      { contextRot1 = rot1
      , contextRot2 = rot2
      , contextRot3 = rot3
      , contextSpheres = spheres'
      }
      zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= 
    Just (keyboard rot1 rot2 rot3 zoom phi' shift' anim save delay spheres')
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot phi' shift' spheres')
  putStrLn
    "*** 3D Steiner chain ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease phi: d,c\n\
        \    Rotate chains: g, b\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
