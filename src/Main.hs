{-
   Robot.hs (adapted from robot.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <svenpanne@gmail.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

  This program shows how to composite modeling transformations to draw
  translated and rotated hierarchical models. Interaction: pressing the s
  and e keys (shoulder and elbow) alters the rotation of the robot arm.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding (position, scale)
import Unsafe.Coerce
import Physics.Hipmunk hiding (Position)
import Control.Concurrent.MVar

data GameState = GameState { ledgePos :: IORef CpFloat, mainBall :: IORef Shape }

makeState :: Space -> IO GameState
makeState space = do mBall <- ball space (0.05, 0.05) 0.3
                     mainBall <- newIORef mBall
                     ledgePos <- newIORef 0.5
                     let gamestate = GameState { ledgePos = ledgePos, mainBall = mainBall }
                     return gamestate

myInit :: IO ()
myInit = do
   clearColor $= Color4 1 1 1 1
   shadeModel $= Flat

simpleInit :: Space -> IO ()
simpleInit space = do
    let title = "awesome game"
    _ <- getArgsAndInitialize
    gameModeCapabilities $= [ Where' GameModeBitsPerPlane IsEqualTo 24 ]
    initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
    
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 100 100

    _ <- createWindow title
    actionOnWindowClose $= MainLoopReturns

    clearColor $= Color4 1 1 1 1
    shadeModel $= Flat
    let scheduleTick = do
            let fps = 60
            addTimerCallback (1000 `div` fps) tick

        tick = do
            postRedisplay Nothing
            step space subStepQuantum
            scheduleTick

    scheduleTick

toVertex :: Num a => (a, a) -> IO ()
toVertex (x,y) = vertex $ Vertex2 (unsafeCoerce x :: GLdouble) (unsafeCoerce y)

-- circle around position
circle :: (GLdouble, GLdouble) -> IO ()
circle (x,y) = preservingMatrix $ do
    let poly  = 24
        ang p = p * 2 * pi / poly
        pos   = map (\p -> (x+cos(ang p)*r, y + sin(ang p)*r)) [1,2..poly]
        r = 0.5
    color $ Color3 1 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.Polygon $
        mapM_ (toVertex) pos
    color $ Color3 0 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.LineLoop $
        mapM_ (toVertex) pos

-- position of rectangle: it's centre top
rectangle :: GLdouble -> IO ()
rectangle x1 = do
            color $ Color3 0 0 (0 :: GLdouble)
            let y1 = -0.1  -- constant y
                pos   = map (\(w, h) -> (x1 + w, y1 + h)) [(-0.5,0), (0.5,0), (0.5, -0.2), (-0.5, -0.2)]
            renderPrimitive Graphics.UI.GLUT.Polygon $
                mapM_ (toVertex) pos


ball :: Space -> (CpFloat, CpFloat) -> Double -> IO Shape
ball space pos rad = do
    -- Body.
    let m = 100 -- just setting a mass
        vel = (0.0, 0.0)
    b <- newBody m infinity
    position b $= uncurry Vector pos
    velocity b $= uncurry Vector vel
    spaceAdd space b

    -- Shape.
    bshape <- newShape b (Circle $ unsafeCoerce rad) (Vector 0 0)
    elasticity bshape    $= 0.9
    friction bshape      $= 0.1
    spaceAdd space bshape
    return bshape

-- newShape body_@(B b) (Polygon verts) off
{-
rectangleBody :: Space -> ((Double, Double), (Double, Double)) -> IO ()
rectangleBody space ((x1,y1), (x2,y2)) = do
    ground <- newBody infinity infinity
    gshape <- newShape ground (LineSegment (Vector x1 y1) (Vector x2 y2) 0.01)
               (Vector 0.0 0.0)
    position ground   $= Vector 0.0 0.00
    elasticity gshape $= 0.5
    friction gshape   $= 0.8
    spaceAdd space (Static gshape)

border :: Space -> ((Double, Double), (Double, Double)) -> IO ()
border space ((x1,y1), (x2,y2)) = do
    ground <- newBody infinity infinity
    gshape <- newShape ground (LineSegment (Vector x1 y1) (Vector x2 y2) 0.01)
               (Vector 0.0 0.0)
    position ground   $= Vector 0.0 0.00
    elasticity gshape $= 0.5
    friction gshape   $= 0.8
    spaceAdd space (Static gshape)
-}

display :: GameState -> DisplayCallback
display gamestate = do
   clear [ ColorBuffer ]
   ledgePos <- get (ledgePos gamestate)
   mainBall <- get (mainBall gamestate)
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   preservingMatrix $ do
      translatef (Vector3 (-1) 0 0)
      preservingMatrix $ do
         translatef (Vector3 1 0 0)
         Vector x y <- get $ position (body mainBall)
         putStrLn $ "position varied? " ++ (show x) ++ " " ++ (show y)
         circle (unsafeCoerce x, unsafeCoerce y)
         rectangle (unsafeCoerce ledgePos)
      translatef (Vector3 1 0 0)
   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 65 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   translatef (Vector3 0 0 (-5))

keyboard :: GameState -> KeyboardMouseCallback
keyboard gamestate key Down _ _ =
   case key of
    SpecialKey KeyRight -> update ledgePos 0.1
    SpecialKey KeyLeft -> update ledgePos (-0.1)
    --SpecialKey KeyUp -> update y 0.1
    --SpecialKey KeyDown -> update y (-0.1)
    Char '\27' -> exitWith ExitSuccess
    _     -> return ()
   where update pos inc = do
            pos gamestate $~ (+ inc)
            postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   state <- makeState
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   myInit
   -- display 
   simpleInit space
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   mainLoop
