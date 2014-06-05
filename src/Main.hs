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

data State = State { x, y :: IORef GLdouble }

makeState :: IO State
makeState = do
   x <- newIORef 0.05
   y <- newIORef 0.05
   return $ State { x = x, y = y }

myInit :: IO ()
myInit = do
   clearColor $= Color4 1 1 1 1
   shadeModel $= Flat

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

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
       scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
   preservingMatrix $ do
      translatef (Vector3 (-1) 0 0)
      x <- get (x state)
      y <- get (y state)
      circle (0.5, 0.5)
      rectangle 0.5
      translatef (Vector3 1 0 0)
      preservingMatrix $ do
         translatef (Vector3 1 0 0)
         circle (x, y)
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

keyboard :: State -> KeyboardMouseCallback
keyboard state key Down _ _ = case key of
   SpecialKey KeyRight -> update x 0.1
   SpecialKey KeyLeft -> update x (-0.1)
   SpecialKey KeyUp -> update y 0.1
   SpecialKey KeyDown -> update y (-0.1)
   Char '\27' -> exitWith ExitSuccess
   _     -> return ()
   where update joint inc = do
            joint state $~ (+ inc)
            postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   _ <- createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   mainLoop
