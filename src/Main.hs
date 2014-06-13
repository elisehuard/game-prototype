{-# language ScopedTypeVariables #-}
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
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import Graphics.UI.GLUT hiding (position, scale)
import Unsafe.Coerce
import Physics.Hipmunk hiding (Position)
import Control.Concurrent.MVar
import Control.Concurrent (forkOS)
import Sound.ALUT hiding (Static)
import Data.List (intersperse)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import System.IO ( hPutStrLn, stderr )

data GameState = GameState { ledgePos :: IORef CpFloat, ledge :: IORef Shape, mainBall :: IORef Shape }

-- constants

type Seconds = CpFloat
subStepQuantum :: Seconds = 0.01

ballRadius :: CpFloat = 0.3
ledgeHeight :: CpFloat = - 0.5

-- constants end

makeState :: Space -> IO GameState
makeState space = do mBall <- ball space (0.0, 0.0) ballRadius 
                     mainBall <- newIORef mBall
                     ledgeShape <- rectangleShape space 0.0
                     ledgePos <- newIORef 0.0
                     ledge <- newIORef ledgeShape
                     let gamestate = GameState { ledgePos = ledgePos, ledge = ledge, mainBall = mainBall }
                     return gamestate

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
        r = unsafeCoerce ballRadius
        pos   = map (\p -> (x+cos(ang p)*r, y + sin(ang p)*r)) [1,2..poly]
    color $ Color3 1 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.Polygon $
        mapM_ (toVertex) pos
    color $ Color3 0 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.LineLoop $
        mapM_ (toVertex) pos

rectDims :: [(CpFloat, CpFloat)]
rectDims = [(-0.5,0.1), (0.5,0.1), (0.5, -0.1), (-0.5, -0.1)]

-- position of rectangle: it's centre top
rectangle :: CpFloat -> IO ()
rectangle x1 = do
            color $ Color3 0 0 (0 :: GLdouble)
            let y1 = ledgeHeight -- constant y
                vert = map (\(w, h) -> (x1 + w, ledgeHeight + h)) rectDims
            renderPrimitive Graphics.UI.GLUT.Polygon $
                mapM_ (toVertex) vert

line :: ((Double, Double), (Double, Double)) -> IO ()
line ((x1, y1), (x2, y2)) = do
            color $ Color3 0 0 (0 :: GLdouble)
            renderPrimitive Graphics.UI.GLUT.Lines $
                mapM_ (toVertex) [(x1, y1), (x2, y2)]


-- ball/circle radius should be defined in one place
ball :: Space -> (CpFloat, CpFloat) -> Double -> IO Shape
ball space pos rad = do
    -- Body.
    let m = 1000 -- just setting a mass
        vel = (0.0, -0.2)
    b <- newBody m infinity
    position b $= uncurry Vector pos
    velocity b $= uncurry Vector vel
    spaceAdd space b

    -- Shape.
    bshape <- newShape b (Circle $ unsafeCoerce rad) (Vector 0 0)
    elasticity bshape    $= 2.0
    friction bshape      $= 0.1
    spaceAdd space bshape
    return bshape

-- newShape body_@(B b) (Polygon verts) offset
rectangleShape :: Space -> CpFloat -> IO Shape
rectangleShape space x1 = do
    -- ground <- newBody infinity infinity -- 'rogue body'
    let vertices = map (\(x, y) -> Vector x y) rectDims
    b <- newBody infinity $ momentForPoly infinity vertices 0
    gshape <- newShape b (Physics.Hipmunk.Polygon vertices) (Vector 0.0 0.0)
    position b   $= Vector x1 ledgeHeight
    elasticity gshape $= 0.5
    friction gshape   $= 0.8
    --spaceAdd space (Static gshape) -- rogue so not adding to space
    spaceAdd space gshape -- rogue so not adding to space
    return gshape

updateRectangleBody :: GameState -> CpFloat -> IO ()
updateRectangleBody state inc = do pong <- get (ledge state)
                                   oldx <- get (ledgePos state)
                                   let newpos = (Vector (oldx + inc) ledgeHeight)
                                   position (body pong) $= newpos

border :: Space -> ((Double, Double), (Double, Double)) -> IO ()
border space ((x1,y1), (x2,y2)) = do
    ground <- newBody infinity infinity
    gshape <- newShape ground (LineSegment (Vector x1 y1) (Vector x2 y2) 0.01)
               (Vector 0.0 0.0)
    position ground   $= Vector 0.0 0.00
    elasticity gshape $= 0.5
    friction gshape   $= 0.8
    spaceAdd space (Static gshape)

display :: GameState -> DisplayCallback
display gamestate = do
   clear [ ColorBuffer ]
   ledgePos <- get (ledgePos gamestate)
   mainBall <- get (mainBall gamestate)
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   preservingMatrix $ do
        Vector x y <- get $ position (body mainBall)
        --putStrLn $ "position varied? " ++ (show x) ++ " " ++ (show y)
        circle (unsafeCoerce x, unsafeCoerce y)
        rectangle (unsafeCoerce ledgePos)
        line ((-3.0, -3.0), (3.0, -3.0))
         
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

keyboardMouse :: GameState -> Space -> KeyboardMouseCallback
keyboardMouse gamestate space key Down _ _ =
   case key of
    SpecialKey KeyRight -> update ledgePos 0.1
    SpecialKey KeyLeft -> update ledgePos (-0.1)
    --SpecialKey KeyUp -> update y 0.1
    --SpecialKey KeyDown -> update y (-0.1)
    Char '\27' -> exitWith ExitSuccess
    _     -> return ()
   where update pos inc = do
            pos gamestate $~ (+ inc)
            updateRectangleBody gamestate inc
            postRedisplay Nothing
keyboardMouse _ _ _ _ _ _ = return ()
stopAll = do
   exitWith ExitSuccess

-- sound
-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html#sec_conc-logger
data SoundService = SoundService (MVar SoundCommand)  -- handle to sound service
data SoundCommand = Play Sound | Stop (MVar ())
data Sound = Pow

initSoundService :: IO SoundService
initSoundService = do
           m <- newEmptyMVar
           let s = SoundService m
           forkOS (soundService s) -- forkIO might not be enough, might need forkOS
           return s

soundService :: SoundService -> IO ()
soundService (SoundService s) = do
  withProgNameAndArgs runALUT $ \progName args -> do
     -- Create an AL buffer from the given sound file.
     pow <- createBuffer (File "sounds/jump.wav")
     loop pow
    where
        loop b = do
                cmd <- takeMVar s
                case cmd of
                    Play Pow -> do
                        playSound b
                        loop b
                    Stop x -> do
                        putStrLn "sound: stop"
                        putMVar x () 

sendPlay :: SoundService -> Sound -> IO Bool
sendPlay (SoundService s) f = do putMVar s (Play f)
                                 return True

stopSoundService :: SoundService -> IO ()
stopSoundService (SoundService s) = do
            m <- newEmptyMVar
            putMVar s (Stop m)
            takeMVar m

playSound buf = do
        -- Generate a single source, attach the buffer to it and start playing.
        source <- genObjectName
        buffer source $= Just buf
        play [source]
        -- Normally nothing should go wrong above, but one never knows...
        errs <- get alErrors
        unless (null errs) $ do
            hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
            --exitFailure
        -- Check every 0.1 seconds if the sound is still playing.
        let waitWhilePlaying = do
                sleep 0.1
                state <- get (sourceState source)
                when (state == Playing) $
                    waitWhilePlaying
        waitWhilePlaying
        return True

main :: IO ()
main = do
   -- sound
   s <- initSoundService
   -- physics
   initChipmunk
   space <- newSpace
   gravity space $= Vector 0 (-1) 
   border space ((-3.0, -3.0), (3.0, -3.0))
   let collHandler = liftIO $ sendPlay s Pow
   setDefaultCollisionHandler space $
      Handler {beginHandler     = Just collHandler
              ,preSolveHandler  = Nothing
              ,postSolveHandler = Nothing
              ,separateHandler  = Nothing}
   state <- makeState space
   -- display 
   simpleInit space
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state s space)
   mainLoop
