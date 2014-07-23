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
import Data.Maybe (isJust)
import Data.List (genericLength, genericDrop)
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure)
import Graphics.UI.GLUT hiding (position, scale)
import Unsafe.Coerce
import Physics.Hipmunk hiding (Position)
import Control.Concurrent.MVar
import Control.Concurrent (forkOS)
import Sound.ALUT hiding (Static)
import Data.List (intersperse)
import Control.Monad (when, unless, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import System.IO ( hPutStrLn, stderr )
import Foreign ( withArray )
import Foreign.C.String ( castCharToCChar )
import Data.Bits ( (.&.) )
import Codec.Image.STB
import Image
import GHC.Float
import Foreign.C.Types (CFloat(..), CDouble(..))

data GameState = GameState { ledgePos :: IORef CpFloat, ledge :: IORef Shape, mainBall :: IORef Shape }

-- constants

type Seconds = CpFloat
subStepQuantum :: Seconds = 0.1
--subStepQuantum :: Seconds = 0.01

ballRadius :: CpFloat = 50
ledgeHeight :: CpFloat = 150

-- constants end

makeState :: Space -> IO GameState
makeState space = do mBall <- ball space (200, 200) ballRadius 
                     mainBall <- newIORef mBall
                     ledgeShape <- rectangleShape space 200
                     ledgePos <- newIORef 200
                     ledge <- newIORef ledgeShape
                     let gamestate = GameState { ledgePos = ledgePos, ledge = ledge, mainBall = mainBall }
                     return gamestate

checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 64 64

withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   -- list comprehension pixels w h, returning list with f = (\c -> Color4 c c c 255)
   -- .&. bitwise and with 0x08 = 0100
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte

simpleInit :: IO ()
simpleInit = do
    let title = "awesome game"
    _ <- getArgsAndInitialize
    -- gameModeCapabilities $= [ Where' GameModeBitsPerPlane IsEqualTo 24 ]
    -- depthFunc $= Just Less
    initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
    rowAlignment Unpack $= 1 -- http://www.khronos.org/opengles/sdk/docs/man/xhtml/glPixelStorei.xml
    
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 100 100

    _ <- createWindow title
    actionOnWindowClose $= MainLoopReturns

    clearColor $= Color4 1 1 1 1

    shadeModel $= Flat

loadTexture :: IO (Maybe TextureObject)
loadTexture = do
    exts <- get glExtensions
    mbTexName <- if "GL_EXT_texture_object" `elem` exts
                   then fmap Just genObjectName
                   else return Nothing
    when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
    textureFunction $= Replace
    {-
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest) -- nearest or linear
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge) -- or ClampToEdge if bigger than shape. if clam s and t from 0 to 1, if repeat then depends on repeats
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    withCheckImage checkImageSize 0x08 (\c -> Color4 c c c 255) $
      texImage2D Texture2D NoProxy 0  RGBA' checkImageSize 0
    -}
    Right img <- loadImage "images/smiley.png"
    tex <- compileTexture2DRGBAF False True img
    return mbTexName

framerate space = do
    let scheduleTick = do
            let fps = 60
            addTimerCallback (1000 `div` fps) tick

        tick = do
            postRedisplay Nothing
            step space subStepQuantum
            scheduleTick

    scheduleTick

toVertex :: (GLdouble, GLdouble) -> IO ()
toVertex (x,y) = vertex $ Vertex2 x y


-- texture
-- Typical values in glTexCoord2f are 0.0->1.0
-- http://stackoverflow.com/questions/8762826/texture-mapping-a-circle-made-using-gl-polygon
toTexture :: (GLfloat, GLfloat) -> IO ()
toTexture (x,y) = texCoord2f (TexCoord2 x y)
                where texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()


toVertexAndTexture :: (GLdouble, GLdouble) -> GLdouble -> GLdouble -> IO ()
toVertexAndTexture (x,y) r angle = do toVertex (x + cos(angle)*r, y + sin(angle)*r)
                                      toTexture (glDoubleToGLfloat (0.5 + cos(angle)*0.5), glDoubleToGLfloat (0.5 + sin(angle)*0.5))

toVandT2 (x,y) (w,h,s,t) = do toVertex (x + w, y + h)
                              toTexture (s,t)

-- types
-- type CpFloat = Double
-- type GLdouble = CDouble
-- type GLfloat = CFloat
cpFloatToGLdouble :: CpFloat -> GLdouble
cpFloatToGLdouble = unsafeCoerce

glDoubleToGLfloat :: GLdouble -> GLfloat
glDoubleToGLfloat (CDouble d) = CFloat $ double2Float d

cDoubleTuple :: (Double, Double) -> (CDouble, CDouble)
cDoubleTuple (x, y) = (unsafeCoerce x, unsafeCoerce y)

-- circle around position
circle :: (GLdouble, GLdouble) -> IO ()
circle (x,y) = preservingMatrix $ do
    let poly  = 24
        ang p = p * 2 * pi / poly
        r = cpFloatToGLdouble ballRadius
        pos   = map (\p -> (x+cos(ang p)*r, y + sin(ang p)*r)) [1,2..poly]
    -- we now want to use the texture
    color $ Color3 1 1 (1 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.Polygon $ do
        mapM_ (toVertexAndTexture (x,y) r . ang) [1,2..poly]
    color $ Color3 0 0 (0 :: GLdouble)
    renderPrimitive Graphics.UI.GLUT.LineLoop $ do
        mapM_ (toVertex) pos

rectDims :: [(CpFloat, CpFloat)]
rectDims = [((-50.0), 10.0), (50.0, 10.0), (50.0, (-10.0)), ((-50.0), (-10.0))]

borderPos :: ((CpFloat, CpFloat), (CpFloat, CpFloat))
borderPos = ((0.0, 0.0), (500.0, 0.0)) -- draw border

-- position of rectangle: it's centre top
rectangle :: CpFloat -> IO ()
rectangle x1 = do
            color $ Color3 0 0 (0 :: GLdouble)
            let y1 = ledgeHeight -- constant y
                vert = map (\(w, h) -> (x1 + w, ledgeHeight + h)) rectDims
            renderPrimitive Graphics.UI.GLUT.Polygon $
                mapM_ (toVertex . cDoubleTuple) vert

line :: ((Double, Double), (Double, Double)) -> IO ()
line ((x1, y1), (x2, y2)) = do
            color $ Color3 0 0 (0 :: GLdouble)
            renderPrimitive Graphics.UI.GLUT.Lines $
                mapM_ (toVertex . cDoubleTuple) [(x1, y1), (x2, y2)]


-- ball/circle radius should be defined in one place
ball :: Space -> (CpFloat, CpFloat) -> Double -> IO Shape
ball space pos rad = do
    -- Body.
    let m = 1000000 -- just setting a mass
        vel = (0.0, -0.2)
    b <- newBody m infinity
    position b $= uncurry Vector pos
    velocity b $= uncurry Vector vel
    spaceAdd space b

    -- Shape.
    bshape <- newShape b (Circle $ rad) (Vector 0 0)
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

space = [
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

letters :: [[GLubyte]]
letters = [
   [ 0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xff, 0xc3, 0xc3, 0xc3, 0x66, 0x3c, 0x18 ],
   [ 0x00, 0x00, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe ],
   [ 0x00, 0x00, 0x7e, 0xe7, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xe7, 0x7e ],
   [ 0x00, 0x00, 0xfc, 0xce, 0xc7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc7, 0xce, 0xfc ],
   [ 0x00, 0x00, 0xff, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xff ],
   [ 0x00, 0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xc0, 0xc0, 0xc0, 0xff ],
   [ 0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xcf, 0xc0, 0xc0, 0xc0, 0xc0, 0xe7, 0x7e ],
   [ 0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xff, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3 ],
   [ 0x00, 0x00, 0x7e, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x7e ],
   [ 0x00, 0x00, 0x7c, 0xee, 0xc6, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06 ],
   [ 0x00, 0x00, 0xc3, 0xc6, 0xcc, 0xd8, 0xf0, 0xe0, 0xf0, 0xd8, 0xcc, 0xc6, 0xc3 ],
   [ 0x00, 0x00, 0xff, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0 ],
   [ 0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xdb, 0xff, 0xff, 0xe7, 0xc3 ],
   [ 0x00, 0x00, 0xc7, 0xc7, 0xcf, 0xcf, 0xdf, 0xdb, 0xfb, 0xf3, 0xf3, 0xe3, 0xe3 ],
   [ 0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xe7, 0x7e ],
   [ 0x00, 0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe ],
   [ 0x00, 0x00, 0x3f, 0x6e, 0xdf, 0xdb, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0x66, 0x3c ],
   [ 0x00, 0x00, 0xc3, 0xc6, 0xcc, 0xd8, 0xf0, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe ],
   [ 0x00, 0x00, 0x7e, 0xe7, 0x03, 0x03, 0x07, 0x7e, 0xe0, 0xc0, 0xc0, 0xe7, 0x7e ],
   [ 0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0xff ],
   [ 0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3 ],
   [ 0x00, 0x00, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3 ],
   [ 0x00, 0x00, 0xc3, 0xe7, 0xff, 0xff, 0xdb, 0xdb, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3 ],
   [ 0x00, 0x00, 0xc3, 0x66, 0x66, 0x3c, 0x3c, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3 ],
   [ 0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3 ],
   [ 0x00, 0x00, 0xff, 0xc0, 0xc0, 0x60, 0x30, 0x7e, 0x0c, 0x06, 0x03, 0x03, 0xff ]]

printString :: DisplayList -> String -> IO ()
printString fontOffset s =
   preservingAttrib [ ListAttributes ] $ do
      listBase $= fontOffset
      withArray (map charToGLubyte s) $
         callLists (genericLength s) UnsignedByte

charToGLubyte :: Char -> GLubyte
charToGLubyte = fromIntegral . castCharToCChar

makeRasterFont :: IO DisplayList
makeRasterFont = do
   rowAlignment Unpack $= 1

   fontDisplayLists@(fontOffset:_) <- genObjectNames 128
   let listsStartingWith ch = genericDrop (charToGLubyte ch) fontDisplayLists
       makeLetter dl letter =
          defineList dl Compile $
             withArray letter $
                bitmap (Size 8 13) (Vertex2 0 2) (Vector2 10 0)

   zipWithM_ makeLetter (listsStartingWith 'A') letters
   makeLetter (head (listsStartingWith ' ')) space
   return fontOffset

display :: GameState -> Maybe TextureObject -> DisplayCallback
display gamestate mbTexName = do
   clear [ ColorBuffer, DepthBuffer ]
   ledgePos <- get (ledgePos gamestate)
   mainBall <- get (mainBall gamestate)

   Vector x y <- get $ position (body mainBall)

   -- draw ball with texture
   texture Texture2D $= Enabled
   textureFunction $= Replace
   when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
   circle (cpFloatToGLdouble x, cpFloatToGLdouble y)
   texture Texture2D $= Disabled

   -- some letters
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   shadeModel $= Flat
   fontOffset <- makeRasterFont
   rasterPos2i (Vertex2 20 60)
   printString fontOffset "THE QUICK BROWN FOX JUMPS"

   rectangle (ledgePos) -- draw ledge

   line borderPos

   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
   matrixMode $= Modelview 0
{-
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 65 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   translatef (Vector3 0 0 (-5)) -- this lifts us up to see the x-y plane
-}

keyboardMouse :: GameState -> SoundService -> Space -> KeyboardMouseCallback
keyboardMouse gamestate soundService space key Down _ _ =
   case key of
    SpecialKey KeyRight -> update ledgePos 1.0
    SpecialKey KeyLeft -> update ledgePos (-1.0)
    --SpecialKey KeyUp -> update y 0.1
    --SpecialKey KeyDown -> update y (-0.1)
    Char '\27' -> stopAll
    _     -> return ()
   where update pos inc = do
            pos gamestate $~ (+ inc)
            updateRectangleBody gamestate inc
            postRedisplay Nothing
keyboardMouse _ _ _ _ _ _ _ = return ()

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
           __ <- forkOS (soundService s) -- forkIO might not be enough, might need forkOS
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
        return True

main :: IO ()
main = do
   -- sound
   s <- initSoundService
   -- physics
   initChipmunk
   space <- newSpace
   gravity space $= Vector 0 (-1) 
   border space borderPos
   let collHandler = liftIO $ sendPlay s Pow
   setDefaultCollisionHandler space $
      Handler {beginHandler     = Just collHandler
              ,preSolveHandler  = Nothing
              ,postSolveHandler = Nothing
              ,separateHandler  = Nothing}
   state <- makeState space
   -- display 
   simpleInit
   mbTexName <- loadTexture
   framerate space
   displayCallback $= display state mbTexName
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state s space)
   mainLoop
