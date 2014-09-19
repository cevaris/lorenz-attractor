import Numeric
import Data.List.Split
import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess), exitFailure )

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw.ARB.WindowPos


----------------------------------------------------------------------------------------------------------------
-- Global State
type View = (GLfloat, GLfloat, GLfloat)

data Zoom = In | Out
data Mod = Increase | Decrease
zoomDelta = 9e-4

data State = State {
   frames  :: IORef Int,
   t0      :: IORef Int,
   ph'     :: IORef GLfloat,
   th'     :: IORef GLfloat,
   info    :: IORef (String,String),
   zoom    :: IORef GLfloat,
   rho     :: IORef Float,
   sigma   :: IORef Float,
   beta    :: IORef Float,
   dt      :: IORef Float,
   steps   :: IORef Integer
 }

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 0
  th <- newIORef 0
  i  <- newIORef ("","")
  z  <- newIORef 0.019
  r  <- newIORef 28
  s  <- newIORef 10
  b  <- newIORef (8/3)
  d  <- newIORef 0.001
  st <- newIORef 50000
  return $ State { 
    frames = f, t0 = t, ph' = ph, th' = th, info = i, zoom = z,
    rho = r, sigma = s, beta = b, dt = d, steps = st
  }

colorPoints :: Integer -> [(Integer, Integer, Integer)]
colorPoints s = do 
  let colors = [(x,y,z) | x <- [1..255], y <-[1..255], z <-[1..255]]
      chunkCount = (length colors) `div` (fromIntegral s :: Int)
      chunks = chunksOf chunkCount colors
  map (head) chunks


----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Lorenz
--ddt :: (Float, Float, Float) -> Vertex3 Float
--ddt (x, y, z) = (Vertex3 x' y' z')
--  where
--    x' = sigma*(y-x)
--    y' = x*(rho-z) - y
--    z' = x*y- beta*z

data Lorenz = Lorenz {c::(Integer, Integer, Integer), step::Integer, x::Float, y::Float, z::Float} deriving (Read, Show, Eq)
data LorenzAttribute = Sigma | Beta | Rho | Dt | Step deriving (Read, Show, Eq)
type LzParams = (Float, Float, Float, Float, Integer)


lzState :: State -> IO LzParams
lzState state = do
  s  <- get (sigma state)
  r  <- get (rho state)
  b  <- get (beta state)
  d  <- get (dt state)
  st <- get (steps state)
  return (s, r, b, d, st)

lzBase   = Lorenz (1, 1, 1) 0 1 1 1

lorenz  :: LzParams -> [Lorenz]
lorenz (s, r, b, d, st) = go (colorPoints st) lzBase [lzBase]
        where 
          go :: [(Integer, Integer, Integer)] -> Lorenz -> [Lorenz] -> [Lorenz]
          go (g:gs) (Lorenz c i x y z) xs = if i >= st
            then reverse xs
            else let l = Lorenz g (i+1) (x+d*(s*(y-x))) (y+d*(x*(r-z)-y)) (z+d*(x*y-b*z))
                 in go gs l (l:xs)

lorenzPoints :: LzParams -> [(Float,Float,Float)] 
lorenzPoints lzp = map (\(Lorenz c i x y z) -> (x, y, z)) (lorenz lzp)
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Grid

gridPoints :: [(Float, Float, Float)]
gridPoints = [(0,0,0),(1,0,0),
              (0,0,0),(0,1,0),
              (0,0,0),(0,0,1)]

----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- Key Binding

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 's')           _ _ _ = modLorenz state Sigma Increase
keyboard state (Char 'S')           _ _ _ = modLorenz state Sigma Decrease
keyboard state (Char 'r')           _ _ _ = modLorenz state Rho Increase
keyboard state (Char 'R')           _ _ _ = modLorenz state Rho Decrease
keyboard state (Char 'b')           _ _ _ = modLorenz state Beta Increase
keyboard state (Char 'B')           _ _ _ = modLorenz state Beta Decrease
keyboard state (Char 'd')           _ _ _ = modLorenz state Dt Increase
keyboard state (Char 'D')           _ _ _ = modLorenz state Dt Decrease
keyboard state (Char 'p')           _ _ _ = modLorenz state Step Increase
keyboard state (Char 'P')           _ _ _ = modLorenz state Step Decrease
keyboard state (Char 'z')           _ _ _ = modScale state In
keyboard state (Char 'Z')           _ _ _ = modScale state Out
keyboard state (Char 'h')           _ _ _ = resetState state
keyboard state (SpecialKey KeyUp)   _ _ _ = modRotate state KeyUp
keyboard state (SpecialKey KeyDown) _ _ _ = modRotate state KeyDown
keyboard state (SpecialKey KeyLeft) _ _ _ = modRotate state KeyLeft
keyboard state (SpecialKey KeyRight)_ _ _ = modRotate state KeyRight
keyboard _     (Char '\27')         _ _ _ = exitWith ExitSuccess
keyboard _     _                    _ _ _ = return ()


----------------------------------------------------------------------------------------------------------------
modLorenz :: State -> LorenzAttribute -> Mod -> IO ()
modLorenz state Sigma Increase = sigma state $~! (+1)
modLorenz state Sigma Decrease = sigma state $~! (\x -> x - 1)
modLorenz state Rho   Increase = rho   state $~! (+2)
modLorenz state Rho   Decrease = rho   state $~! (\x -> x - 2)
modLorenz state Beta  Increase = beta  state $~! (+0.25)
modLorenz state Beta  Decrease = beta  state $~! (\x -> x - 0.25)
modLorenz state Dt    Increase = dt    state $~! (+0.0005)
modLorenz state Dt    Decrease = dt    state $~! (\x -> x - 0.0005)
modLorenz state Step  Increase = steps  state $~! (+500)
modLorenz state Step  Decrease = steps  state $~! (\x -> x - 500)

resetState :: State -> IO ()
resetState state = do
  state <- makeState
  return ()

modScale :: State -> Zoom -> IO ()
modScale state In = do
  z <- get (zoom state)
  if 2e-2 <= z 
    then (zoom state) $~! (\x -> x - zoomDelta)
    else (zoom state) $~! (+zoomDelta)
  postRedisplay Nothing
modScale state Out = do
  z <- get (zoom state)
  if 6e-3 >= z 
    then (zoom state) $~! (+zoomDelta)
    else (zoom state) $~! (\x -> x - zoomDelta)
  postRedisplay Nothing

modRotate :: State -> SpecialKey -> IO ()
modRotate state KeyDown = do
  ph' state $~! (+5)
  postRedisplay Nothing
modRotate state KeyUp  = do
  ph' state $~! (\x -> x - 5)
  postRedisplay Nothing
modRotate state KeyRight = do
  th' state $~! (+5)
  postRedisplay Nothing
modRotate state KeyLeft = do
  th' state $~!(\x -> x - 5)
  postRedisplay Nothing


idle :: State -> IdleCallback
idle state = do
   postRedisplay Nothing

visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing

reshape :: ReshapeCallback
reshape s@(Size width height) = do
  let wf = fromIntegral width
      hf = fromIntegral height

  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity  

  if width <= height
    then ortho (-1) 1 (-1) (hf/wf) (-1) (1:: GLdouble)
    else ortho (-1) (wf/hf) (-1) 1 (-1) (1:: GLdouble)
  --perspective 90 (wf/hf) 0 1
  matrixMode $= Modelview 0

  loadIdentity

toGfloat :: Float -> GLfloat
toGfloat f = (realToFrac f)::GLfloat

drawVertex3f :: Float -> Float -> Float -> IO ()
drawVertex3f x y z = vertex $ vertex3f x y z

vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat)

vertex4f :: Float -> Float -> Float -> Float -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat) ((realToFrac w)::GLfloat)

glWindowPos :: GLfloat -> GLfloat -> IO ()
glWindowPos x y = glWindowPos2f x y


timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)

round2 :: Float -> String
round2 x = showFFloat (Just 2) x ""

roundGL2 :: GLfloat -> String
roundGL2 x = showGFloat (Just 2) x ""

updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    (s, r, b, d, st) <- (lzState state)
    
    zoom <- get (zoom state)
    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ roundGL2 ph ++ "] [th " ++ roundGL2 th ++ "] [z "  ++ roundGL2 zoom ++ "]", 
                  "[sigma " ++ round2 s ++ "] [rho " ++ round2 r ++ "] [beta " ++ round2 b ++ "] [dt " ++ show d ++ "] [steps " ++ show st ++ "]")
    info state $= result
    t0 state $= t
    frames state $= 0

color3f :: Float -> Float -> Float -> IO ()
color3f x y z = color (Color3 ((realToFrac x)::GLfloat) ((realToFrac y)::GLfloat) ((realToFrac z)::GLfloat))

drawGrid :: State -> IO DisplayList
drawGrid state = do 
  grid <- defineNewList Compile $ do
    renderPrimitive Lines $ do
      mapM_ (\(x, y, z) -> drawVertex3f x y z ) gridPoints
  return grid

drawLorenz :: State -> IO DisplayList
drawLorenz state = do
  lzp <- (lzState state)
  lorenzAttractor <- defineNewList Compile $ do
    renderPrimitive LineStrip $ do
      mapM_ (\(Lorenz (a, b, c) i x y z) -> do
        --color3f ((x+i)::Float) ((y+i)::Float) ((z+i)::Float)
        --color3f (i::Float) (i::Float) (i::Float)
        --color3f (x*(fromIntegral (i))::Float) (y*(fromIntegral i)::Float) (z*(fromIntegral i)::Float)
        color3f ((fromIntegral a) :: Float) ((fromIntegral b) :: Float) ((fromIntegral c) :: Float)
        drawVertex3f x y z) (lorenz lzp)

  return lorenzAttractor

draw :: State -> IO ()
draw state = do
    
  clear [ ColorBuffer, DepthBuffer ]

  ph <- get (ph' state)
  th <- get (th' state)
  info <- get (info state)
  zoom <- get (zoom state)
  lz  <- (drawLorenz state)
  grid <- (drawGrid state)
  
  loadIdentity

  scale zoom zoom (zoom::GLfloat)

  rotate ph (Vector3 1 0 0)
  rotate th (Vector3 0 1 0)

  -- Set up perspective
  lookAt (Vertex3 0.1 0 0.1) (Vertex3 0 0 0) (Vector3 0 1 0)

  preservingMatrix $ do       
    lineWidth $= 0.5
    callList lz
  
  preservingMatrix $ do
    lineWidth $= 2
    scale 45 45 (45::GLfloat)
    callList grid

  preservingMatrix $ do
    currentRasterPosition $= vertex4f 45 0 0 1
    renderString Helvetica18 $ "X"
    currentRasterPosition $= vertex4f 0 45 0 1
    renderString Helvetica18 $ "Y"
    currentRasterPosition $= vertex4f 0 0 45 1
    renderString Helvetica18 $ "Z"
    currentRasterPosition $= vertex4f 0 0 0 1

  preservingMatrix $ do
    glWindowPos 5 30
    renderString Helvetica18 $ (fst info)
    glWindowPos 5 5
    renderString Helvetica18 $ (snd info)

  --preservingMatrix $ do
  --  pointSize $= 2
  --  renderPrimitive Points $ do
  --    let lzp = (s, r, b, d, st)
  --    --0.009
  --    mapM_ (\(x, y, z) -> drawVertex3f x y z ) (lorenzPoints lzp)

  swapBuffers
  updateInfo state
  reportErrors
  

myInit :: [String] -> State -> IO ()
myInit args state = do
  --position (Light 0) $= Vertex4 5 5 15 0
  --cullFace $= Just Back
  --lighting $= Enabled
  --light (Light 0) $= Enabled
  depthFunc $= Just Less
  shadeModel $= Flat
  depthRange $= (0, 1)

----------------------------------------------------------------------------------------------------------------
-- Key Binding

main :: IO ()
main = do
    initialWindowSize $= Size 800 800
    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    
    initialWindowPosition $= Position 500 500
    _window <- createWindow "Lorenz Attractor"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just reshape
    
    keyboardMouseCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)
    mainLoop
  


