module SDLUtils where

import qualified SDL
import Data.StateVar
import Data.Text
import Linear
import Data.Bits
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Endian


runSDL :: IO () -> IO ()
runSDL program = do
    --SDL.initialize [SDL.InitVideo]
    SDL.initializeAll
    program
    SDL.quit

withWindow :: String -> SDL.WindowConfig -> (SDL.Window -> IO ()) -> IO ()
withWindow title config func = do
    window <- SDL.createWindow (pack title) config
    func window
    SDL.destroyWindow window

withRenderer :: SDL.RendererConfig -> SDL.Window -> (SDL.Renderer -> IO ()) -> IO ()
withRenderer rendererConf window func = do
    renderer <- SDL.createRenderer window (-1) rendererConf
    func renderer
    SDL.destroyRenderer renderer

withTexture :: SDL.Renderer -> V2 CInt -> (SDL.Texture -> IO ()) -> IO ()
withTexture rend size action = do
    texture <- SDL.createTexture rend SDL.ARGB8888 SDL.TextureAccessStreaming size
    action texture
    SDL.destroyTexture texture

withPixelsFromTexture :: SDL.Texture -> (Int -> Ptr () -> IO ()) -> IO ()
withPixelsFromTexture texture renderer = do
      (pixels, pitch) <- SDL.lockTexture texture Nothing
      renderer (fromIntegral pitch) pixels
      SDL.unlockTexture texture

-- RENDERING
{-
renderCairoViaTexture :: Cairo.Render () -> SDL.Window -> SDL.Renderer -> IO ()
renderCairoViaTexture cairoRend win rend = do
  size@(V2 w h) <- get $ SDL.windowSize win
  withTexture rend size $ \ texture -> do
      withPixelsFromTexture texture $ cairoRenderToPixels cairoRend (fromIntegral w) (fromIntegral h)
      sdlRenderTexture rend texture

sdlRenderTexture :: SDL.Renderer -> SDL.Texture -> IO ()
sdlRenderTexture renderer texture = do
  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  return ()

cairoRenderToPixels :: Cairo.Render () -> Int -> Int -> Int -> Ptr () -> IO ()
cairoRenderToPixels graphic w h pitch pixels =
  Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 w h pitch $ \ surface ->
    Cairo.renderWith surface graphic

-- destroys texture's content!!!
resizeTexture :: SDL.Renderer -> SDL.Texture -> IO SDL.Texture
resizeTexture rend texture = do
  (SDL.TextureInfo format access width height) <- SDL.queryTexture texture
  SDL.destroyTexture texture
  SDL.createTexture rend format access $ V2 (fromIntegral width) (fromIntegral height)
-}
