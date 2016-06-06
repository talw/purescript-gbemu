module Gpu
  ( gpuStep
  , gpuRd8
  , gpuWr8
  , wrVRam
  , cleanGpu
  , module Graphics.Canvas
  ) where

import Prelude (Unit, (*), (+), void, ($), (/=), bind, (-), unit, return
               ,(<), (&&), (>=), id, negate, not, (==), map, (++))
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Data.Function (runFn4, mkFn4)
import Data.List as L
import Data.Int.Bits ((.&.), (.|.), shl, zshr)
import Data.Foldable (traverse_)
import Data.Functor ((<$))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Canvas, Context2D, getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Partial (crash)

import Types (MemAccess, MemSection, I16, Gpu, I8, Palette, Color
             ,GpuMode(OamScan, HBlank, VBlank, VramScan), Tiles(Tiles))
import MemSection as M

--Set the color of a single canvas pixel.
foreign import setCanvasPixelColor :: forall e. Int -> Palette -> Int -> Int
                              -> Eff (canvas :: Canvas | e) Unit

--Updates canvas with the buffer in imgData,
--i.e. that one written to using setCanvasPixelColor.
foreign import setScreen :: forall e. Context2D
                         -> Eff (canvas :: Canvas | e) Unit

modeDuration :: GpuMode -> Int
modeDuration = case _ of
  HBlank -> 51
  VBlank -> 114
  VramScan -> 43
  OamScan -> 20

getCanvas :: forall e. Eff (canvas :: Canvas | e) Context2D
getCanvas = unsafePartial $ do 
  mCanvasElem <- getCanvasElementById canvasElemId
  case mCanvasElem of
    Just canvasElem -> getContext2D canvasElem
    Nothing -> crash
      $ "canvas with element id: " ++ canvasElemId ++ " was not found!"

cleanGpu :: Gpu
cleanGpu =
  { mTimer : modeDuration VBlank - 16
  , dispOn : true
  , bgOn : true
  , bgMap1 : false
  , bgSet1 : true
  , vblFinish : true
  , currLine : pixHeight + 10 - 1
  , currPos : 0 
  , yScroll : 0
  , xScroll : 0
  , vblIntrr : false
  , palette : M.getNew 16 0
  , mode : VBlank
  , tiles : Tiles $ M.getNew (384*64) 0
  , regs : M.getNew 0x40 0
  , vram : M.getNew 8192 0
  , oam : M.getNew 160 0
  }

cleanColor :: Color
cleanColor = {a:0,r:0,g:0,b:0}

--Update gpu every operation step using the timing of the operation that have
--just run. Basically updates an internal clk counter, and proceed to the 
--next gpu mode, if the current one finished.
gpuStep :: forall e. I8 -> Gpu -> Eff (canvas :: Canvas, ma :: MemAccess | e) Gpu
gpuStep _ gpu@{dispOn = false} = return gpu

gpuStep opTime gpu@{mTimer,mode,currLine,currPos,vblFinish} =
  map updateIfVblFinished eGpu'
 where
  mTimer' = mTimer + opTime

  eGpu' = if mTimer' < modeDuration mode
    then return gpu { mTimer = mTimer' }
    else case mode of 
      HBlank -> 
        let doOnLastLine gp = if currLine /= pixHeight - 1 then return gp
              else do
                {--traceA "draw!"--}
                setScreen =<< getCanvas

                return $ gp { mode = VBlank
                            , vblIntrr = true
                            }
         in doOnLastLine $ gpu { mTimer = mTimer' - modeDuration HBlank
                             , currLine = currLine + 1
                             , currPos = currPos + bytesWidth
                             , mode = OamScan
                             }  
        
      VBlank -> do
        let setOnLastLine =
              if vblFinish
                -- 10 increments after last line of 143
                then _ { vblFinish = false
                       , currLine = 0
                       , currPos = 0
                       , mode = OamScan
                       }
                else id
        return $ setOnLastLine gpu { mTimer = mTimer' - modeDuration VBlank , currLine = currLine + 1 }
      OamScan -> return gpu { mTimer = mTimer' - modeDuration OamScan, mode = VramScan }
      VramScan ->
        let renderOrNot = if gpu.dispOn && gpu.bgOn then renderLine else return
        in renderOrNot gpu { mTimer = mTimer' - modeDuration VramScan, mode = HBlank }

--Calculates if vertical blank line have finished
updateIfVblFinished :: Gpu -> Gpu
updateIfVblFinished gpu@{mTimer = mTimer', currLine, mode} =
  if mode == VBlank && mTimer' >= 4 && currLine == pixHeight + 10 - 1
    then gpu { currLine = 0, vblFinish = true }
    else gpu

--Write the current line to the canvas' imgData.
--Canvas will be refreshed once all lines are written.
renderLine :: forall e. Gpu -> Eff (ma :: MemAccess, canvas :: Canvas | e) Gpu
--NOTE: Change condition of renderOrNot once object rendering is added
--And perhaps rename the current renderLine to renderBG and have renderLine
--be what calls renderBG and renderFG
renderLine gpu = do
  tileIx <- getTileIx gpu tileIxAddr
  foldRes <- L.foldM updateCanvas
                     { tho:tileHorizOff
                     , mho:memHorizOff, tix:tileIx }
                     (0 L... (pixWidth-1))
  return gpu
 where
  tileIxAddr = bgMapOff + memVertOff + memHorizOff
  --map #0 9800-9BFF, map #1 9C00-9FFF
  bgMapOff = if gpu.bgMap1 then 0x1C00 else 0x1800
  --Each tile's height is 3 bits long,
  --and each map row of tiles is 5 bits (32 tiles wide)
  memVertOff = (pixelVertOff `zshr` 3) `shl` 5
  --The map's height is 256 pixels long
  pixelVertOff = 255 .&. (gpu.currLine + gpu.yScroll)
  -- the map is 32 tiles wide
  memHorizOff = 31 .&. (gpu.xScroll `zshr` 3)
  --tile height is 8 pixels / 3 bits long
  tileVertOff = 7 .&. (gpu.currLine + gpu.yScroll)
  --tile width is 8 pixels / 3 bits wide
  tileHorizOff = 7 .&. gpu.xScroll

  setCanvasPixelColor' = mkFn4 setCanvasPixelColor

  updateCanvas {tho,mho,tix} i = do
    --Would've rewritten with a single 'if', if PureScript had supported
    --pattern matching in where clause
    let tho' = if lastTileRowPixel
          then 0
          else tho + 1
        mho' = if lastTileRowPixel
          then 31 .&. (1 + mho)
          else mho
    tix' <- if lastTileRowPixel
      then getTileIx gpu $ bgMapOff + memVertOff + mho'
      else return tix

    --NOTE trace error, if invalid color ix
    colorIx <- getTilePixel tho tileVertOff tix gpu.tiles 

    runFn4 setCanvasPixelColor' colorIx pal (i*4) gpu.currLine
    return $ {tho:tho',mho:mho',tix:tix'}
   where
    lastTileRowPixel = tho == 7
    pal = gpu.palette

--Convert the tile index the Game Boy uses to one that fits our arrays.
getTileIx :: forall e. Gpu -> I16 -> Eff (ma :: MemAccess | e) I8
getTileIx { vram, bgSet1 } addr = do
  tix <- vram M.!! addr
  return $ if (not bgSet1) then tix + 256 else tix

--Reading a byte from gpu's non-vram memory, used by MainMem's rd8 function.
gpuRd8 :: forall e. I16 -> Gpu -> Eff (ma :: MemAccess | e) I8
gpuRd8 addr gpu = case addr of
  0xFF40 -> return $ getCtrlFlags gpu
  0xFF42 -> return gpu.yScroll
  0xFF43 -> return gpu.xScroll
  0xFF44 -> return gpu.currLine
  0xFF47 -> return $ -1 --NOTE log this
                        --if needed implement getPalette Seq{argb}->I8
  otherwise -> gpu.regs M.!! (addr - 0xFF40)

--Writing a byte to gpu's non-vram memory, used by MainMem's wr8 function.
gpuWr8 :: forall e. I8 -> I16 -> Gpu -> Eff (ma :: MemAccess | e) Gpu
gpuWr8 i8 addr gpu = do
  setRegs
  case addr of
    0xFF40 -> return $ setCtrlFlags i8 gpu
    0xFF42 -> return gpu { yScroll = i8 }
    0xFF43 -> return gpu { xScroll = i8 }
    0xFF44 -> return gpu { currLine = i8 }
    0xFF47 -> setPalette  
    otherwise -> return gpu
 where
  setRegs = M.replace i8 (addr - 0xFF40) gpu.regs 
  setPalette = gpu <$ traverse_ stPl (0 A... 3)
  --Every color is 2 bits wide
  --NOTE should this be reversed?
  stPl i = case (i8 `zshr` (i*2)) .&. 3 of
    0 -> setColor {a:255,r:255,g:255,b:255} i gpu.palette
    1 -> setColor {a:255,r:192,g:192,b:192} i gpu.palette
    2 -> setColor {a:255,r:96 ,g:96 ,b:96} i gpu.palette
    3 -> setColor {a:255,r:0  ,g:0  ,b:0} i gpu.palette
    otherwise -> return unit -- NOTE log this

--Write one of palette's 4 colors.
setColor :: forall e. {a :: Int, r :: Int, g :: Int, b :: Int} -> Int -> Palette
         -> Eff (ma :: MemAccess | e) Unit
setColor {a,r,g,b} cIx pal = do
  M.replace a i pal
  M.replace r (i + 1) pal
  M.replace g (i + 2) pal
  M.replace b (i + 3) pal
  return unit
 where i = cIx*4

--Return the current state of gpu ctrl flags,
--as a bits flag the Game Boy recognizes.
getCtrlFlags :: Gpu -> I8
getCtrlFlags gpu =  cf gpu.bgOn   0x01
                .|. cf gpu.bgMap1 0x08
                .|. cf gpu.bgSet1 0x10
                .|. cf gpu.dispOn 0x80
 where cf bool flag = if bool then flag else 0x00

--Reflect the bits flag in Gpu's ctrl attributes.
setCtrlFlags :: I8 -> Gpu -> Gpu
setCtrlFlags ctrlFlags gpu = enableDisableOrNot gpu.dispOn dispOn'
  gpu
    { bgOn   = isFlagSet 0x01
    , bgMap1 = isFlagSet 0x08
    , bgSet1 = isFlagSet 0x10
    , dispOn = dispOn'
    }
 where
  isFlagSet flag = (flag .&. ctrlFlags) /= 0
  dispOn' = isFlagSet 0x80

  enableDisableOrNot true false = disableGpu
  enableDisableOrNot false true = enableGpu
  enableDisableOrNot _ _ = id

--Used when dispOn changes from true to false
disableGpu :: Gpu -> Gpu
disableGpu gpu = gpu
  { mTimer = 0
  , currLine = 0
  , mode = HBlank
  , vblFinish = false
  , dispOn = false
  }

--Used when dispOn changes from false to true
enableGpu :: Gpu -> Gpu
enableGpu gpu = gpu
  { mTimer = 0
  , mode = OamScan
  , dispOn = true
  }

--Write a byte to vram, used by MainMem's wr8 function.
--In addition to a regular write, the changes are reflected in Gpu's internal
--representation of the tiles.
wrVRam :: forall e. I8 -> I16 -> Gpu -> Eff (ma :: MemAccess | e) Unit
wrVRam i8 addr gpu@{vram,tiles} = do
  vram' <- M.replace i8 addr' vram
  if (addr >= 0x8000 && addr < 0x9800)
    then vramWriteToTiles addr' vram' tiles
    else return unit
 where
  addr' = 0x1FFF .&. addr
  



--------------TILE--------    R O W
-- b    b b b b    b b b b    b b b b
--Each tile is 8 * 8 pixels * 2 bits per pixel = 16 bytes
--Each row is 8 pixels * 2 bits per pixel = 16 bits = 2 bytes

--Total of 384 TILES from 0x8000 to 0x97FF
--2 tile maps from 0x9800 to 0x9FFF

--Reflect vram writes in the Gpu's internal representation of the tiles.
vramWriteToTiles :: forall e. I16 -> MemSection -> Tiles -> Eff (ma :: MemAccess | e) Unit
vramWriteToTiles addr vram tiles =
  traverse_ updateColor (0 A... 7)
 where
  -- Doesn't matter if a byte of LOWER color bit of 8 pixels, or HIGHER have
  -- changed, I have to update those 8 pixels colors just the same.
  addr' = addr - (addr .&. 1)
  tileIx = (addr `zshr` 4) .&. 511
  y = (addr `zshr` 1) .&. 7

  updateColor :: Int -> Eff (ma :: MemAccess | e) Unit
  updateColor x = do
    highColors <- vram M.!! addr + 1
    lowColors <- vram M.!! addr
    let c =  (if (shiftMask .&. highColors) /= 0 then 2 else 0)
         .|. (if (shiftMask .&. lowColors) /= 0 then 1 else 0)
    setTilePixel c x y tileIx tiles
   where
    shiftMask = 1 `shl` (7-x)

--NOTE Add a check that tile index is valid (0<=i<384)
getTilePixel :: forall e. Int -> Int -> Int -> Tiles
             -> Eff (ma :: MemAccess | e) Int
getTilePixel x y tileIx (Tiles ms) = ms M.!! (tileIx*64) + (y*8) + x

--NOTE Add a check that indices and color are valid (0<=i<8) (0<=c<4)
setTilePixel :: forall e. Int -> Int -> Int -> Int -> Tiles
             -> Eff (ma :: MemAccess | e) Unit
setTilePixel c x y tileIx (Tiles ms) = void
  $ M.replace c ((tileIx*64) + (y*8) + x) ms

pixWidth :: Int
pixWidth = 160

bytesWidth :: Int
bytesWidth = 160*4

pixHeight :: Int
pixHeight = 144

canvasElemId :: String
canvasElemId = "screen"
