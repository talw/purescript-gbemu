module Gpu
  ( resetScreen
  , gpuStep
  , gpuRd8
  , gpuWr8
  , wrVRam
  , cleanGpu
  , module Graphics.Canvas
  ) where


import Prelude
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int
import Data.Int.Bits
import Data.Foldable
import Data.Array as A
import Data.Sequence as S
import Data.Maybe
import Graphics.Canvas

import Types
import Utils

foreign import setScreen :: forall e. Array I8 -> Context2D
                       -> Eff (canvas :: Canvas | e) Unit

resetScreen :: forall e. Eff (canvas :: Canvas | e) Unit
resetScreen = setScreen arr =<< getCanvas
 where
  arr = A.replicate (160*144*4) 255

getCanvas :: forall e. Eff (canvas :: Canvas | e) Context2D
getCanvas = do 
  Just canvasElem <- getCanvasElementById "screen"
  getContext2D canvasElem

--NOTE Add a check that tile index is valid (0<=i<384)
setTile :: Tile -> Int -> Tiles -> Tiles
setTile t i ts = S.replace t i ts

--NOTES  replace fromMaybe with something that will log invalid indices
getTile :: Int -> Tiles -> Tile
getTile ix = getFromSeq cleanTile ix

getTilePixel :: Int -> Int -> Tile -> Int
getTilePixel x y (Tile s) = s !! (y*8 + x)

--NOTE Add a check that indices and color are valid (0<=i<8) (0<=c<4)
setTilePixel :: Int -> Int -> Int -> Tile -> Tile
setTilePixel c x y (Tile s) = Tile $ S.replace c (y*8 + x) s

pixWidth :: Int
pixWidth = 160

bytesWidth :: Int
bytesWidth = 160*4

pixHeight :: Int
pixHeight = 144

modeDuration :: GpuMode -> Int
modeDuration = case _ of
  HBlank -> 51
  VBlank -> 114
  VramScan -> 43
  OamScan -> 20

getCtrlFlags :: Gpu -> I8
getCtrlFlags gpu =  cf gpu.bgOn   0x01
                .|. cf gpu.bgMap1 0x08
                .|. cf gpu.bgSet1 0x10
                .|. cf gpu.dispOn 0x80
 where cf bool flag = if bool then flag else 0x00

cleanGpu :: Gpu
cleanGpu =
  { mTimer : 0
  , dispOn : false
  , bgOn : false
  , bgMap1 : false
  , bgSet1 : false
  , scrBuf : S.empty 
  , currLine : 0
  , currPos : 0 
  , yScroll : 0
  , xScroll : 0
  , palette : S.fromFoldable $ A.replicate 4 cleanColor
  , mode : OamScan
  , tiles : S.fromFoldable $ A.replicate 384 cleanTile
  , regs : S.fromFoldable $ A.replicate 0x40 0
  , vram : S.fromFoldable $ A.replicate 8192 0
  , oam  : S.fromFoldable $ A.replicate 160 0
  }

cleanTile :: Tile
cleanTile = Tile $ S.fromFoldable $ A.replicate 64 0

cleanColor :: Color
cleanColor = {a:0,r:0,g:0,b:0}

gpuStep :: forall e. I8 -> Gpu -> Eff (canvas :: Canvas | e) Gpu
gpuStep mReg gpu@{mTimer,mode,currLine,currPos,scrBuf} = gpu'
 where
  mTimer' = mTimer + mReg

  gpu' = if mTimer' < modeDuration mode
    then return gpu { mTimer = mTimer' }
    else case mode of 
      HBlank -> do --TODO set True least significant interrupt flags bit
        when ((trs "currLine" currLine) == pixHeight - 1)
          $ setScreen (seqToArray scrBuf) =<< getCanvas
        return gpu { mTimer = 0
                   , scrBuf = S.empty :: S.Seq I8 --psc forces me here
                   , currLine = currLine + 1
                   , currPos = currPos + bytesWidth
                   , mode = if currLine == pixHeight - 1 then VBlank
                                                         else OamScan
                   }  
      VBlank -> do
        let setOnLastLine =
              if currLine == pixHeight + 10 - 1
                -- 10 increments after last line of 143
                then _ { mTimer = 0
                       , currLine = 0
                       , currPos = 0
                       , mode = OamScan
                       }
                else id
        return $ setOnLastLine gpu { mTimer = 0 , currLine = (trs "vbl" (currLine + 1)) }
      OamScan -> return gpu { mTimer = 0, mode = VramScan }
      VramScan -> return $ renderLine gpu { mTimer = 0, mode = HBlank }

