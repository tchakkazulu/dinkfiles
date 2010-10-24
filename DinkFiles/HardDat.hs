module DinkFiles.HardDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.Array.IArray as A
import Data.Ix

data HardType = No | Low | High | Mystery
  deriving (Eq, Show)

data Tile = Tile { page :: Int
                 , tile_x :: Int
                 , tile_y :: Int
                 }
  deriving (Eq, Show)

getHardDat :: FilePath -> IO HardDat
getHardDat = liftM decode . BS.readFile

tileStart = Tile 1 1 1
tileEnd = Tile 41 12 8

toTup :: Tile -> (Int, Int, Int)
toTup (Tile p x y) = (p-1, y-1, x-1)

fromTup :: (Int, Int, Int) -> Tile
fromTup (p,y,x) = Tile (p+1) (x+1) (y+1)

toTile' :: Int -> Tile
toTile' n = let (pg,coords) = n `divMod` 128
                (y,x)       = coords `divMod` 12
            in fromTup (pg,y,x)

fromTile' :: Tile -> Int
fromTile' t = let (p,y,x) = toTup t
              in 128*p + 12*y + x

instance Ord Tile where
  compare t1 t2 = compare (toTup t1) (toTup t2)

instance Ix Tile where
  range (a,b) = map fromTup $ range ((toTup a), (toTup b))
  index (a,b) c = index (toTup a, toTup b) (toTup c)
  inRange (a,b) c = inRange (toTup a, toTup b) (toTup c)
  rangeSize (a,b) = rangeSize (toTup a, toTup b)

newtype HardDat = HardDat (A.Array Int (A.Array (Int,Int) HardType), A.Array Tile Int)

instance Binary HardDat where
  get = do
    htypes <- replicateM 800 $ do
      theData <- replicateM (51*51) get
      skip 1
      skip 4
      return theData
    tilerefs <- replicateM 3936 $ fmap fromIntegral getWord32le
    skip (4*(8000-3936))
    return (mkHardDat htypes tilerefs)
  put (HardDat (htypes, tilerefs)) = do
    forM_ (A.elems htypes) $ \a -> do
      forM_ (A.elems a) put
      junk 1
      junk 4
    forM_ (A.elems tilerefs) put

junk :: Int -> Put
junk n = replicateM_ n (putWord8 0)

instance Binary HardType where
  get = toHardType `fmap` getWord8
  put = putWord8 . fromHardType

toHardType :: Word8 -> HardType
toHardType 0 = No
toHardType 1 = Low
toHardType 2 = High
toHardType 3 = Mystery
toHardType v = error $ "DinkFiles.HardDat.toHardType: " ++ show v

fromHardType :: HardType -> Word8
fromHardType No      = 0
fromHardType Low     = 1
fromHardType High    = 2
fromhardType Mystery = 3

mkHardDat :: [[HardType]] -> [Int] -> HardDat
mkHardDat htypes tilerefs = HardDat (A.array (1,800) htypes', A.array (tileStart,tileEnd) tilerefs')
  where htypes' = zip [1..800] $ map (A.array ((1,1),(51,51)) . zip indices) htypes
        indices = [(a,b) | a <- [1..51], b <- [1..51]]
        tilerefs' = zip (map toTile' [0..3935]) tilerefs

