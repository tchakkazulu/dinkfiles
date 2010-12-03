module Game.Dink.Files.HardDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Debug.Trace

import qualified Data.Array.IArray as A

import CStructUtil

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

instance A.Ix Tile where
  range (a,b) = map fromTup $ A.range ((toTup a), (toTup b))
  index (a,b) c = A.index (toTup a, toTup b) (toTup c)
  inRange (a,b) c = A.inRange (toTup a, toTup b) (toTup c)
  rangeSize (a,b) = A.rangeSize (toTup a, toTup b)

newtype HardDat = HardDat (A.Array Int (A.Array (Int,Int) HardType, Bool), A.Array Tile Int)
  deriving (Eq)

instance Binary HardDat where
  get = do
    htypes <- replicateM 800 $ do
      theData <- replicateM (51*51) get
      used <- getBool
      skip 2 -- for alignment padding
      skip 4 -- unused hold field
      return (theData, used)
    tilerefs <- replicateM 8000 $ getInt
    return (mkHardDat htypes tilerefs)
  put (HardDat (htypes, tilerefs)) = do
    forM_ (A.elems htypes) $ \(a,used) -> do
      forM_ (A.elems a) put
      putBool used
      junk 2 -- alignment padding
      junk 4 -- unused hold field
    forM_ [0..7999] $ \i -> if inRange (toTile' i) then putInt (tilerefs A.! toTile' i)
                                                   else putInt 0

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
fromHardType Mystery = 3

mkHardDat :: [([HardType],Bool)] -> [Int] -> HardDat
mkHardDat htypes tilerefs = HardDat (A.array (1,800) htypes', A.array (tileStart,tileEnd) tilerefs')
  where htypes' = zip [1..800] $ map (\(hts, used) -> (A.array ((1,1),(51,51)) . zip indices $ hts,used)) htypes
        indices = [(a,b) | a <- [1..51], b <- [1..51]]
        tilerefs' = filter (inRange . fst) $ zip (map toTile' [0..7999]) tilerefs

inRange :: Tile -> Bool
inRange = A.inRange (tileStart, tileEnd)

