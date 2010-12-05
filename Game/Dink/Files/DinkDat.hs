{-# LANGUAGE FlexibleInstances #-}

module Game.Dink.Files.DinkDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.List

import qualified Data.Array.IArray as A

import CStructUtil

getDinkDat :: FilePath -> IO (DinkDat Int)
getDinkDat = liftM decode . BS.readFile

newtype DinkDat a = DinkDat (A.Array Int (ScreenInfo a))
  deriving (Eq)

data ScreenInfo a = ScreenInfo { screenMusic :: Int
                               , screenIndoor :: Bool
                               , screenData :: Maybe a
                               }
  deriving (Eq)

instance Functor ScreenInfo where
  fmap f si = si{screenData = fmap f (screenData si)}

instance Functor DinkDat where
  fmap f (DinkDat arr) = DinkDat (fmap (fmap f) arr)

mkScreenInfo :: Int -> Int -> Bool -> ScreenInfo Int
mkScreenInfo mapnum music indoor = ScreenInfo music indoor (if mapnum == 0 then Nothing else Just mapnum)

getMusics :: DinkDat a -> [Int]
getMusics (DinkDat arr) = nub $ map screenMusic $ A.elems arr

instance Binary (DinkDat Int) where
  get = do
    skip 20 -- unused name field
    skip 4 -- exists[0]. Seth hates 0.
    exists <- allScreenGet getInt
    skip 4 -- music[0]. Seth hates 0.
    musics <- allScreenGet getInt
    skip 4 -- indoor[0]. Seth hates 0.
    insides <- allScreenGet getInside
    skip (40*4) -- unused v field
    skip 80     -- unused s field
    skip 2000
    return (mkDinkDat exists musics insides)
  put dinkdat = do
    let info = map snd (elems dinkdat)
    junk 20
    junk 4
    mapM_  putExists (map screenData info)
    junk 4
    mapM_ putInt (map screenMusic info)
    junk 4
    mapM_ putInside (map screenIndoor info)
    junk (40*4)
    junk 80
    junk 2000

allScreenGet :: Get a -> Get [a]
allScreenGet = replicateM 768


-- special, because int and not bool.
-- Makes the functions from CStructUtil useless.
getInside :: Get Bool
getInside = do
  this <- getWord32le
  if this == 0 then return False
               else return True

putInside :: Bool -> Put
putInside True = putWord32le 1
putInside False = putWord32le 0

putExists :: Maybe Int -> Put
putExists Nothing = putWord32le 0
putExists (Just x) = putInt x

mkDinkDat :: [Int] -> [Int] -> [Bool] -> DinkDat Int
mkDinkDat exists musics insides = DinkDat (A.array (1,768) infos)
  where infos = zip [1..] $ zipWith3 mkScreenInfo exists musics insides

(!) :: DinkDat a -> Int -> ScreenInfo a
(DinkDat arr) ! i = arr A.! i

elems :: DinkDat a -> [(Int, ScreenInfo a)]
elems (DinkDat arr) = A.assocs arr

