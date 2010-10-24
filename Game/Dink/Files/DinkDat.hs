{-# LANGUAGE FlexibleInstances #-}

module Game.Dink.Files.DinkDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.List

import qualified Data.Array.IArray as A

getDinkDat :: FilePath -> IO (DinkDat Int)
getDinkDat = liftM decode . BS.readFile

newtype DinkDat a = DinkDat (A.Array Int (ScreenInfo a))

data ScreenInfo a = ScreenInfo { screenMusic :: Int
                               , screenIndoor :: Bool
                               , screenData :: Maybe a
                               }

instance Functor ScreenInfo where
  fmap f si = si{screenData = fmap f (screenData si)}

instance Functor DinkDat where
  fmap f (DinkDat arr) = DinkDat (fmap (fmap f) arr)

mkScreenInfo :: Int -> Int -> Bool -> ScreenInfo Int
mkScreenInfo mapnum music indoor = ScreenInfo music indoor (if mapnum == 0 then Nothing else Just mapnum)

getMusics :: DinkDat Int -> [(Int,Int)]
getMusics (DinkDat arr) = nub $ map (screenData . snd) $ A.assocs arr

instance Binary (DinkDat Int) where
  get = do
    skip 24
    exists <- allScreenGet getExists
    skip 4
    musics <- allScreenGet getMusic
    skip 4
    insides <- allScreenGet getInside
    skip (40*4)
    skip 80
    skip 2000
    return (mkDinkDat exists musics insides)
  put dinkdat = do
    let info = map snd (elems dinkdat)
    junk 24
    allScreenPut putExist (map screenData info)
    junk 4
    allScreenPut putMusic (map screenMusic info)
    junk 4
    allScreenPut putInside (map screenIndoor info)
    junk (40*4)
    junk 80
    junk 2000

allScreenGet :: Get a -> Get [a]
allScreenGet = replicateM 768

getExists :: Get Int
getExists = do
  this <- getWord32le
  return (fromIntegral this)

getMusic :: Get Int
getMusic = do
  this <- getWord32le
  return (fromIntegral this)

getInside :: Get Bool
getInside = do
  this <- getWord32le
  if this == 0 then return False
               else return True

junk :: Int -> Put
junk n = replicateM_ n (putWord8 0)

allScreenPut :: (a -> Put) -> [a] -> Put
allScreenPut = mapM_

putExist :: Int -> Put
putExist = putWord32le . fromIntegral

putMusic :: Int -> Put
putMusic = putWord32le . fromIntegral

putInside :: Bool -> Put
putInside True = putWord32le 1
putInside False = putWord32le 0

mkDinkDat :: [Int] -> [Int] -> [Bool] -> DinkDat Int
mkDinkDat exists musics insides = DinkDat (A.array (1,768) infos)
  where infos = zip [1..] $ zipWith3 mkInfo exists musics insides
        mkInfo exist music inside = ScreenInfo music inside exist

(!) :: DinkDat a -> Int -> ScreenInfo a
(DinkDat arr) ! i = arr A.! i

elems :: DinkDat a -> [(Int, ScreenInfo a)]
elems (DinkDat arr) = A.assocs arr

