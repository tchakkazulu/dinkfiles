module CStructUtil where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Char

getInt :: Get Int
getInt = fmap convert getWord32le
  where convert :: Word32 -> Int
        convert = fromIntegral

getBool :: Get Bool
getBool = fmap convert getWord8
  where convert :: Word8 -> Bool
        convert 0 = False
        convert _ = True

getString :: Int -> Get String
getString = fmap (takeWhile (/='\NUL') . map (chr . fromIntegral) . BS.unpack) . getLazyByteString . fromIntegral


putInt :: Int -> Put
putInt = putWord32le . convert
  where convert :: Int -> Word32
        convert = fromIntegral

putBool :: Bool -> Put
putBool = putWord8 . convert
  where convert :: Bool -> Word8
        convert True = 1
        convert False = 0

putString :: Int -> String -> Put
putString num string = putLazyByteString . BS.pack . map (fromIntegral . ord) . take num $ string ++ repeat '\NUL'

junk :: Int -> Put
junk n = replicateM_ n (putWord8 0)

