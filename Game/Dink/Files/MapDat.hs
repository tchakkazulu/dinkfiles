module Game.Dink.Files.MapDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Int

import Data.Char

import qualified Data.Array.IArray as A
import Data.Ix

import Game.Dink.Files.HardDat

import CStructUtil

getMapDat :: FilePath -> IO MapDat
getMapDat = liftM decode . BS.readFile

getScreen :: FilePath -> Int -> IO DinkScreen
getScreen mapdat scr = do
  cont <- BS.readFile mapdat
  return $ flip runGet cont $ do
             skip (scr*31280)
             get

newtype MapDat = MapDat (A.Array Int DinkScreen)
  deriving (Eq)

instance Binary MapDat where
  get = do
    let loop = do
          done <- isEmpty
          if not done
            then do
              x <- get
              (xs,count) <- loop
              return $ count `seq` ((x:xs),count+1)
            else return ([],0)
    (screens, count) <- loop
    return $ mkMapDat screens count
  put (MapDat arr) = mapM_ put (A.elems arr)

mkMapDat :: [DinkScreen] -> Int -> MapDat
mkMapDat screens n = MapDat $ A.array (1,n) $ zip [1..n] screens

data DinkScreen = DinkScreen { scr_tiles :: A.Array (Int,Int) (Tile, Int)
                             , scr_sprites :: A.Array Int Sprite
                             , scr_script :: String
                             }
  deriving (Eq,Show)

-- A screen should be 31280 bytes
instance Binary DinkScreen where
  get = do
    skip 20
    (_:tiles) <- replicateM 97 getTile
    skip 160
    skip 80
    (_:sprites) <- replicateM 101 get
    script <- getString 13
    skip 13
    skip 13
    skip 1000
    skip 1 
    return $ mkDinkScreen tiles sprites script
  put (DinkScreen tiles sprites script) = do
    junk 20
    junk 80 -- 0-tile
    mapM_ putTile (A.elems tiles)
    junk 160
    junk 80
    junk 220 -- 0-sprite
    mapM_ put (A.elems sprites)
    putString 13 script
    junk (13+13+1000+1)

-- A tile should be 80 bytes
getTile :: Get (Tile,Int)
getTile = do
  t' <- getInt
  skip 4 -- property
  altHard <- getInt
  skip 4 -- more2
  skip 1 -- more3
  skip 1 -- more4
  skip 2 -- padding for alignment
  skip 60
  return (toTile' t', altHard)

putTile :: (Tile,Int) -> Put
putTile (tile, int) = do
  let num = fromTile' tile
  putInt num
  junk 4
  putInt int
  junk (4+1+1+2+60)

mkDinkScreen :: [(Tile,Int)] -> [Sprite] -> String -> DinkScreen
mkDinkScreen tiles sprites script = DinkScreen tileArr sprArr script
  where tileArr = A.array ((1,1),(12,8)) $ zip tileIndices tiles
        tileIndices = [(a,b) | a <- [1..12], b <- [1..8]]
        sprArr = A.array (1,100) $ zip [1..100] sprites


data Sprite = Sprite { sp_x :: Int
                     , sp_y :: Int
                     , sp_seq :: Int
                     , sp_frame :: Int
                     , sp_type :: Int
                     , sp_size :: Int
                     , sp_exists :: Bool          -- Throw this out later, or implement "undelete" feature?
                     , sp_rotation :: Int         -- Throw this out if it appears nothing uses it.
                     , sp_special :: Int
                     , sp_brain :: Int
                     , sp_script :: String
                     , sp_hit :: String           -- Throw out?
                     , sp_die :: String           -- Throw this out if nothing uses it.
                     , sp_talk :: String          -- Throw this out if nothing uses it.
                     , sp_speed :: Int
                     , sp_base_walk :: Int
                     , sp_base_idle :: Int
                     , sp_base_attack :: Int
                     , sp_timing :: Int
                     , sp_que :: Int
                     , sp_hard :: Bool
                     , sp_trim :: Trim
                     , sp_warp :: Maybe Warp
                     , sp_touch_seq :: Int
                     , sp_base_death :: Int
                     , sp_gold :: Int             -- Throw this out if nothing uses it.
                     , sp_hitpoints :: Int
                     , sp_strength :: Int
                     , sp_defense :: Int
                     , sp_exp :: Int
                     , sp_sound :: Int
                     , sp_vision :: Int
                     , sp_nohit :: Int
                     , sp_touch_damage :: Int
                     }
  deriving (Eq, Show)


-- A sprite should be 220 bytes
instance Binary Sprite where
  get = do
    [x,y,seq,frame,typ,size,exists',rot,special,brain] <- replicateM 10 getInt
    [script,script_hit,script_die,script_talk] <- replicateM 4 (getString 13)
    [speed,base_walk,base_idle,base_attack,base_hit,timing,que,hard'] <- replicateM 8 getInt
    trim <- get
    [has_warp,warpscreen,warpx,warpy] <- replicateM 4 getInt
    let warp = if has_warp == 0 then Nothing
                                else Just $ Warp warpscreen warpx warpy
    [touch_seq,base_death,gold,hitpoints,strength,defense,exp,sound,vision,nohit,touch_damage] <- replicateM 11 getInt
    skip 20
    return $ Sprite x y seq frame typ size (toBool exists') rot special brain script script_hit script_die script_talk speed base_walk base_idle base_attack timing que (toBool hard') trim warp touch_seq base_death gold hitpoints strength defense exp sound vision nohit touch_damage
  put spr = do
    mapM_ (putInt . ($spr)) $ [sp_x,sp_y,sp_seq,sp_frame,sp_type,sp_size, fromBool . sp_exists, sp_rotation, sp_special, sp_brain]
    mapM_ (putString 13 . ($spr)) $ [sp_script, sp_hit, sp_die, sp_talk]
    mapM_ (putInt . ($spr)) $ [sp_speed,sp_base_walk,sp_base_idle,sp_base_attack,const 0,sp_timing,sp_que,fromBool . sp_hard]
    put (sp_trim spr)
    mapM_ putInt $ case sp_warp spr of
                     Nothing           -> [0,0,0,0]
                     Just (Warp s x y) -> [1,s,x,y]
    mapM_ (putInt . ($spr)) $ [sp_touch_seq,sp_base_death,sp_gold,sp_hitpoints,sp_strength,sp_defense,sp_exp,sp_sound,sp_vision,sp_nohit,sp_touch_damage]
    junk 20

toBool 0 = False
toBool _ = True

fromBool False = 0
fromBool True = 1

data Trim = Trim { trim_left :: Int
                 , trim_top :: Int
                 , trim_right :: Int
                 , trim_bottom :: Int
                 }
  deriving (Eq,Show)

instance Binary Trim where
  get = do
    [tl,tt,tr,tb] <- replicateM 4 getInt
    return $ Trim tl tt tr tb
  put (Trim tl tt tr tb) =
    mapM_ putInt [tl,tt,tr,tb]

data Warp = Warp { warp_screen :: Int
                 , warp_x :: Int
                 , warp_y :: Int
                 }
  deriving (Eq,Show)

