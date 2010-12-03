module Game.Dink.Files.SaveDat where

import Control.Monad

import qualified Data.ByteString.Lazy as BS

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Char

import qualified Data.Array.IArray as A
import qualified Data.Map as M

import CStructUtil

data DinkSave = Save { save_version :: Int
                     , save_info :: String
                     , save_minutes :: Int
                     , save_x :: Int
                     , save_y :: Int
                     , save_die :: Int
                     , save_size :: Int
                     , save_defense :: Int
                     , save_dir :: Int
                     , save_pframe :: Int
                     , save_pseq :: Int
                     , save_seq :: Int
                     , save_frame :: Int
                     , save_strength :: Int
                     , save_base_walk :: Int
                     , save_base_idle :: Int
                     , save_base_hit :: Int
                     , save_que :: Int
                     , save_magic :: A.Array Int Magic
                     , save_items :: A.Array Int Item
                     , save_spmap :: A.Array Int (A.Array Int EditorInfo, Int)  -- [769], [100]
                     , save_buttons :: A.Array Int Int
                     , save_varman :: M.Map String VarInfo
                     , save_last_talk :: Int
                     , save_mouse :: Int
                     , save_last_map :: Int
                     , save_mapdat :: String
                     , save_dinkdat :: String
                     , save_palette :: String
                     , save_tiles :: A.Array Int String
                     , save_funcs :: M.Map String Func
                     }
  deriving (Show)

instance Binary DinkSave where
  get = do
    version <- getInt
    info <- getString 196
    [min,x,y,die,size,def,dir,pfr,pseq,seq,fr,str,base_walk,base_idle,base_hit,que] <- replicateM 16 getInt
    (_:magic') <- replicateM 9 get
    (_:items') <- replicateM 17 get
    let magic = A.array (1,8) $ zip [1..] magic'
        items = A.array (1,16) $ zip [1..] items'
    skip (3*4)
    spMap <- getSpMap
    button' <- replicateM 10 getInt
    let buttons = A.array (1,10) $ zip [1..] button'
    varman <- getVarMan
    skip (3*4)
    [last_talk,mouse] <- replicateM 2 getInt
    skip 4
    last_map <- getInt
    mapdat <- getString 50
    dinkdat <- getString 50
    palette <- getString 50
    tiles' <- replicateM 42 $ getString 50
    let tiles = A.array (1,42) $ zip [1..] tiles'
    funcs <- getFuncs
    skip 750
    return $ Save version info min x y die size def dir pfr pseq seq fr str base_walk base_idle base_hit que magic items spMap buttons varman last_talk mouse last_map mapdat dinkdat palette tiles funcs

data EditorInfo = EditorInfo { editor_type :: Int    -- in-file as char
                             , editor_seq :: Int     -- in-file as short
                             , editor_frame :: Int   -- in-file as char
                             }
  deriving (Show)

getSpMap :: Get (A.Array Int (A.Array Int EditorInfo, Int))
getSpMap = fmap (A.array (1,768) . zip [1..] . tail) $ replicateM 769 getEditorInfo


getEditorInfo :: Get (A.Array Int EditorInfo, Int)
getEditorInfo = do
  (_:types) <- fmap (map fromIntegral) $ replicateM 100 getWord8
  (_:seqs) <- fmap (map fromIntegral) $ replicateM 100 getWord16le
  (_:frames) <- fmap (map fromIntegral) $ replicateM 100 getWord8
  last <- getInt
  let arr = A.array (1,99) $ zip [1..] $ zipWith3 EditorInfo types seqs frames
  return (arr, last)

data VarInfo = VarInfo { var_val :: Int
                       , var_scope :: Int
                       , var_active :: Bool
                       }
  deriving (Show)

getVarMan :: Get (M.Map String VarInfo)
getVarMan = do
  vars <- replicateM 100 $ do
    val <- getInt
    name <- getString 20
    scope <- getInt
    active <- getInt
    return $ (name,VarInfo val scope (toBool active))
  return $ M.fromList vars

data Func = Func { func_file :: String
                 , func_func :: String
                 }
  deriving (Show)

getFuncs :: Get (M.Map String Func)
getFuncs = do
  funcs <- replicateM 100 $ do
    file <- getString 10
    func <- getString 20
    return (func,Func file func)
  return $ M.fromList funcs

type Magic = Item
data Item = Item { item_active :: Bool
                 , item_name :: String
                 , item_seq :: Int
                 , item_frame :: Int
                 }
  deriving (Show)

instance Binary Item where
  get = do
    act <- getInt
    name <- getString 10
    seq <- getInt
    fr <- getInt
    return $ Item (toBool act) name seq fr

toBool 0 = False
toBool _ = True

