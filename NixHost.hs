{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module NixHost where

import Data.Word
import Data.Map
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.Environment (getArgs)
import Data.Int (Int64)

type ValueId = Word32

foreign import ccall "panic" nix_panic :: Ptr () -> Word32 -> IO ()
foreign import ccall "warn" nix_warn :: Ptr () -> Word32 -> IO ()

foreign import ccall "get_type" nix_get_type :: ValueId -> IO Word32

foreign import ccall "make_int" nix_make_int :: Word64 -> IO ValueId
foreign import ccall "get_int" nix_get_int :: ValueId -> IO Int64

foreign import ccall "make_float" nix_make_float :: Word64 -> IO ValueId
foreign import ccall "get_float" nix_get_float :: ValueId -> IO CDouble

foreign import ccall "make_bool" nix_make_bool :: Word32 -> IO ValueId
foreign import ccall "get_bool" nix_get_bool :: ValueId -> IO Word32

foreign import ccall "make_string" nix_make_string :: Ptr Word32 -> Word32 -> IO ValueId
--                                                    value      ptr       max_len   len
foreign import ccall "copy_string" nix_copy_string :: ValueId -> Ptr Word32 -> Word32 -> IO Word32

--                                                value      ptr       max_len   id
foreign import ccall "make_path" nix_make_path :: ValueId -> Ptr Word32 -> Word32 -> IO ValueId
--                                                value      ptr       max_len   len
foreign import ccall "copy_path" nix_copy_path :: ValueId -> Ptr Word32 -> Word32 -> IO Word32

--                                                ptr       len       id
foreign import ccall "make_list" nix_make_list :: Ptr Word32 -> Word32 -> IO ValueId
--                                                value      ptr       max_len   len
foreign import ccall "copy_list" nix_copy_list :: ValueId -> Ptr Word32 -> Word32 -> IO Word32

--                                                      ptr       len       id
foreign import ccall "make_attrset" nix_make_attrset :: Ptr Word32 -> Word32 -> IO ValueId
--                                                      value      ptr       max_len   len
foreign import ccall "copy_attrset" nix_copy_attrset :: ValueId -> Ptr NixAttr -> Word32 -> IO Word32
--                                                        value        attr_idx  ptr       max_len 
foreign import ccall "copy_attrname" nix_copy_attrname :: ValueId -> Word32 -> Ptr Word32 -> Word32 -> IO ()

--                                              value      ptr       str_len 
foreign import ccall "get_attr" nix_get_attr :: ValueId -> Word32 -> Ptr Word32 -> IO ValueId

--                                              fu         ptr       len 
foreign import ccall "make_app" nix_make_app :: ValueId -> Ptr Word32 -> Word32 -> IO ValueId

foreign import ccall "return_to_nix" return_to_nix :: ValueId -> IO ()

foreign import ccall "read_file" nix_read_file :: ValueId -> Ptr Word32 -> Word32 -> IO Word32

data NixType = TNixInt | TNixFloat | TNixBool | TNixString | TNixPath | TNixNull | TNixAttrset | TNixList | TNixFunction | Invalid
  deriving (Show,Eq)

getType' :: Word32 -> NixType
getType' 1 = TNixInt
getType' 2 = TNixFloat
getType' 3 = TNixBool
getType' 4 = TNixString
getType' 5 = TNixPath
getType' 6 = TNixNull
getType' 7 = TNixAttrset
getType' 8 = TNixList
getType' 9 = TNixFunction
getType' _ = error "Invalid type"

getType :: ValueId -> IO NixType
getType id = nix_get_type id >>= pure . getType'

data NixAttr = NixAttr
  { attrValueId :: ValueId
  , attrNameLen :: Word32
  }

instance Storable NixAttr where
  sizeOf _ = 8
  alignment _ = 8

  peek ptr = do
    id <- peekByteOff ptr 0
    len <- peekByteOff ptr 4
    return $ NixAttr id len

data NixValue = NixInt (Int64)
              | NixFloat (Double)
              | NixBool (Bool)
              | NixString (String)
              | NixPath (String)
              | NixNull
              | NixAttrset (Map String NixValue)
              | NixList ([NixValue])
              | NixFunction (ValueId)
              deriving (Show)

intoNixValue :: ValueId -> IO NixValue
intoNixValue id = do
  getType id >>= \case
    TNixInt -> NixInt <$> nix_get_int id
    TNixFloat -> NixFloat . realToFrac <$> nix_get_float id
    TNixBool -> NixBool . toBool <$> nix_get_bool id
    TNixString -> NixString <$> toString id 
    TNixPath -> NixPath <$> toPath id
    TNixNull -> return NixNull
    TNixAttrset -> NixAttrset <$> toAttrset id
    TNixList -> NixList <$> toList id
    TNixFunction -> return $ NixFunction id
  where
    toBool 0 = False
    toBool _ = True

    copyStringLike :: (ValueId -> Ptr Word32 -> Word32 -> IO Word32) -> ValueId -> IO String
    copyStringLike copy id = do
      len <- copy id nullPtr 0
      allocaBytes (fromIntegral len) $ \ptr -> do
        _ <- copy id ptr len
        peekCStringLen (castPtr ptr, fromIntegral len)

    toString = copyStringLike nix_copy_string
    toPath = copyStringLike nix_copy_path

    toList id = do
      len <- nix_copy_list id nullPtr 0
      allocaArray (fromIntegral len) $ \ptr -> do
        _ <- nix_copy_list id ptr len
        list <- peekArray (fromIntegral len) ptr
        mapM intoNixValue list

    toKVP :: ValueId -> (Int, Word32) -> IO String
    toKVP id (i, len) = do
      allocaBytes (fromIntegral len)$ \ptr -> do
        _ <- nix_copy_attrname id (fromIntegral i) ptr (fromIntegral len)
        peekCStringLen (castPtr ptr, fromIntegral len)

    toAttrset :: ValueId -> IO (Map String NixValue)
    toAttrset id = do
      len <- nix_copy_attrset id nullPtr 0
      allocaArray (fromIntegral len) $ \ptr -> do
        _ <- nix_copy_attrset id ptr len
        list <- peekArray (fromIntegral len) ptr
        names <- mapM (toKVP id) $ zip [0..] (Prelude.map attrNameLen list)
        values <- mapM (intoNixValue . attrValueId) list

        return $ fromList $ zip names values

makeNixString :: String -> IO ValueId
makeNixString s = withCStringLen s $ \(ptr, len) ->
  nix_make_string (castPtr ptr) (fromIntegral len)

getInputValue :: IO ValueId
getInputValue = do
  args <- getArgs
  case args of
    (v:_) -> return (read v)
    _     -> error "No"

