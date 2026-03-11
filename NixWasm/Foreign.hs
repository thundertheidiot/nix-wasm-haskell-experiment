module NixWasm.Foreign where

import Data.Word
import Foreign.Ptr
import Data.Int (Int64)
import Foreign.C.Types 
import Foreign.Storable

type ValueId = Word32

foreign import ccall "panic" nix_panic :: Ptr Word32 -> Word32 -> IO ()
foreign import ccall "warn" nix_warn :: Ptr Word32 -> Word32 -> IO ()

foreign import ccall "get_type" nix_get_type :: ValueId -> IO Word32

foreign import ccall "make_int" nix_make_int :: Word64 -> IO ValueId
foreign import ccall "get_int" nix_get_int :: ValueId -> IO Int64

foreign import ccall "make_float" nix_make_float :: CDouble -> IO ValueId
foreign import ccall "get_float" nix_get_float :: ValueId -> IO CDouble

foreign import ccall "make_bool" nix_make_bool :: Word32 -> IO ValueId
foreign import ccall "get_bool" nix_get_bool :: ValueId -> IO Word32

foreign import ccall "make_null"  nix_make_null :: IO ValueId

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
foreign import ccall "make_attrset" nix_make_attrset :: Ptr FFINixAttr -> Word32 -> IO ValueId
--                                                      value      ptr       max_len   len
foreign import ccall "copy_attrset" nix_copy_attrset :: ValueId -> Ptr FFINixCopiedAttr -> Word32 -> IO Word32
--                                                        value        attr_idx  ptr       max_len 
foreign import ccall "copy_attrname" nix_copy_attrname :: ValueId -> Word32 -> Ptr Word32 -> Word32 -> IO ()

--                                              value      ptr       str_len 
foreign import ccall "get_attr" nix_get_attr :: ValueId -> Ptr Word32 -> Word32 -> IO ValueId

foreign import ccall "call_function" nix_call_function :: ValueId -> Ptr ValueId -> Word32 -> IO ValueId

--                                              fu         ptr       len 
foreign import ccall "make_app" nix_make_app :: ValueId -> Ptr ValueId -> Word32 -> IO ValueId

foreign import ccall "return_to_nix" return_to_nix :: ValueId -> IO ()

foreign import ccall "read_file" nix_read_file :: ValueId -> Ptr Word32 -> Word32 -> IO Word32

data FFINixCopiedAttr = NixCopiedAttr
  { attrValueId :: ValueId
  , attrNameLen :: Word32
  }

instance Storable FFINixCopiedAttr where
  sizeOf _ = 8
  alignment _ = 8

  peek ptr = do
    id <- peekByteOff ptr 0
    len <- peekByteOff ptr 4
    return $ NixCopiedAttr id len

data FFINixAttr = NixAttr
  { namePtr :: Ptr Word32
  , nameLen :: Word32
  , valueId :: ValueId
  }

instance Storable FFINixAttr where
  sizeOf _ = 12
  alignment _ = 12

  peek ptr = do
    name <- peekByteOff ptr 0
    nameLen <- peekByteOff ptr 4
    valueId <- peekByteOff ptr 8

    return (NixAttr name nameLen valueId)

  poke ptr val = do
    pokeByteOff ptr 0 (namePtr val)
    pokeByteOff ptr 4 (nameLen val)
    pokeByteOff ptr 8 (valueId val)
