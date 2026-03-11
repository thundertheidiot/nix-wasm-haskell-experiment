{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module NixWasm.Lib where

import NixWasm.Foreign

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Environment (getArgs)
import Data.Int (Int64)

import Data.List (intersectBy, singleton)
import Data.Containers.ListUtils (nubOrdOn)

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

type NixAttr = (String, NixValue)

data NixValue = NixInt (Int64)
              | NixFloat (Double)
              | NixBool (Bool)
              | NixString (String)
              | NixPath (ValueId, String)
              | NixNull
              | NixAttrset ([NixAttr])
              | NixList ([NixValue])
              | NixFunction (ValueId)
              | NixRawId (ValueId)
              deriving Show

nixBool :: NixValue -> Bool
nixBool (NixBool b) = b
nixBool _ = error "Expected a boolean"

nixList :: ValueId -> IO [ValueId]
nixList vid = do
  len <- nix_copy_list vid nullPtr 0
  allocaArray (fromIntegral len) $ \ptr -> do
    _ <- nix_copy_list vid ptr len
    list <- peekArray (fromIntegral len) ptr
    pure list

lazyNixValue :: ValueId -> IO NixValue
lazyNixValue vid = do
  getType vid >>= \case
    TNixList -> NixList <$> map NixRawId <$> nixList vid
    TNixAttrset -> NixAttrset <$> lazyAttrSet vid
    _ -> pure $ NixRawId vid

attrName :: ValueId -> (Int, Word32) -> IO String
attrName id (i, len) = do
  allocaBytes (fromIntegral len)$ \ptr -> do
    _ <- nix_copy_attrname id (fromIntegral i) ptr (fromIntegral len)
    peekCStringLen (castPtr ptr, fromIntegral len)

lazyAttrSet :: ValueId -> IO [(String, NixValue)]
lazyAttrSet vid = do
      len <- nix_copy_attrset vid nullPtr 0
      allocaArray (fromIntegral len) $ \ptr -> do
        _ <- nix_copy_attrset vid ptr len
        list <- peekArray (fromIntegral len) ptr
        names <- mapM (attrName vid) $ zip [0..] (map attrNameLen list)
        
        return $ zip names $ map (NixRawId . attrValueId) list

intoNixValue :: ValueId -> IO NixValue
intoNixValue id = do
  getType id >>= \case
    TNixInt -> NixInt <$> nix_get_int id
    TNixFloat -> NixFloat . realToFrac <$> nix_get_float id
    TNixBool -> NixBool . toBool <$> nix_get_bool id
    TNixString -> NixString <$> toString id 
    TNixPath -> NixPath <$> toPath id
    TNixNull -> pure NixNull
    TNixAttrset -> NixAttrset <$> toAttrset id
    TNixList -> NixList <$> toList id
    TNixFunction -> pure $ NixFunction id
    _ -> pure $ NixRawId id
  where
    toBool 0 = False
    toBool _ = True

    copyStringLike :: (ValueId -> Ptr Word32 -> Word32 -> IO Word32) -> ValueId -> IO String
    copyStringLike copy id = do
      len <- copy id nullPtr 0
      allocaBytes (fromIntegral len) $ \ptr -> do
        _ <- copy id ptr len
        peekCStringLen (castPtr ptr, fromIntegral len)

    toList :: ValueId -> IO [NixValue]
    toList vid = do
      list <- nixList vid
      mapM intoNixValue list

    toString = copyStringLike nix_copy_string

    toPath :: ValueId -> IO (ValueId, String)
    toPath vid = (vid,) <$> copyStringLike nix_copy_path vid

    toAttrset :: ValueId -> IO [NixAttr]
    toAttrset id = do
      len <- nix_copy_attrset id nullPtr 0
      allocaArray (fromIntegral len) $ \ptr -> do
        _ <- nix_copy_attrset id ptr len
        list <- peekArray (fromIntegral len) ptr
        names <- mapM (attrName id) $ zip [0..] (map attrNameLen list)
        values <- mapM (intoNixValue . attrValueId) list
        
        return $ zip names values

class ToNix a where
  toNix :: a -> NixValue

instance {-# OVERLAPPING #-} ToNix Integer where toNix = NixInt . fromIntegral
instance ToNix Double where toNix = NixFloat . realToFrac
instance ToNix Bool where toNix = NixBool
instance {-# OVERLAPPING #-} ToNix String where toNix = NixString
instance ToNix a => ToNix [a] where
  toNix = NixList . map toNix
instance ToNix a => ToNix [(String, a)] where
  toNix = NixAttrset . map (\(k, v) -> (k, toNix v))
instance ToNix a => ToNix (String, a) where
  toNix = NixAttrset . singleton . attr' 
instance ToNix ValueId where toNix = NixRawId
instance ToNix NixValue where toNix = id

fromNixValue :: ToNix a => a -> IO ValueId
fromNixValue value =
  case toNix value of
    NixInt int -> nix_make_int (fromIntegral int)
    NixFloat float -> nix_make_float $ realToFrac float
    NixBool bool -> nix_make_bool $ if bool then 1 else 0
    NixString string -> makeNixString string
    NixPath p -> pure $ fst p
    NixNull -> nix_make_null
    NixAttrset s -> makeNixAttrset s
    NixList l -> makeNixList l
    NixFunction id -> pure id
    NixRawId id -> pure id

makeNixString :: String -> IO ValueId
makeNixString s = withCStringLen s $ \(ptr, len) ->
  nix_make_string (castPtr ptr) (fromIntegral len)

makeNixList :: [NixValue] -> IO ValueId
makeNixList values =
  mapM fromNixValue values >>= newArray >>= (flip nix_make_list) (fromIntegral $ length values)

getString :: NixValue -> String
getString (NixString s) = s
getString _ = error "Expected string"

getList :: NixValue -> [NixValue]
getList (NixList l) = l
getList _ = error "Expected list"

(|++) :: ToNix a => ToNix b => [a] -> [b] -> [NixValue]
a |++ b = map toNix a ++ map toNix b

-- attrsets

makeNixAttr :: (String, NixValue) -> IO FFINixAttr
makeNixAttr (k, v) = do
  withCStringLen k $ \(ptr, len) -> do
    val <- fromNixValue v
    return $ NixAttr (castPtr ptr) (fromIntegral len) val

makeNixAttrset :: [NixAttr] -> IO ValueId
makeNixAttrset set =
  mapM makeNixAttr set >>= newArray >>= (flip nix_make_attrset) (fromIntegral $ length set)

getAttr :: ValueId -> String -> IO ValueId
getAttr vid key =
  withCStringLen key $ \(ptr, len) -> do
    nix_get_attr vid (castPtr ptr) (fromIntegral len)

class NixGetAttrs a where
  (*.) :: a -> String -> IO ValueId 
  (**.) :: a -> String -> IO NixValue -- lazy
  (***.) :: a -> String -> IO NixValue -- eager

infixl 8 *.
infixl 8 **.
infixl 8 ***.

instance NixGetAttrs ValueId where
  vid *. key =
    getAttr vid key

  vid **. key =
    getAttr vid key >>= lazyNixValue

  vid ***. key =
    getAttr vid key >>= intoNixValue

instance NixGetAttrs (IO ValueId) where
  vid *. key = do
    vid <- vid
    getAttr vid key

  vid **. key = do
    vid <- vid
    getAttr vid key >>= lazyNixValue

  vid ***. key = do
    vid <- vid
    getAttr vid key >>= intoNixValue

instance NixGetAttrs NixValue where
  val *. key =
    fromNixValue val >>= flip getAttr key

  val **. key =
    fromNixValue val >>= flip getAttr key >>= lazyNixValue

  val ***. key =
    fromNixValue val >>= flip getAttr key >>= intoNixValue

attr :: ToNix a => String -> a -> NixAttr
attr k v = (k, toNix v)

attr' :: ToNix a => (String, a) -> NixAttr
attr' (k, v) = attr k v
    
attrs :: ToNix a => [(String, a)] -> NixValue
attrs = NixAttrset . map attr'

mAttrs :: ToNix a => [(String, a)] -> NixValue
mAttrs = foldl mergeNix (NixAttrset []) . map (\a -> NixAttrset [a]) . map attr'

emptyAttrs :: NixValue
emptyAttrs = NixAttrset []

(|.) :: ToNix a => String -> a -> NixAttr
key |. value = attr key value 

infixr 9 |.

(|.++) :: ToNix a => ToNix b => [(String, a)] -> [(String, b)] -> [NixAttr]
a |.++ b =
  map (\(k, v) -> (k, toNix v)) a ++ map (\(k, v) -> (k, toNix v)) b

class MergeAttrs a b where
  (//) :: a -> b -> NixValue
  (///) :: a -> b -> NixValue

infixl 8 //
infixl 8 ///

dedupe :: Ord a => Eq a => [(a, b)] -> [(a, b)]
dedupe = nubOrdOn fst

mergeNix :: NixValue -> NixValue -> NixValue
mergeNix (NixAttrset left) (NixAttrset right) =
  deepMerge left right
mergeNix (NixList left) (NixList right) =
  NixList $ left ++ right
mergeNix _ right = right

joinNixString' :: NixValue -> NixValue -> NixValue
joinNixString' (NixString l) (NixString r) =
  NixString $ l ++ r
joinNixString' _ _ = error "Can only join strings"

joinNixString :: ToNix a => ToNix b => a -> b -> NixValue
joinNixString l r = joinNixString' (toNix l) (toNix r)

deepMerge :: [NixAttr] -> [NixAttr] -> NixValue
deepMerge left right =
  NixAttrset $ dedupe $ joined ++ left ++ right
  where
    intersecting =
      [ (k, l, r) | (k, l) <- left, (k', r) <- right, k == k']

    joined =
      [ (k, mergeNix l r) | (k, l, r) <- intersecting ]

instance (ToNix a, ToNix b) => MergeAttrs [(String, a)] [(String, b)] where
  left // right = NixAttrset $ dedupe $ map attr' left ++ map attr' right
  left /// right = deepMerge (map attr' left) (map attr' right)

instance MergeAttrs NixValue NixValue where
  (NixAttrset left) // (NixAttrset right) = left // right 
  _ // _ = error "Can only merge attrsets"

  (NixAttrset left) /// (NixAttrset right) = left /// right 
  _ /// _ = error "Can only merge attrsets"

instance MergeAttrs NixAttr NixAttr where
  left // right = [left] // [right]
  left /// right = [left] /// [right]

instance MergeAttrs NixValue NixAttr where
  (NixAttrset left) // right = left // [right]
  _ // _ = error "Can only merge attrsets"

  (NixAttrset left) /// right = left /// [right]
  _ /// _ = error "Can only merge attrsets"

instance ToNix a => MergeAttrs NixValue [(String, a)] where
  (NixAttrset left) // right = left // right 
  _ // _ = error "Can only merge attrsets"

  (NixAttrset left) /// right = left /// right 
  _ /// _ = error "Can only merge attrsets"

instance ToNix a => MergeAttrs [(String, a)] NixValue where
  left // (NixAttrset right) = left // right 
  _ // _ = error "Can only merge attrsets"

  left /// (NixAttrset right) = left /// right 
  _ /// _ = error "Can only merge attrsets"

-- paths

makeNixPath :: NixValue -> String -> IO NixValue
makeNixPath p s = withCStringLen s $ \(ptr, len) -> do
  vid <- fromNixValue p
  nix_make_path vid (castPtr ptr) (fromIntegral len) >>= intoNixValue


-- functions

call :: ToNix a => ValueId -> a -> IO ValueId
call fun arg = do
  array <- mapM id [fromNixValue arg] >>= newArray
  nix_call_function fun array 1

class NixCallable f where
  ($$) :: ToNix a => f -> a -> IO ValueId
  ($$$) :: ToNix a => f -> a -> IO NixValue
  
infixl 8 $$
infixl 8 $$$

instance NixCallable ValueId where
  funId $$ arg =
    call funId arg

  funId $$$ arg =
    call funId arg >>= intoNixValue

instance NixCallable (IO ValueId) where
  funId $$ arg = do
    funId <- funId
    call funId arg

  funId $$$ arg = do
    funId <- funId
    call funId arg >>= intoNixValue

instance NixCallable NixValue where
  val $$ arg = do
    funId <- fromNixValue val
    call funId arg

  val $$$ arg = do
    funId <- fromNixValue val
    call funId arg >>= intoNixValue

instance NixCallable (IO NixValue) where
  ioVal $$ arg = do
    val <- ioVal
    funId <- fromNixValue val
    call funId arg

  ioVal $$$ arg = do
    val <- ioVal
    funId <- fromNixValue val
    call funId arg >>= intoNixValue

nixReturn :: ToNix a => a -> IO ()
nixReturn v = fromNixValue v >>= return_to_nix

getInputValue :: IO ValueId
getInputValue = do
  args <- getArgs
  case args of
    (v:_) -> return (read v)
    _     -> error "No"

nixWarn :: String -> IO ()
nixWarn s =
  withCStringLen s $ \(ptr, len) ->
    nix_warn (castPtr ptr) (fromIntegral len)
