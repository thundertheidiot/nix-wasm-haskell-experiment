module NixWasm.UnsafeGet where

import NixWasm.Lib
import NixWasm.Foreign (ValueId)
import Data.Int (Int64)

unsafeInt :: NixValue -> Int64
unsafeInt (NixInt i) = i
unsafeInt _ = error "Expected an integer"

unsafeFloat :: NixValue -> Double
unsafeFloat (NixFloat i) = i
unsafeFloat _ = error "Expected a float"

unsafeBool :: NixValue -> Bool
unsafeBool (NixBool b) = b
unsafeBool _ = error "Expected a boolean"

unsafeString :: NixValue -> String
unsafeString (NixString s) = s
unsafeString _ = error "Expected string"

unsafePath :: NixValue -> (ValueId, String)
unsafePath (NixPath s) = s
unsafePath _ = error "Expected path"

unsafeAttrs :: NixValue -> [NixAttr]
unsafeAttrs (NixAttrset a) = a
unsafeAttrs _ = error "Expected attrset"

unsafeList :: NixValue -> [NixValue]
unsafeList (NixList l) = l
unsafeList _ = error "Expected list"

unsafeFunction :: NixValue -> ValueId
unsafeFunction (NixFunction f) = f
unsafeFunction _ = error "Expected function"

unsafeRawId :: NixValue -> ValueId
unsafeRawId (NixRawId f) = f
unsafeRawId _ = error "Expected rawId"
