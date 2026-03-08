import NixHost

import qualified Data.Map.Strict as M

transform :: NixValue -> NixValue
transform (NixInt i) = NixInt $ i * 2
transform (NixFloat i) = NixFloat $ i * 2
transform (NixList l) = NixList $ map transform l
transform (NixAttrset s) = NixAttrset $ M.map transform s
transform a = a

main :: IO ()
main = do
  input <- getInputValue
  input <- intoNixValue input

  -- value <- fromNixValue $ transform input

  -- value <- fromNixValue $ ([1, 2, 3] :: [Integer])
  --          |++ ["hi", "hello"]
  --          |++ ["a" |. ("b" |. (1::Integer))]

  value <- fromNixValue $
           "a" |. (attrs $
                    [ ("a", (1::Integer))
                    , ("b", (2::Integer))
                    ] |.++
                    [ ("c", "String")
                    , ("d", "other string")
                    ] |.++
                    [ ("e", ("nested" |. "attr"))
                    ])

  return_to_nix value
