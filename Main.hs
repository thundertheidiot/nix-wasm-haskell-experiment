import NixWasm.Lib

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  -- value <- pure $ ([1, 2, 3] :: [Integer])
  --          |++ ["hi", "hello"]
  --          |++ [hello, mkDerivation]
  --          |++ ["a" |. (attrs $
  --                       [ ("key", "Value")
  --                       ])]
  
  -- value <- fromNixValue $
  --          "a" |. (attrs $
  --                   [ ("a", (1::Integer))
  --                   , ("b", (2::Integer))
  --                   ] |.++
  --                   [ ("c", "String")
  --                   , ("d", "other string")
  --                   ] |.++
  --                   [ ("e", ("nested" |. "attr"))
  --                   ])

  -- value <- call input [1::Integer, 2] >>= fromNixValue
  input <- getInputValue

  pkgs <- getAttr input "pkgs"

  mkDerivation <- pkgs *. "stdenv" *. "mkDerivation"

  hello <- let
    args = attrs
      $ [ ("name", "hello")
        , ("buildPhase", "echo hello from haskell > $out")
        ]
    in mkDerivation $$ args

  prosody <- pkgs *. "prosody"

  prosodyBuildInputs <- prosody **. "buildInputs"
  openssl <- pkgs *. "openssl"
  
  prosody <- prosody *. "overrideAttrs"
    $$ "buildInputs" |. (getList prosodyBuildInputs |++ [openssl])

  prosody <- prosody *. "override"
    $$ "withCommunityModules" |. ["http_altconnect", "http_health"] 
             
  
  nixReturn [hello, prosody]
