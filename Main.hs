import NixHost

main :: IO ()
main = do
  input <- getInputValue
  input <- intoNixValue input
  
  
  makeNixString (show input) >>= return_to_nix
