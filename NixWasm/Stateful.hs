module NixWasm.Stateful where

import NixWasm.Lib
import NixWasm.UnsafeGet
import Control.Monad.State
import Distribution.Simple.Utils (safeHead)

data NixState = NixState { inputs :: NixValue -- NixRawId
                         , nixConfig :: NixValue -- NixRawId
                         , config :: NixValue -- [NixAttr] 
                         }
                
type Nix = StateT NixState IO

emptyState :: NixState
emptyState = NixState { inputs = NixRawId 0
                      , nixConfig = NixRawId 0
                      , config = NixAttrset []
                      }

getKey :: String -> [NixAttr] -> Maybe NixValue
getKey key attrs = safeHead $ map snd $ filter ((==key) . fst) attrs

instance NixGetAttrs NixState where
  state *. key = case getKey key $ unsafeAttrs $ config state of
    Just key -> fromNixValue key
    Nothing -> nixConfig state *. key
  
  state **. key = case getKey key $ unsafeAttrs $ config state of
    Just key -> pure key
    Nothing -> nixConfig state **. key
    
  state ***. key = case getKey key $ unsafeAttrs $ config state of
    Just key -> pure key
    Nothing -> nixConfig state ***. key

initializeNix :: String -> Nix ()
initializeNix hostPlatform = do
  inputs <- liftIO $ getInputValue

  modules <- liftIO $ inputs *. "evalModules"
             $$ [ attrs [ "nixpkgs" |. "hostPlatform" |. hostPlatform]
                ]
             
  config <- liftIO $ modules *. "config"

  put $ NixState { inputs = NixRawId inputs
                 , nixConfig = NixRawId config
                 , config = NixAttrset []
                 }

addConfig :: NixValue -> Nix ()
addConfig attrs = modify (\s -> s { config = config s /// unsafeAttrs attrs })
