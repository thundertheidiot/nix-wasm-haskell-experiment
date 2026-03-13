module NixWasm.NixOS where

import NixWasm.Lib
import NixWasm.UnsafeGet
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad.State
import Distribution.Simple.Utils (safeHead)

data NixOSState = NixOSState { inputs :: NixValue -- NixRawId
                         , pkgs :: NixValue -- NixRawId
                         , nixOverlayConfig :: NixValue -- NixRawId
                         , haskellConfig :: NixValue -- [NixAttr]
                         }

type NixOS = StateT NixOSState IO

emptyState :: NixOSState
emptyState = NixOSState { inputs = NixRawId 0
                      , pkgs = NixRawId 0
                      , nixOverlayConfig = NixRawId 0
                      , haskellConfig = NixAttrset []
                      }

getKey :: String -> [NixAttr] -> Maybe NixValue
getKey key attrs = safeHead $ map snd $ filter ((==key) . fst) attrs

instance NixGetAttrs NixOSState where
  state *. key = case getKey key $ unsafeAttrs $ haskellConfig state of
    Just key -> fromNixValue key
    Nothing -> nixOverlayConfig state *. key

  state **. key = case getKey key $ unsafeAttrs $ haskellConfig state of
    Just key -> pure key
    Nothing -> nixOverlayConfig state **. key

  state ***. key = case getKey key $ unsafeAttrs $ haskellConfig state of
    Just key -> pure key
    Nothing -> nixOverlayConfig state ***. key

getPkgs :: NixOS NixValue
getPkgs = get >>= return . pkgs

initializeNixOS :: NixOS ()
initializeNixOS = do
  inputs <- liftIO $ getInputValue

  -- NOTE it should not matter when we fetch the data from nix
  pkgs <- liftIO . unsafeInterleaveIO $ inputs *. "specialArgs" *. "pkgs"

  modules <- liftIO . unsafeInterleaveIO $ (\val -> inputs *. "evalModules" $$ [val])
    =<< ioAttrs [ "nixpkgs" ||. "hostPlatform" ||. (pkgs *. "hostPlatform" *. "system") ]
  
  config <- liftIO . unsafeInterleaveIO $ modules *. "config"

  put $ NixOSState { inputs = NixRawId inputs
                   , pkgs = NixRawId pkgs
                   , nixOverlayConfig = NixRawId config
                   , haskellConfig = NixAttrset []
                   }

addConfig :: NixValue -> NixOS ()
addConfig attrs = modify (\s -> s { haskellConfig = haskellConfig s /// unsafeAttrs attrs })

configIO :: ToNix a => [IO (String, a)] -> NixOS ()
configIO attrs = addConfig =<< liftIO (ioAttrs attrs)

configMIO :: ToNix a => [IO (String, a)] -> NixOS ()
configMIO attrs = addConfig =<< liftIO (ioMAttrs attrs)

config :: ToNix a => [(String, a)] -> NixOS ()
config = addConfig . attrs

configM :: ToNix a => [(String, a)] -> NixOS ()
configM = addConfig . mAttrs
