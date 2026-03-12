import NixWasm.Lib
import NixWasm.Stateful

import Control.Monad (join)
import Control.Monad.State

boot :: Nix ()
boot = addConfig $ mAttrs [ "fileSystems" |. "/" |. "fsType" |. "tmpfs"
                          , "boot" |. "loader" |. "grub" |.
                            (attrs
                             [ "devices" |. ["nodev"]
                             , "efiSupport" |. True
                             , "zfsSupport" |. True
                             ])
                          , "boot"  |. "initrd" |. "systemd" |. "enable" |. True
                          ]

mainNix :: Nix NixValue
mainNix = do
  initializeNix "x86_64-linux"

  pkgs <- get >>= \state -> liftIO $ inputs state *. "specialArgs" *. "pkgs"

  -- print example
  -- get >>= \state -> liftIO $ (state *. "networking" ***. "hostName") >>= nixWarn . show

  addConfig $ attrs [ "networking" |. "hostName" |. "haskell"
                    , "time" |. "timeZone" |. "Europe/Helsinki"
                    , "users" |. "users" |. "user" |. attrs
                      [ "extraGroups" |. ["wheel", "networkmanager"]
                      , "isNormalUser" |. True ]
                    , "services" |. "openssh" |. "enable" |. True
                    ]

  addConfig =<< liftIO
    (ioAttrs [ "environment" ||. "systemPackages" ||. mapM (pkgs *.) ["vim"]
             ]) 

  boot

  return . config =<< get

main :: IO ()
main = runStateT mainNix emptyState >>= nixReturn . fst
