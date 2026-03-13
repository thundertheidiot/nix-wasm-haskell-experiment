import NixWasm.Lib
import NixWasm.NixOS

import Control.Monad (join)
import Control.Monad.State

boot :: NixOS ()
boot = config [ "fileSystems" |. "/" |. "fsType" |. "tmpfs"
              , "boot" |. "loader" |. "grub" |.
                (attrs
                  [ "devices" |. ["nodev"]
                  , "efiSupport" |. True
                  , "zfsSupport" |. True
                  ])
              , "boot"  |. "initrd" |. "systemd" |. "enable" |. True
              ]

mainNix :: NixOS NixValue
mainNix = do
  initializeNixOS

  -- print example
  -- get >>= \state -> liftIO $ (state *. "networking" ***. "hostName") >>= nixWarn . show

  pkgs <- getPkgs

  config [ "networking" |. "hostName" |. "haskell"
         , "time" |. "timeZone" |. "Europe/Helsinki"
         , "users" |. "users" |. "user" |. attrs
           [ "extraGroups" |. ["wheel", "networkmanager"]
           , "isNormalUser" |. True ]
         , "services" |. "openssh" |. "enable" |. True
         ]

  configIO [ "environment" ||. "systemPackages" ||. mapM (pkgs *.) ["vim"]
           ]

  boot

  return . haskellConfig =<< get

main :: IO ()
main = runStateT mainNix emptyState >>= nixReturn . fst
