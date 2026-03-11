import NixWasm.Lib 
import Control.Monad (join)

main = do
  inputs <- getInputValue

  -- *. gets only the id
  pkgs <- inputs *. "specialArgs" *. "pkgs"

  mod <- pure $
         attrs [ "nixpkgs" |. "hostPlatform" |. "x86_64-linux"
               ]

  evalModules <- inputs *. "evalModules"

  modules <- evalModules $$ [mod]
  config <- modules **. "config"

  _ <- nixWarn . show =<< modules **. "config"

  systemPackages <- mapM (pkgs *.) ["vim", "hello"]

  nixReturn $ "config" |. mAttrs 
    [ "networking" |. "hostName" |. "built_by_haskell"
     , "time" |. "timeZone" |. "Europe/Helsinki"
     , "users" |. "users" |. "user" |. attrs
       [ "extraGroups" |. ["wheel", "networkmanager"]
       , "isNormalUser" |. False ]
     -- end precedence
     , "users" |. "users" |. "user" |. "isNormalUser" |. True

     , "fileSystems" |. "/" |. "fsType" |. "tmpfs"
     -- mAttrs merges attribute sets
     , "boot" |. mAttrs
       [ "loader" |. "grub" |. "devices" |. ["nodev"]
       , "loader" |. "grub" |. "efiSupport" |. True
       , "loader" |. "grub" |. "zfsSupport" |. True -- both remain
       , "initrd" |. "systemd" |. enable  
       ]
     , "system" |. "stateVersion" |. "26.05"
     
     -- lists are also merged
     , "environment" |. "systemPackages" |. systemPackages

     , "services" |. "openssh" |. enable
     ]
  where
    enable = "enable" |. True
