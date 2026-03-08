import NixHost
import Foreign.C.String
import Foreign.Ptr
import Control.Monad

main = do
  inputs <- getInputValue

  pkgs <- inputs *. "specialArgs" *. "pkgs"

  systemPackages <- mapM (pkgs *.) ["vim", "kodi"]

  config <- pure
    $ "networking" |. "hostName" |. "built_by_nixkell"
    // "time" |. "timeZone" |. "Europe/Helsinki"
    // "users" |. "users" |. "user" |. attrs ([ ("extraGroups", ["wheel", "networkmanager"])
                                              ] |.++
                                              [ ("isNormalUser", True)
                                              ])
    // "nixpkgs" |. "hostPlatform" |. "x86_64-linux"
    // "fileSystems" |. "/" |. attrs ([ ("fsType", "tmpfs")
                                      ])
    // "boot" |. "loader" |. "grub" |. "devices" |. ["nodev"]
    // "system" |. "stateVersion" |. "26.05"
    // "environment" |. "systemPackages" |. systemPackages
  
  nixReturn $ "config" |. config
