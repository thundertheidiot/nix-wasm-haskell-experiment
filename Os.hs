import NixHost

main = do
  inputs <- getInputValue

  pkgs <- inputs *. "specialArgs" *. "pkgs"

  systemPackages <- mapM (pkgs *.) ["vim", "hello"]

  config <- pure
    $ "networking" |. "hostName" |. "built_by_haskell"
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
