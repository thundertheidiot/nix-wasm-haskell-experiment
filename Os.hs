import NixWasm.Lib 

main = do
  inputs <- getInputValue

  -- *. gets only the id
  pkgs <- inputs *. "specialArgs" *. "pkgs"

  systemPackages <- mapM (pkgs *.) ["vim", "hello"]

  -- lets override some stuff
  catppuccinGtk <- pkgs *. "catppuccin-gtk" *. "override"
    $$ attrs [ "accents" |. ["mauve"]
             , "size" |. "compact"
             , "variant" |. "mocha"
             ]

  -- **. lazily gets the value, leaving lists and attrsets as lists and attrsets of ids
  gajimNativeBuildInputs <- pkgs *. "gajim" **. "nativeBuildInputs"
  makeWrapper <- pkgs *. "makeWrapper"
  -- ***. eagerly gets the value, copying over the contents
  -- this causes segfaults with at least derivations, but basic data types work fine
  gajimPostInstall <- pkgs *. "gajim" ***. "postInstall"

  gajim <- pkgs *. "gajim" *. "overrideAttrs"
    $$ attrs [ "nativeBuildInputs" |. mergeNix -- mergeNix operates on lists
               gajimNativeBuildInputs (toNix [makeWrapper])
             , "postInstall" |. joinNixString -- joins any combination of ToNix
               gajimPostInstall "\n\
                                \wrapProgram $out/bin/gajim --set XDG_CURRENT_DESKTOP GNOME"
             ]

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
     , "environment" |. "systemPackages" |. [gajim]
     , "environment" |. "systemPackages" |. [catppuccinGtk]

     , "services" |. "openssh" |. enable
     ]
  where
    enable = "enable" |. True
