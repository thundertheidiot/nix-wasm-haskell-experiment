{
  nixosSystem,
  pkgs,
  configurationName,
  wasm,
}:
nixosSystem {
  specialArgs = {inherit pkgs;};
  modules = [
    {
      config.nixpkgs.pkgs = pkgs;
    }
    (set:
      builtins.wasm {path = wasm;} (set
        // {
          inherit configurationName;
          evalModules = extra:
            pkgs.lib.evalModules {
              class = "nixos";
              modules = import "${set.specialArgs.modulesPath}/../../nixos/modules/module-list.nix" ++ extra;
            };
        }))
  ];
}
