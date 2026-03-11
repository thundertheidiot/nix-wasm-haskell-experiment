{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.dix.url = "github:DeterminateSystems/nix-src";
  inputs.ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

  outputs = {nixpkgs, ...} @ inputs: let
    forAllSystems = f:
      nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
        system:
          f (import nixpkgs {
            inherit system;
          })
      );
  in {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = [
          (inputs.dix.packages."${pkgs.system}".default.overrideAttrs (old: {
            # me when i ship a broken build
            # truly inspiring confidence
            doCheck = false;
          }))
          inputs.ghc-wasm-meta.packages."${pkgs.system}".default
          (pkgs.writeShellScriptBin "build" ''
            mkdir ./build

            wasm32-wasi-ghc Os.hs -o ./build/nixos.wasm -optl-Wl,--export=memory -optl-Wl,--allow-undefined -odir ./build -hidir ./build
          '')

          (pkgs.writeShellScriptBin "buildos" ''
            set -euox pipefail

            mkdir --parents ./build ./out
            wasm32-wasi-ghc Os.hs -o ./out/config.wasm -optl-Wl,--export=memory -optl-Wl,--allow-undefined -odir ./build -hidir ./build

            nix --extra-experimental-features wasm-builtin build .#nixosConfigurations.test.config.system.build.toplevel --show-trace --print-out-paths --no-link
          '')
        ];
      };
    });

    nixosConfigurations.test = nixpkgs.lib.nixosSystem (let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      specialArgs = {inherit pkgs;};
      modules = [
        nixpkgs.nixosModules.readOnlyPkgs
        ({...}: {
          config.nixpkgs.pkgs = pkgs;
        })
        (set:
          builtins.wasm {path = ./out/config.wasm;} (set
            // {
              evalModules = extra:
                nixpkgs.lib.evalModules {
                  class = "nixos";
                  modules = import "${set.specialArgs.modulesPath}/../../nixos/modules/module-list.nix" ++ extra;
                };
            }))
      ];
    });
  };
}
