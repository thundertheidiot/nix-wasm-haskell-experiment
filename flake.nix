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
            wasm32-wasi-ghc Main.hs -o nix.wasm -optl-Wl,--export=memory -optl-Wl,--allow-undefined -odir ./build -hidir ./build
          '')
        ];
      };
    });
  };
}
