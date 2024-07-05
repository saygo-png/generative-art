{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          lib,
          ...
        }:
        {
          # Per-system attributes can be defined here. The self' and inputs'
          # module parameters provide easy access to attributes of the same
          # system.
          devenv.shells.default = {
            # https://devenv.sh/reference/options/
            packages = with pkgs.python3Packages; [
              pkgs.processing
              numpy
              jpype1
              pkgs.jdk
              pkgs.pipenv
              pkgs.scilab-bin
              glcontext
            ];

            env = {
              # LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
              NIX_LD_LIBRARY_PATH = lib.makeLibraryPath [
                pkgs.stdenv.cc.cc
                pkgs.processing
              ];
              NIX_LD = builtins.readFile "${pkgs.stdenv.cc}/nix-support/dynamic-linker";
            };

            dotenv.disableHint = true;

            languages.java.enable = true;
            languages.python = {
              enable = true;
              venv.enable = true;
            };
          };
        };
    };
}
