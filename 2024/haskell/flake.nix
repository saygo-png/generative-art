{
  description = "Haskell development environment with Gloss, Cabal, GHC, GHCI, and Haskell Language Server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    pkgs = import nixpkgs {system = "x86_64-linux";};
  in {
    devShells = {
      x86_64-linux.default = pkgs.mkShell {
        packages = with pkgs; [
          # We specify packages here
          cabal-install
          ghcid
          haskell-language-server
          (
            pkgs.haskellPackages.ghcWithPackages (
              haskellPackages:
                with haskellPackages; [
                  # We specify hackage packages here
                  GLUT
                  gloss
                ]
            )
          )
        ];
      };
    };
  };
}
