{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = with pkgs; [
    (
      pkgs.haskellPackages.ghcWithPackages (
        haskellPackages: [
          # We packages here, so ghc knows about them.
          haskellPackages.GLUT
          haskellPackages.gloss
          haskellPackages.rio
          haskellPackages.containers
          haskellPackages.haskell-language-server
          haskellPackages.implicit-hie
          ghcid
          cabal-install
        ]
      )
    )
    haskell-language-server
    ghcid
    cabal-install
    haskellPackages.GLUT
    haskellPackages.gloss
    haskellPackages.rio
    haskellPackages.containers
    (
      # Wrap Stack to work with our Nix integration. We don't want to modify
      # stack.yaml so non-Nix users don't notice anything.
      # - no-nix: We don't want Stack's way of integrating Nix.
      # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
      # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
      pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [pkgs.stack];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      }
    )
  ];
}
