{
  description = "Gmic development";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

  outputs = { nixpkgs, ... }: {
    devShells.x86_64-linux = nixpkgs.lib.mkShell {
      packages = [ nixpkgs.gmic ];
    };
  };
}
