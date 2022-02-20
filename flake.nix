{
  description = "Haskell wrapper for the ffmpeg library";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { 
            inherit system;
          };
          compiler = "ghc8107";
          hspkgs = pkgs.haskell.packages.${compiler};
          ffmpeg-light = hspkgs.callPackage (import ./default.nix) { nix-filter = nix-filter.lib; };
          ghc = hspkgs.ghc.withHoogle (ps: ffmpeg-light.passthru.getBuildInputs.haskellBuildInputs);
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.SDL2 pkgs.ffmpeg pkgs.pkgconfig
            ghc hspkgs.cabal-install
            # hspkgs.haskell-language-server
          ];
        };
        packages.ffmpeg-light = ffmpeg-light;
        defaultPackage = self.packages.${system}.ffmpeg-light;
      }
    );
}
