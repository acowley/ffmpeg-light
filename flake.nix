{
  description = "Haskell wrapper for the ffmpeg library";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { 
            inherit system;
          };
          # compiler = "8107";
          compiler = "921";
          hspkgs =
            let doJailbreak = pkgs.haskell.lib.doJailbreak;
                dontCheck = pkgs.haskell.lib.dontCheck;
            in pkgs.haskell.packages."ghc${compiler}".override {
            overrides = final: prev:
              if compiler == "921"
              then {
                linear = prev.callHackage "linear" "1.21.8" {};
                sdl2 = dontCheck (prev.callHackage "sdl2" "2.5.3.1" {});
              }
              else { };
          };
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
