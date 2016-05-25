{ nixpkgs ? import <nixpkgs> {}, compiler ? "mine" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, either, exceptions, ffmpeg-full, JuicyPixels
      , mtl, stdenv, transformers, vector, Rasterific, time, cabal-install
      }:
      mkDerivation {
        pname = "ffmpeg-light";
        version = "0.11.0";
        src = builtins.filterSource (path: type:
          type != "directory" || (baseNameOf path != "dist")) ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base either exceptions JuicyPixels mtl transformers vector
        ];
        libraryPkgconfigDepends = [ ffmpeg-full ];
        executableHaskellDepends = [
          base JuicyPixels mtl transformers vector Rasterific time
        ];
        buildTools = [ cabal-install stdenv ];
        homepage = "http://github.com/acowley/ffmpeg-light";
        description = "Minimal bindings to the FFmpeg library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else if compiler == "mine"
                            then pkgs.myHaskellPackages
                            else with import (<nixpkgs> + /pkgs/development/haskell-modules/lib.nix) { pkgs=nixpkgs; };
                                 pkgs.haskell.packages.${compiler}.override {
                              overrides = self: super: {
                                FontyFruity = doJailbreak super.FontyFruity;
                                Rasterific = doJailbreak super.Rasterific;
                                either = super.either_4_4_1_1;
                                cabal-install = dontCheck super.cabal-install_1_24_0_0;
                                ed25519 = dontCheck super.ed25519;
                                hackage-security = dontCheck super.hackage-security;
                              };
                            };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
