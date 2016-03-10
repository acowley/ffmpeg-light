{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, either, exceptions, ffmpeg-full, JuicyPixels
      , mtl, stdenv, transformers, vector, Rasterific, time
      }:
      mkDerivation {
        pname = "ffmpeg-light";
        version = "0.11.0";
        src = builtins.filterSource (path: type:
          type != "directory" || (let bn = baseNameOf path;
            in bn == "src" || bn == "demo"));
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base either exceptions JuicyPixels mtl transformers vector
        ];
        libraryPkgconfigDepends = [ ffmpeg-full ];
        executableHaskellDepends = [
          base JuicyPixels mtl transformers vector Rasterific time
        ];
        homepage = "http://github.com/acowley/ffmpeg-light";
        description = "Minimal bindings to the FFmpeg library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = pkgs.myHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
