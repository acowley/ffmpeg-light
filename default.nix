{ mkDerivation, nix-filter, base, bytestring, either, exceptions, ffmpeg
, JuicyPixels, lib, monad-loops, mtl, Rasterific, sdl2
, stm, text, time, transformers, vector
}:
mkDerivation {
  pname = "ffmpeg-light";
  version = "0.14.1";
  src = nix-filter {
    root = ./.;
    include = [
      "./CHANGELOG.md"
      "demo"
      "./ffmpeg-light.cabal"
      "./LICENSE"
      "./README.md"
      "./Setup.hs"
      "./stack.yaml"
      "src"
      (nix-filter.inDirectory "src")
      (nix-filter.inDirectory "demo")
    ];
  };
  configureFlags = [
    "-fbuildaudioextractdemo" "-fbuildaudiosindemo" "-fbuilddemo"
    "-fbuildrasterdemo" "-fbuildtranscodedemo" "-fbuildvplaydemo"
  ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring either exceptions JuicyPixels mtl stm transformers
    vector
  ];
  libraryPkgconfigDepends = [ ffmpeg ];
  executableHaskellDepends = [
    base bytestring JuicyPixels monad-loops mtl Rasterific sdl2 text
    time transformers vector
  ];
  homepage = "http://github.com/acowley/ffmpeg-light";
  description = "Minimal bindings to the FFmpeg library";
  license = lib.licenses.bsd3;
}
