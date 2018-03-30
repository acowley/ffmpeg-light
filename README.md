Minimal bindings to the [FFmpeg](www.ffmpeg.org) library.

Stream frames from an encoded video, or stream frames to a video output file. To read the first frame from an `h264`-encoded file into a [`JuicyPixels`](http://hackage.haskell.org/package/JuicyPixels) `Maybe DynamicImage`,

```haskell
import Codec.FFmpeg
import Codec.Picture
import Control.Applicative

go :: IO (Maybe DynamicImage)
go = do initFFmpeg
        (getFrame, cleanup) <- imageReader (File "myVideo.mov")
        (fmap ImageRGB8 <$> getFrame) <* cleanup
```

A demonstration of creating an animation using the
[`Rasterific`](http://hackage.haskell.org/package/Rasterific) library
may be found in
[`demo/Raster.hs`](https://github.com/acowley/ffmpeg-light/blob/master/demo/Raster.hs). A
weird animated variation of the `Rasterific` logo is the result:

![Animated Rasterific Logo](https://github.com/acowley/ffmpeg-light/raw/master/demo/logoTiny.gif)

Note that encoding an animation to a modern video codec like h264 can
result in even smaller files. But those files can't be embedded in a
README on github.

Tested on OS X 10.9.2 with FFmpeg 2.2.1 installed via [homebrew](http://brew.sh).

Debian and Ubuntu users: Your package manager's `ffmpeg` package is actually a not-quite-compatible fork of the `ffmpeg` project. To use `ffmpeg-light`, run the included `ffmpeg-ubuntu-compile.sh` script as regular (non-root) user. This builds the ffmpeg libraries locally. Configure your projects that depend on `ffmpeg-light` with a modified `PKG_CONFIG_PATH`:

```bash
PKG_CONFIG_PATH="$HOME/ffmpeg_build/lib/pkgconfig" cabal configure --disable-shared my-project
```

There are signs that the next Ubuntu release will come with the original `ffmpeg` and development packages.

[![Build Status](https://travis-ci.org/acowley/ffmpeg-light.png)](https://travis-ci.org/acowley/ffmpeg-light)
