Minimal bindings to the [FFmpeg](www.ffmpeg.org) library.

Stream frames from an encoded video, or stream frames to a video output file. To read the first frame from an `h264`-encoded file into a [`JuicyPixels`](http://hackage.haskell.org/package/JuicyPixels) `Maybe DynamicImage`,

```haskell
import Codec.FFmpeg
import Codec.Picture
import Control.Applicative

go :: IO (Maybe DynamicImage)
go = do (getFrame, cleanup) <- imageReader "myVideo.mov"
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

[![Build Status](https://travis-ci.org/acowley/ffmpeg-light.png)](https://travis-ci.org/acowley/ffmpeg-light)
