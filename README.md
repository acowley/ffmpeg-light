Minimal bindings to the [FFmpeg](www.ffmpeg.org) library.

Stream frames from an encoded video, or stream frames to a video output file. To read the first frame from an `h264`-encoded file into a [`JuicyPixels`](http://hackage.haskell.org/package/JuicyPixels) `Maybe DynamicImage`,

```haskell
import Codec.FFmpeg
import Codec.Picture
import Control.Applicative
import Data.Traversable

go :: IO DynamicImage
go = do (getFrame, cleanup) <- frameReader "myVideo.mov"
        (getFrame >>= traverse juicyRGB) <* cleanup
```

Tested on OS X 10.9.2 with FFmpeg 2.2.1 installed via [homebrew](http://brew.sh).
