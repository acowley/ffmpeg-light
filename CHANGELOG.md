# 0.11.0

* Query stream duration (Matthias Treydte)

# 0.10. 0

* Fix encoder bug that created a single black frame at the start of
  every video (Jonathan Daugherty)

# 0.9.0

* Add support for camera input (Thomas M. DuBuisson)
  * Try it: build the demo executable (`cabal configure -fBuildDemo`)
    and run `cabal run demo -- cam` to record 10s of video from a
    connected camera to an output file `camera.mov`.

* Extract frame time stamps from the video stream rather than the
  codec context (hat tip to Jaro Reinders)

# 0.8.2

* Added probe features

# 0.8.1

* Update raster demo to use new JuicyPixels-3.2 API

# 0.8

* Update to transformers-0.4.1 and mtl-2.2.1
  * Changed decode-related types to accomodate deprecation of the
    `Error` class. This means that if you want to initialize decoders
    in your own transformer stack that has a `MonadError` instance,
    you will need to use the variants with names suffixed by a "T"
    (for transformer).

* Update to ffmpeg 2.3

  * Address deprecation warning

    `Using AVStream.codec.time_base as a timebase hint to the muxer is
       deprecated. Set AVStream.time_base instead.`

  * Address "non-strictly-monotonic PTS" warning

* Rasterific bump
  * Rasterific exports its own linear algebra types as of 0.3

# 0.7.1

* Bumped transformers dependency

  Note: The use of mtl still triggers deprecation warnings from
  transformers.

* Fixed bug with changing source pixel format from RGB during
  encoding.

* Added BGRA pixel format

# 0.7

* Simplified top-level API to focus on JuicyPixels-based interface

# 0.6

* Cleaned the API of detritus. Use the image* functions.

# 0.5

* Juiced the Encode and Decode APIs.

  Using `imageWriter` and `imageReader` provides a degree of pixel
  format polymorphism based on JuicyPixels pixel types.

# 0.4

* Fixed corrupted output of palettized animated GIFs.

* Added palettization options

    * Using `avPixFmtRgb8` results in a small file

    * Using the default pixel format (`avPixFmtPal8`) results in a good-looking,
       fairly large file thanks to JuicyPixels's `palettize` function.

    * Setting the `epPreset` field of the `EncodingParams` value passed to
      `frameWriter` to `"dither"` results in an even prettier, even larger GIF
      file (again, thanks to JuicyPixels's `palettize` function).

    * See the `demo/Raster.hs` for examples.

# 0.3.1

* Automatically palettize RGB24 to RGB8 for GIF output.

* Add a Rasterific demo program that records an animation.

# 0.3

* Support for GIF encoding (and other palletized formats).

# 0.2

* Separate `Scaler` module and friendly `libswscaler` interface.

* Generalized `toJuicy` conversion.

* Added demo program.

# 0.1

* Basic h264 encoding and decoding.
