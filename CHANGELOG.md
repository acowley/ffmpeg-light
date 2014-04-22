0.4
---

* Fixed corrupted output of palettized animated GIFs.

* Added palettization options

    * Using `avPixFmtRgb8` results in a small file

    * Using the default pixel format (`avPixFmtPal8`) results in a good-looking,
       fairly large file thanks to JuicyPixels's `palettize` function.

    * Setting the `epPreset` field of the `EncodingParams` value passed to
      `frameWriter` to `"dither"` results in an even prettier, even larger GIF
      file (again, thanks to JuicyPixels's `palettize` function).

    * See the `demo/Raster.hs` for examples.

0.3.1
---

* Automatically palettize RGB24 to RGB8 for GIF output.

* Add a Rasterific demo program that records an animation.

0.3
---

* Support for GIF encoding (and other palletized formats).

0.2
---

* Separate `Scaler` module and friendly `libswscaler` interface.

* Generalized `toJuicy` conversion.

* Added demo program.

0.1
---

* Basic h264 encoding and decoding.
