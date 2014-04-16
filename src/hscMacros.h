#include <stdio.h>

/*
Creates a class for a field with a particular Haskell type.
Example: #mkField Width, CInt
Generates:
class HasWidth t where
  getWidth :: t -> IO CInt
  setWidth :: t -> CInt -> IO ()
  hasWidth :: t -> Ptr CInt
*/

#define hsc_mkField(name, hType) \
  printf("class Has%s t where\n", #name);\
  printf("  get%s :: t -> IO %s\n", #name, #hType);\
  printf("  set%s :: t -> %s -> IO ()\n", #name, #hType);\
  printf("  has%s :: t -> Ptr %s\n", #name, #hType);

/*
Creates an instance of settable field class. The assumption is that
the first argument, the type name, is the name of bot the C and
Haskell types involved. This makes use of the HasPtr class whose
single method, getPtr, with type "a -> Ptr ()", unwraps a raw Ptr from
a value (typically a newtype around a Ptr).

Example: #hasField AVCodecContext, BitRate, bit_rate
Generates:
instance HasBitRate AVCodecContext where
  getBitRate = (#peek AVCodecContext, bit_rate) . getPtr
  setBitRate = (#poke AVCodecContext, bit_rate) . getPtr
  hasBitRate = (#ptr AVCodecContext, bit_rate) . getPtr
*/

#define hsc_hasField(type, hName, cName) \
   printf("instance Has%s %s where\n", #hName, #type);\
   printf("  get%s = ", #hName);\
   hsc_peek(type, cName)\
   printf(" . getPtr\n");\
   printf("  set%s = ", #hName);\
   hsc_poke(type, cName)\
   printf(" . getPtr\n");\
   printf("  has%s = ", #hName);\
   hsc_ptr(type, cName)\
   printf(" . getPtr\n");

