# FFmpeg compilation instructions from:
# http://trac.ffmpeg.org/wiki/CompilationGuide/Ubuntu

# Prepare to compile FFmpeg
mkdir -p ~/ffmpeg_sources
mkdir -p ~/ffmpeg_build
mkdir -p ~/bin

# libx264
cd ~/ffmpeg_sources
wget http://download.videolan.org/pub/x264/snapshots/last_x264.tar.bz2
tar xjvf last_x264.tar.bz2
cd x264-snapshot*
PATH="$PATH:$HOME/bin" ./configure --prefix="$HOME/ffmpeg_build" --bindir="$HOME/bin" --enable-static --disable-opencl
PATH="$PATH:$HOME/bin" make
make install
make distclean

# libfdk-aac
cd ~/ffmpeg_sources
wget -O fdk-aac.zip https://github.com/mstorsjo/fdk-aac/zipball/master
unzip fdk-aac.zip
cd mstorsjo-fdk-aac*
autoreconf -fiv
./configure --prefix="$HOME/ffmpeg_build" --disable-shared
make
make install
make distclean

# Compile FFmpeg
cd ~/ffmpeg_sources
wget http://ffmpeg.org/releases/ffmpeg-snapshot.tar.bz2
tar xjvf ffmpeg-snapshot.tar.bz2
cd ffmpeg
PATH="$PATH:$HOME/bin" PKG_CONFIG_PATH="$HOME/ffmpeg_build/lib/pkgconfig" ./configure \
  --prefix="$HOME/ffmpeg_build" \
  --extra-cflags="-I$HOME/ffmpeg_build/include" \
  --extra-ldflags="-L$HOME/ffmpeg_build/lib" \
  --bindir="$HOME/bin" \
  --enable-gpl \
  --enable-libass \
  --enable-libfdk-aac \
  --enable-libmp3lame \
  --enable-libx264 \
  --enable-nonfree
PATH="$PATH:$HOME/bin" make
make install
make distclean
hash -r

# ffmpeg-light
cd
cabal get ffmpeg-light
cd ffmpeg-light*
cabal install --dependencies-only
PKG_CONFIG_PATH="$HOME/ffmpeg_build/lib/pkgconfig" cabal configure --disable-shared -fBuildDemo
cabal build demo

echo 'Now you should be able to "cabal run demo"'
