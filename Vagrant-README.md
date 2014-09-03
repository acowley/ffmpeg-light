Getting FFmpeg installed can be tricky, so a [Vagrant](www.vagrantup.com) environment description is provided for working with ffmpeg-light in a Ubuntu 14.04 VM. After you've installed `vagrant` (and perhaps [VirtualBox](https://www.virtualbox.org) if that is how you are provisioning the VM), the following steps will get you started with GHC in a fresh VM. The terminal session shown below uses `host$` to indicate the prompt on your host machine, and `vagrant$` to indicate the prompt in the Vagrant VM.

- Change into the ffmpeg-light directory with the `Vagrantfile`
- `host$ vagrant init ubuntu/trusty64`
- `host$ vagrant up`
- `host$ vagrant ssh`
- `vagrant$ sh /vagrant/ffmpeg-ubuntu-compile.sh`
- `vagrant$ cabal run demo`

You can copy the video file produced by `cabal run demo` onto your host system by running, `cd pulse.mov /vagrant/linux-pulse.mov` to verify that everything worked.
