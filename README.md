# Haspun

Haspun is a less than bare bones web server written in Haskell.

I wrote this around 2009 and recently cleaned it up slightly to be compatible with the latest version of GHC.

## Building

Grab the [Haskell Platform](http://www.haskell.org/platform/) or your choice of [GHC](http://www.haskell.org/ghc/) setup. Then:

```bash
$ ghc haspun.hs
```

## Usage

haspun
  -V       --version      show version number
  -h       --help         show usage
  -c FILE  --config=FILE  set configuration file (not implemented)
  -d DIR   --docroot=DIR  set doc root
  -p PORT  --port=PORT    set port

Specify a port and a document root and you're off.

```bash
$ haspun --docroot=test/docroot/ --port=8081
```

Point your web server at http://localhost:8081/index.html

## License

Copyright © 2013 Andy Payne

Distributed under the BSD 3 clause license:

Copyright (c) 2013, Andy Payne
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of the Andy Payne nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

