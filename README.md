# ping

This library provides functions that issue ICMP echo requests and wait for
responses, measuring the elapsed time. This relies on `posix-api`, which
needs a currently-unreleased version of `hsc2hs` in order to build. In
order to try out this library, try:

```
~/dev $ git clone https://github.com/haskell/hsc2hs
~/dev $ cd hsc2hs
~/dev/hsc2hs $ cabal install
~/dev/hsc2hs $ cd ..
~/dev $ git clone https://github.com/andrewthad/ping
~/dev $ cd ping
~/dev/ping $ cabal new-build --with-hsc2hs=~/.cabal/bin/hsc2hs
```

This will build all dependencies, including `posix-api`, with the
upstream `hsc2hs` tool.

