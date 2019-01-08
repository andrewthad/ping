# ping

## Objective

This library provides high-performance functions that issue ICMP echo
requests and wait for responses, measuring the elapsed time. It is intended
to be cross-platform as possible, and it may use any system APIs available.
However, it will not resort to running `/bin/ping` as a subprocess. Pull
requests that improve compatibility will accepted so long as they do not
start a subprocess.

## Build Instructions

This library relies on `posix-api`, which needs a currently-unreleased
version of `hsc2hs` in order to build. In order to try out this library, try:

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

## Infelicities

This project's objective is to be cross-platform. However, it does not
accomplish this. The author primarily runs a Debian Linux distribution
and consequently lacks the resources and the motivation to make this
work on other platforms. Contributions to improve compatibility are
welcomed. Current known problems include:

- On Linux, the library should fallback to using raw sockets if `IPPROTO_ICMP`
  does not work.
- Support for Windows is missing.
- Support for BSD is missing. However, adding raw socket support to improve
  the situation on Linux may fix this as well.
- Support for Darwin is missing. This should not be difficult since
  Darwin supports `IPPROTO_ICMP` sockets.

