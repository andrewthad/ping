# Changelog
All notable changes to this project will be documented in this file.

The format is inspired by [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).
This changelog deviates from the recommendation by not grouping changes into
added, changed, deprecated, etc. subsections.

This project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.1.0.3] - 2019-07-22
- Library now builds with newer `posix-api-0.3.*` and with
  `primitive-0.7.*`.

## [0.1.0.2] - 2019-04-08
- Not documented

## [0.1.0.1] - 2018-01-07
- Fix a problem in `multihosts` that occassionally caused the function
  to wait for a number of nanoseconds close to `maxBound :: Word64`.
  This had been caused by a subtraction underflow.

## [0.1.0.0] - 2018-01-02
- Initial release.
- Function `host` for pinging a single host once.
- Function `hosts` and `range` for pinging multiple hosts once each.
- Function `multihosts` and `multirange` for pinging multiple hosts once each.
