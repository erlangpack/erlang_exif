# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [3.0.0] - 2021-08-27
### Changed
- mworrell: Renamed to erlang_exif, removed R16 compat, published to Hex

## [2.0.3] - 2017-01-25
### Changed
- mworrell: Fix a problem with some cameras inserting 'no value' exif tags.
- mworrell: Fix parsing exif info when IFD block is at end of file.

## [2.0.2] - 2016-03-16
### Changed
- Fix code and tests to work with Erlang/OTP R16.

## [2.0.1] - 2015-12-22
### Changed
- Update `rebar.config` Erlang/OTP dependency to include R18.

## [2.0.0] - 2015-05-17
### Changed
- The read functions now return `{ok, Data}` instead of just `Data`, to make it easier
  to distinquish from the error case (`{error, Reason}`).

## [1.1.0] - 2015-05-14
### Changed
- Library now detects and skips JFIF segments, if present. Previously it would fail
  to read the Exif data because the APP1 marker did not follow the SOI marker (which
  is, in fact, correct).

## [1.0.0] - 2011-01-06
- Initial release, able to read Exif if APP1 marker immediately followed SOI marker.
