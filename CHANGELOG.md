# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [1.1.0] - 2015-03-14
### Changed
- Library now detects and skips JFIF segments, if present. Previously it would fail
  to read the Exif data because the APP1 marker did not follow the SOI marker (which
  is, in fact, correct).

## [1.0.0] - 2011-01-06
- Initial release, able to read Exif if APP1 marker immediately followed SOI marker.
