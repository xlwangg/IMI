# Changelog

This file documents all notable changes to the GEOS-Chem Classic wrapper repository since version 13.4.1. See also CHANGELOG files for individual submodules:
- src/geos-chem/CHANGELOG.md
- src/HEMCO/CHANGELOG.md

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [14.2.2] - 2023-10-23
### Changed
- Updated GEOS-Chem submodule to 14.2.2

## [14.2.1] - 2023-10-10
### Changed
- `test` now points to `src/GEOS-Chem/test`
- Updated GEOS-Chem submodule to 14.2.1
- Updated HEMCO submodule to 3.7.1
- Updated version numbers 
- Updated version numbers in `CMakeLists.txt` and `docs/source/conf.py` to 14.2.0

## [14.2.0] - 2023-10-05
## Added
- Quick start guide for GEOS-Chem Classic in ReadTheDocs
- Add note about WashU status downtime and link to `itstatus.wustl.edu`
- Added `SatDiagn` to list of GEOS-Chem History collections
- Added definitions for transport tracer properties to ReadTheDocs

### Changed
- Updated GEOS-Chem submodule to 14.2.0
- Updated HEMCO submodule to 3.7.0
- Updated version numbers in `CMakeLists.txt` and `docs/source/conf.py` to 14.2.0
- Reorganize folder structure of ReadTheDocs `docs/source` subdirectories
- Note that HEMCO log output is now sent to the GEOS-Chem log file
- Updated `AUTHORS.txt` for version 14.2.0
- Updated links in `README.md` to point to `http://geos-chem.org`
- Updated logo & badge links in `README.md` to use HTML
- Updated version numbers to 14.2.0

### Fixed
- Fixed typo in Intel debug option in `CMakeLists.txt`
- Fixed broken link to NCO page in ReadTheDocs

### Removed
- `passive_species` menu in `geoschem_config.rst` ReadTheDocs file

## [14.1.1] - 2023-03-03
### Added
  - Added `.github/ISSUE_TEMPLATE/config.yml` file w/ Github issue options

### Changed
  - Updated GEOS-Chem submodule to 14.1.1
  - Updated HEMCO submodule to 3.6.2
  - The GitHub PR template is now named `./github/PULL_REQUEST_TEMPLATE.md`
  - Updated ReadTheDocs user registration documentation GEOS-Chem 14.1.1
  - Updated ReadTheDocs run directory creation information for GEOS-Chem 14.1.1
  - Updated version numbers in `CMakeLists.txt` and `docs/source/conf.py` to 14.1.1

## [14.1.0] - 2023-02-01
### Added
  - Added SANITIZE CMake option

### Changed
  - Updated GEOS-Chem submodule to 14.1.0
  - Updated HEMCO submodule to 3.6.0
  - Updated ReadTheDocs documentation for 14.1.0

### Fixed
  - Fixed logic in .github/workflows/findRefKey.sh for determing previous commit

## [14.0.2] - 2022-11-29
### Changed
  - Updated GEOS-Chem submodule to 14.0.2
  - Updated HEMCO submodule to 3.5.2
  - Updated the documentation for clarity


## [14.0.1] - 2022-10-31
### Changed
  - Updated GEOS-Chem submodule to 14.0.1


## [14.0.0] - 2022-10-25
### Added
  - Added files for GEOS-Chem User Manual on ReadTheDocs
  - Created CHANGELOG.md

### Changed
  - Changed GEOS-Chem submodule to 14.0.0 release
  - Changed HEMCO submodule to 3.5.0 release
  - Updated CONTRIBUTING.md and SUPPORT.md to retire guidelines on GEOS-Chem wiki
