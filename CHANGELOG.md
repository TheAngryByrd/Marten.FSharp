# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.3]

### Fixed
- Task map with error handling (https://github.com/TheAngryByrd/Marten.FSharp/pull/26)

## [0.4.2]

### Changed
- API Documentation (https://github.com/TheAngryByrd/Marten.FSharp/pull/24)

### BREAKING:
- Api Changes for Patch and Count (https://github.com/TheAngryByrd/Marten.FSharp/pull/24)

## [0.4.1]

### Fixed
- Fix misc api errors (https://github.com/TheAngryByrd/Marten.FSharp/pull/21/files)

## [0.4.0]

### Added
- Revealing cancellable version of task/async calls (https://github.com/TheAngryByrd/Marten.FSharp/pull/19)
- Reveal count apis (https://github.com/TheAngryByrd/Marten.FSharp/pull/20)

### INFRASTRUCTURE:
- Update Marten 2.9 (https://github.com/TheAngryByrd/Marten.FSharp/pull/20)
- Use paket (https://github.com/TheAngryByrd/Marten.FSharp/pull/20)

## [0.3.0]

### Added
- [Added Session.deleteBy a wrapper for DeleteWhere.](https://github.com/TheAngryByrd/Marten.FSharp/pull/15)

### Fixed
- [Fixed Session.load and Session.delete APIs](https://github.com/TheAngryByrd/Marten.FSharp/pull/15)

## [0.2.0]

### BREAKING:
- [Update to Marten 2.7](https://github.com/TheAngryByrd/Marten.FSharp/pull/14)

## [0.1.1]

### Fixed
- [Fixed using IsOneOf in Queryable.filter](https://github.com/TheAngryByrd/Marten.FSharp/pull/12)

## [0.1.0]

### BREAKING:
- Api Design Changes (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)
- API Modifications
- Functions associated with IQuerySession and IDocumentSession have been moved to Session module
- Functions associated with IQueryable have been moved to Queryable module
- Functions Associated with Patching have been moved to Session.Patch module

### Changed
- Expose Tasks as well as Async calls (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)

### Added
- Allow for Dicriminated Union of PrimaryKey for Delete and Load functions (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)
- Ability to query with SQL (https://github.com/TheAngryByrd/Marten.FSharp/pull/6)
- Improve Patch API and added other Linq queries (https://github.com/TheAngryByrd/Marten.FSharp/pull/4) via @ibnuda
- Patch Functionality (https://github.com/TheAngryByrd/Marten.FSharp/pull/3) via @ibnuda

[0.4.3]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.4.2...0.4.3
[0.4.2]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.4.1...0.4.2
[0.4.1]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.1.1...0.2.0
[0.1.1]: https://github.com/TheAngryByrd/MiniScaffold/compare/0.1.0...0.1.1
[0.1.0]: https://github.com/TheAngryByrd/MiniScaffold/releases/tag/0.1.0