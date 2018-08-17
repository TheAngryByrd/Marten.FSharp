#### 0.4.0 - 17.08.2018
* FEATURE: Revealing cancellable version of task/async calls
* FEATURE: Reveal count apis
* INFRASTRUCTURE: Update Marten 2.9
* INFRASTRUCTURE: Use paket

#### 0.3.0 - 25.04.2018
* FEATURE: [Added Session.deleteBy a wrapper for DeleteWhere.](https://github.com/TheAngryByrd/Marten.FSharp/pull/15)
* BUGFIX:  [Fixed Session.load and Session.delete APIs](https://github.com/TheAngryByrd/Marten.FSharp/pull/15)

#### 0.2.0 - 18.04.2018
* BREAKING: [Update to Marten 2.7](https://github.com/TheAngryByrd/Marten.FSharp/pull/14)

#### 0.1.1 - 30.11.2017
* BUGFIX: [Fixed using IsOneOf in Queryable.filter](https://github.com/TheAngryByrd/Marten.FSharp/pull/12)

#### 0.1.0 - 28.07.2017
* BREAKING: Api Design Changes (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)
  * Functions associated with IQuerySession and IDocumentSession have been moved to Session module
  * Functions associated with IQueryable have been moved to Queryable module
  * Functions Associated with Patching have been moved to Session.Patch module
* MINOR: Expose Tasks as well as Async calls (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)
* FEATURE: Allow for Dicriminated Union of PrimaryKey for Delete and Load functions (https://github.com/TheAngryByrd/Marten.FSharp/pull/8)
* FEATURE:  Ability to query with SQL (https://github.com/TheAngryByrd/Marten.FSharp/pull/6)
* FEATURE:  Improve Patch API and added other Linq queries (https://github.com/TheAngryByrd/Marten.FSharp/pull/4) via @ibnuda
* FEATURE:  Patch Functionality (https://github.com/TheAngryByrd/Marten.FSharp/pull/3) via @ibnuda 
* BREAKING: API Modifications
