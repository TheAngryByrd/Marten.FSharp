namespace Marten

open System
open Marten
open Marten.PLv8
open Marten.PLv8.Patching
open Marten.Pagination

/// Useful functions for combining error handling computations with async computations.
[<AutoOpen>]
module AsyncExtensions =
    /// Useful functions for combining error handling computations with async computations.
    [<RequireQualifiedAccess>]
    module Async =
        /// Creates an async computation that return the given value
        let singleton = async.Return

        /// Creates an async computation that runs a computation and
        /// when it generates a result run a binding function on the said result
        let bind f x = async.Bind(x, f)

        /// Creates an async computation that runs a mapping function on the result of an async computation
        let map f x = x |> bind (f >> singleton)

    [<RequireQualifiedAccess>]
    module Task =
        open System.Threading.Tasks

        /// Applies a mapping function to the result of a successful task
        let map f (t: Task<_>) =
            let tcs = new TaskCompletionSource<'U>()

            t.ContinueWith(fun (task: Task<_>) ->
                if task.IsFaulted then
                    tcs.SetException(task.Exception.InnerExceptions)
                elif task.IsCanceled then
                    tcs.SetCanceled()
                else
                    try
                        tcs.SetResult(task.Result |> f)
                    with ex ->
                        tcs.SetException(ex))
            |> ignore

            tcs.Task


module Option =
    /// **Description**
    /// Creates an option from a potentially null record type.
    ///
    /// **Parameters**
    ///   * `record` - parameter of type `'a`
    ///
    /// **Output Type**
    ///   * `'a option`
    ///
    /// **Exceptions**
    ///
    let ofNullableRecord record =
        if (record |> box |> isNull) then None else Some record


open System
open System.Linq.Expressions
open System.Threading

module Lambda =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter

    let rec translateExpr (linq: Expression) =
        match linq with
        | :? MethodCallExpression as methodCall ->
            match methodCall.Arguments[0] with
            | :? LambdaExpression as lambda ->
                let args, body = translateExpr lambda.Body
                lambda.Parameters[0] :: args, body
            // | :? MemberExpression as me -> ...
            | _ -> [], linq
        | _ -> [], linq

    let inline toLinq<'a> expr =
        let args, body = expr |> QuotationToExpression |> translateExpr
        Expression.Lambda<'a>(body, args |> Array.ofList)

    let inline ofArity1 (expr: Expr<'a -> 'b>) = toLinq<Func<'a, 'b>> expr
    let inline ofArity2 (expr: Expr<'a -> 'b -> 'c>) = toLinq<Func<'a, 'b, 'c>> expr

module Session =

    type PrimaryKey =
        | Guid of Guid
        | String of string
        | Int of int32
        | Int64 of int64

    /// **Description**
    /// Deletes an entity
    ///
    /// **Parameters**
    ///   * `entity` - parameter of type `'a`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteEntity (entity: 'a) (session: IDocumentSession) = session.Delete(entity)

    /// **Description**
    /// Deletes an entity by given Guid identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `Guid`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteByGuid<'a> (id: Guid) (session: IDocumentSession) = session.Delete<'a>(id)

    /// **Description**
    /// Deletes an entity by given string identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `string`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteByString<'a> (id: string) (session: IDocumentSession) = session.Delete<'a>(id)

    /// **Description**
    /// Deletes an entity by given int identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteByInt<'a> (id: int) (session: IDocumentSession) = session.Delete<'a>(id)

    /// **Description**
    /// Deletes an entity by given int64 identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int64`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteByInt64<'a> (id: int64) (session: IDocumentSession) = session.Delete<'a>(id)

    /// **Description**
    /// Deletes an entity by given PrimaryKey
    ///
    /// **Parameters**
    ///   * `primaryKey` - parameter of type `PrimaryKey`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let delete<'a> (primaryKey: PrimaryKey) (session: IDocumentSession) =
        session
        |> match primaryKey with
           | Guid g -> deleteByGuid<'a> g
           | String s -> deleteByString<'a> s
           | Int i -> deleteByInt<'a> i
           | Int64 i -> deleteByInt64<'a> i

    /// **Description**
    ///
    /// Deletes entities by given expression
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/deletes.html#deleting-documents
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let deleteBy<'a> (predicate: Quotations.Expr<'a -> bool>) (session: IDocumentSession) =
        predicate |> Lambda.ofArity1 |> session.DeleteWhere

    /// **Description**
    /// Loads an entity by given Guid identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `Guid`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let loadByGuid<'a> (id: Guid) (session: IQuerySession) =
        session.Load<'a>(id) |> Option.ofNullableRecord

    /// **Description**
    /// Loads an entity by given int identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let loadByInt<'a> (id: int) (session: IQuerySession) =
        session.Load<'a>(id) |> Option.ofNullableRecord

    /// **Description**
    /// Loads an entity by given int64 identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int64`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let loadByInt64<'a> (id: int64) (session: IQuerySession) =
        session.Load<'a>(id) |> Option.ofNullableRecord

    /// **Description**
    /// Loads an entity by given string identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `string`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let loadByString<'a> (id: string) (session: IQuerySession) =
        session.Load<'a>(id) |> Option.ofNullableRecord

    /// **Description**
    /// Loads an entity by given PrimaryKey
    ///
    /// **Parameters**
    ///   * `primaryKey` - parameter of type `PrimaryKey`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `'a option`
    ///
    /// **Exceptions**
    ///
    let load<'a> (primaryKey: PrimaryKey) (session: IQuerySession) =
        session
        |> match primaryKey with
           | Guid g -> loadByGuid<'a> g
           | String s -> loadByString<'a> s
           | Int i -> loadByInt<'a> i
           | Int64 i -> loadByInt64<'a> i

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given Guid identifier
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `id` - parameter of type `Guid`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**


    let loadByGuidTask<'a> (cancellationToken: CancellationToken) (id: Guid) (session: IQuerySession) =
        session.LoadAsync<'a>(id, cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given int identifier
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `id` - parameter of type `int`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadByIntTask<'a> (cancellationToken: CancellationToken) (id: int32) (session: IQuerySession) =
        session.LoadAsync<'a>(id, cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given int64 identifier
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `id` - parameter of type `int64`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadByInt64Task<'a> (cancellationToken: CancellationToken) (id: int64) (session: IQuerySession) =
        session.LoadAsync<'a>(id, cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given string identifier
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `id` - parameter of type `string`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**


    let loadByStringTask<'a> (cancellationToken: CancellationToken) (id: string) (session: IQuerySession) =
        session.LoadAsync<'a>(id, cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given Guid identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `Guid`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///


    let loadByGuidAsync<'a> (id: Guid) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> loadByGuidTask<'a> ct id |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given int identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**


    let loadByIntAsync<'a> (id: int32) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> loadByIntTask<'a> ct id |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given int64 identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int64`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadByInt64Async<'a> (id: int64) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> loadByInt64Task<'a> ct id |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given string identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `string`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadByStringAsync<'a> (id: string) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> loadByStringTask<'a> ct id |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given PrimaryKey
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `primaryKey` - parameter of type `PrimaryKey`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadTask<'a> (cancellationToken: CancellationToken) (primaryKey: PrimaryKey) (session: IQuerySession) =
        session
        |> match primaryKey with
           | Guid g -> loadByGuidTask<'a> cancellationToken g
           | String s -> loadByStringTask<'a> cancellationToken s
           | Int i -> loadByIntTask<'a> cancellationToken i
           | Int64 i -> loadByInt64Task<'a> cancellationToken i

    /// **Description**
    ///
    /// Loads an entity asynchrnously by given PrimaryKey
    ///
    /// **Parameters**
    ///   * `primaryKey` - parameter of type `PrimaryKey`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/byid.html#loading-documents-by-id
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///
    let loadAsync<'a> (pk: PrimaryKey) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> loadTask<'a> ct pk |> Async.AwaitTask
        }

    /// **Description**
    /// Creates a queryable from a session
    ///
    /// **Parameters**
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/querying/linq/
    ///
    /// **Output Type**
    ///   * `Linq.IMartenQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let query<'a> (session: IQuerySession) = session.Query<'a>()

    /// **Description**
    /// Use raw sql to query for an entity.
    ///
    /// **Parameters**
    ///   * `sqlString` - parameter of type `string`
    ///   * `parameters` - parameter of type `obj []`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/querying/linq/
    ///
    /// **Output Type**
    ///   * `Collections.Generic.IReadOnlyList<'a>`
    ///
    /// **Exceptions**
    ///
    let sql<'a> (sqlString: string) (parameters: obj array) (session: IQuerySession) =
        session.Query<'a>(sqlString, parameters)

    /// **Description**
    /// Use raw sql to query for an entity asynchronously.
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `sqlString` - parameter of type `string`
    ///   * `parameters` - parameter of type `obj []`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/querying/linq/
    ///
    /// **Output Type**
    ///   * `Tasks.Task<Collections.Generic.IReadOnlyList<'a>>`
    ///
    /// **Exceptions**
    ///
    let sqlTask<'a>
        (cancellationToken: CancellationToken)
        (sqlString: string)
        (parameters: obj[])
        (session: IQuerySession)
        =
        session.QueryAsync<'a>(sqlString, cancellationToken, parameters = parameters)

    /// **Description**
    /// Use raw sql to query for an entity asynchronously.
    ///
    /// **Parameters**
    ///   * `sqlString` - parameter of type `string`
    ///   * `parameters` - parameter of type `obj []`
    ///   * `session` - parameter of type `IQuerySession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/querying/linq/
    ///
    /// **Output Type**
    ///   * `Async<Collections.Generic.IReadOnlyList<'a>>`
    ///
    /// **Exceptions**
    ///
    let sqlAsync<'a> (sqlString: string) (parameters: obj[]) (session: IQuerySession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> sqlTask<'a> ct sqlString parameters |> Async.AwaitTask
        }

    /// **Description**
    /// Saves changes to a unit of work.
    ///
    /// **Parameters**
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/storing.html
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let saveChanges (session: IDocumentSession) = session.SaveChanges()

    /// **Description**
    /// Saves changes to a unit of work asynchronously.
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/storing.html
    ///
    /// **Output Type**
    ///   * `Task.Task`
    ///
    /// **Exceptions**
    ///
    let saveChangesTask (cancellationToken: CancellationToken) (session: IDocumentSession) =
        session.SaveChangesAsync(cancellationToken)

    /// **Description**
    /// Saves changes to a unit of work asynchronously.
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/storing.html
    ///
    /// **Output Type**
    ///   * `Async<unit>`
    ///
    /// **Exceptions**
    ///
    let saveChangesAsync (session: IDocumentSession) =
        async {
            let! ct = Async.CancellationToken
            return! session |> saveChangesTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Queue up persisting of given entity
    ///
    /// **Parameters**
    ///   * `entity` - parameter of type `'a`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/storing.html
    ///
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let storeSingle (entity: 'a) (session: IDocumentSession) = session.Store([| entity |])

    /// **Description**
    ///
    /// Queue up persisting of given entities
    ///
    /// **Parameters**
    ///   * `entities` - parameter of type `seq<'a>`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/storing.html
    ///
    ///
    /// **Output Type**
    ///   * `unit`
    ///
    /// **Exceptions**
    ///
    let storeMany (entities: #seq<'a>) (session: IDocumentSession) =
        entities |> Seq.toArray |> session.Store

    /// **Description**
    ///
    /// Creates a IPatchExpression for given Guid identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `Guid`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/plv8.html#the-patching-api
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patchByGuid<'a> (id: Guid) (session: IDocumentSession) = session.Patch<'a>(id)

    /// **Description**
    ///
    /// Creates a IPatchExpression for given string identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `string`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/plv8.html#the-patching-api
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patchByString<'a> (id: string) (session: IDocumentSession) = session.Patch<'a>(id)

    /// **Description**
    ///
    /// Creates a IPatchExpression for given int identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/plv8.html#the-patching-api
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patchByInt<'a> (id: int) (session: IDocumentSession) = session.Patch<'a>(id)

    /// **Description**
    ///
    /// Creates a IPatchExpression for given int64 identifier
    ///
    /// **Parameters**
    ///   * `id` - parameter of type `int64`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/plv8.html#the-patching-api
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patchByInt64<'a> (id: int64) (session: IDocumentSession) = session.Patch<'a>(id)

    /// **Description**
    ///
    /// Creates a IPatchExpression for given PrimaryKey
    ///
    /// **Parameters**
    ///   * `primaryKey` - parameter of type `PrimaryKey`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    ///
    /// **Reference**
    ///
    /// https://martendb.io/documents/plv8.html#the-patching-api
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patch (primaryKey: PrimaryKey) (session: IDocumentSession) =
        session
        |> match primaryKey with
           | Guid g -> patchByGuid g
           | String s -> patchByString s
           | Int i -> patchByInt i
           | Int64 i -> patchByInt64 i

    /// **Description**
    ///
    /// Creates a IPatchExpression for given expression
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `session` - parameter of type `IDocumentSession`
    ///
    /// **Output Type**
    ///   * `Patching.IPatchExpression<'a>`
    ///
    /// **Exceptions**
    ///
    let patchWhere (predicate: Quotations.Expr<'a -> bool>) (session: IDocumentSession) =
        predicate |> Lambda.ofArity1 |> session.Patch

    module Patch =

        /// **Description**
        ///
        /// Sets a value using the patch api
        ///
        /// **Parameters**
        ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> 'b)>`
        ///   * `newVal` - parameter of type `'b`
        ///   * `pExpr` - parameter of type `Patching.IPatchExpression<'a>`
        ///
        /// **Output Type**
        ///   * `unit`
        ///
        /// **Exceptions**
        ///
        let set<'a, 'b> (keySelector: Quotations.Expr<'a -> 'b>) (newVal: 'b) (pExpr: Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 keySelector
            pExpr.Set<'b>(func, newVal)

        /// **Description**
        ///
        /// Increments a value by one using the patch api
        ///
        /// **Parameters**
        ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> int)>`
        ///   * `pExpr` - parameter of type `Patching.IPatchExpression<'a>`
        ///
        /// **Output Type**
        ///   * `unit`
        ///
        /// **Exceptions**
        ///
        let inc<'a> (keySelector: Quotations.Expr<'a -> int>) (pExpr: Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 keySelector
            pExpr.Increment(func, 1)


        /// **Description**
        ///
        /// Increments a value by given value using the patch api
        ///
        /// **Parameters**
        ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> int)>`
        ///   * `inc` - parameter of type `int`
        ///   * `pExpr` - parameter of type `Patching.IPatchExpression<'a>`
        ///
        /// **Output Type**
        ///   * `unit`
        ///
        /// **Exceptions**
        ///
        let incPlural<'a> (keySelector: Quotations.Expr<'a -> int>) (inc: int) (pExpr: Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 keySelector
            pExpr.Increment(func, inc)

module Queryable =
    open System.Linq

    /// **Description**
    ///
    /// Returns the only element of a sequence, and throws an exception if there is not exactly one element in the sequence.
    /// Equivalent to `queryable.Single`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'a`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    let exactlyOne (q: IQueryable<'a>) = q.Single()

    /// **Description**
    ///
    /// Returns the only element of a sequence, and throws an exception if there is not exactly one element in the sequence.
    /// Equivalent to `queryable.SingleAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a>`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    let exactlyOneTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) = q.SingleAsync(cancellationToken)

    /// **Description**
    ///
    /// Returns the only element of a sequence, and throws an exception if there is not exactly one element in the sequence.
    /// Equivalent to `queryable.SingleAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<'a>`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    let exactlyOneAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> exactlyOneTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Filters a sequence of values based on a predicate.
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let filter (predicate: Quotations.Expr<'a -> bool>) (q: IQueryable<'a>) = predicate |> Lambda.ofArity1 |> q.Where

    /// **Description**
    ///
    /// Returns the first element of a sequence.
    /// Equivalent to `queryable.First`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'a`
    ///
    /// **Exceptions**
    ///
    let head (q: IQueryable<'a>) = q.First()

    /// **Description**
    ///
    /// Returns the first element of a sequence.
    /// Equivalent to `queryable.FirstAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a>`
    ///
    /// **Exceptions**
    ///
    let headTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) = q.FirstAsync(cancellationToken)

    /// **Description**
    ///
    /// Returns the first element of a sequence.
    /// Equivalent to `queryable.FirstAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<'a>`
    ///
    /// **Exceptions**
    ///
    let headAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> headTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Projects each element of a sequence into a new form.
    /// Equivalent to `queryable.Select`
    ///
    /// **Parameters**
    ///   * `mapper` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IQueryable<'b>`
    ///
    /// **Exceptions**
    ///
    let map (mapper: Quotations.Expr<'a -> 'b>) (q: IQueryable<'a>) = mapper |> Lambda.ofArity1 |> q.Select

    /// **Description**
    ///
    /// Creates a list from a queryable.  This will execute the query.
    /// Equivalent to `queryable.ToList`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Collections.Generic.List<'a>`
    ///
    /// **Exceptions**
    ///
    let toList (q: IQueryable<'a>) = q.ToList()

    /// **Description**
    ///
    /// Creates a list from a queryable.  This will execute the query.
    /// Equivalent to `queryable.ToListAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<Collections.Generic.IReadOnlyList<'a>>`
    ///
    /// **Exceptions**
    ///
    let toListTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) = q.ToListAsync(cancellationToken)

    /// **Description**
    ///
    /// Creates a list from a queryable.  This will execute the query.
    /// Equivalent to `queryable.ToListAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<Collections.Generic.IReadOnlyList<'a>>`
    ///
    /// **Exceptions**
    ///
    let toListAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> toListTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Returns the only element of a sequence, or a `None` if the sequence is empty; this method throws an exception if there is more than one element in the sequence.
    /// Equivalent to `queryable.SingleOrDefault`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'a option`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    let tryExactlyOne (q: IQueryable<'a>) =
        q.SingleOrDefault() |> Option.ofNullableRecord

    /// **Description**
    ///
    /// Returns the only element of a sequence, or a `None` if the sequence is empty; this method throws an exception if there is more than one element in the sequence.
    /// Equivalent to `queryable.SingleOrDefaultAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    let tryExactlyOneTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) =
        q.SingleOrDefaultAsync(cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Returns the only element of a sequence, or a `None` if the sequence is empty; this method throws an exception if there is more than one element in the sequence.
    /// Equivalent to `queryable.SingleOrDefaultAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///   * `System.InvalidOperationException`: source has more than one element
    ///
    let tryExactlyOneAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> tryExactlyOneTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Returns the first element of a sequence, or a `None` if the sequence contains no elements.
    /// Equivalent to `queryable.FirstOrDefault`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'a option`
    ///
    /// **Exceptions**
    ///
    let tryHead (q: IQueryable<'a>) =
        q.FirstOrDefault() |> Option.ofNullableRecord

    /// **Description**
    ///
    /// Returns the first element of a sequence, or a `None` if the sequence contains no elements.
    /// Equivalent to `queryable.FirstOrDefaultAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<'a option>`
    ///
    /// **Exceptions**
    ///
    let tryHeadTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) =
        q.FirstOrDefaultAsync(cancellationToken) |> Task.map Option.ofNullableRecord

    /// **Description**
    ///
    /// Returns the first element of a sequence, or a `None` if the sequence contains no elements.
    /// Equivalent to `queryable.FirstOrDefaultAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<'a option>`
    ///
    /// **Exceptions**
    ///
    let tryHeadAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> tryHeadTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.Count`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `int`
    ///
    /// **Exceptions**
    ///
    let countWhere<'a> (predicate: Quotations.Expr<'a -> bool>) (q: IQueryable<'a>) =
        predicate |> Lambda.ofArity1 |> q.Count

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.Count`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `int`
    ///
    /// **Exceptions**
    ///
    let count (q: IQueryable<'a>) = q.Count()

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.LongCount`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `int64`
    ///
    /// **Exceptions**
    ///
    let countLongWhere (predicate: Quotations.Expr<'a -> bool>) (q: IQueryable<'a>) =
        predicate |> Lambda.ofArity1 |> q.LongCount

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.LongCount`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `int64`
    ///
    /// **Exceptions**
    ///
    let countLong (q: IQueryable<'a>) = q.LongCount()
    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.CountAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<int>`
    ///
    /// **Exceptions**
    ///
    let countTask (cancellationToken: CancellationToken) (q: IQueryable<'a>) = q.CountAsync(cancellationToken)

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.CountAsync`
    ///
    /// **Parameters**
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<int>`
    ///
    /// **Exceptions**
    ///
    let countAsync (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> countTask ct |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.CountAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<int>`
    ///
    /// **Exceptions**
    ///
    let countWhereTask
        (cancellationToken: CancellationToken)
        (predicate: Quotations.Expr<'a -> bool>)
        (q: IQueryable<'a>)
        =
        q.CountAsync(Lambda.ofArity1 predicate, cancellationToken)

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.CountAsync`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<int>`
    ///
    /// **Exceptions**
    ///
    let countWhereAsync (predicate: Quotations.Expr<'a -> bool>) (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! q |> countWhereTask ct predicate |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.LongCountAsync`
    ///
    /// **Parameters**
    ///   * `cancellationToken` - parameter of type `CancellationToken`
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Tasks.Task<int64>`
    ///
    /// **Exceptions**
    ///
    let countLongWhereTask
        (cancellationToken: CancellationToken)
        (predicate: Quotations.Expr<'a -> bool>)
        (q: IQueryable<'a>)
        =
        q.LongCountAsync(Lambda.ofArity1 predicate, cancellationToken)

    /// **Description**
    ///
    /// Returns the number of elements in the specified sequence that satisfies a condition.
    /// Equivalent to `queryable.LongCountAsync`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> bool)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `Async<int64>`
    ///
    /// **Exceptions**
    ///
    let countLongWhereAsync (predicate: Quotations.Expr<'a -> bool>) (q: IQueryable<'a>) =
        async {
            let! ct = Async.CancellationToken
            return! countLongWhereTask ct predicate q |> Async.AwaitTask
        }

    /// **Description**
    ///
    /// Invokes a projection function on each element of a generic `System.Linq.IQueryable1` and returns the minimum resulting value.
    /// Equivalent to `queryable.Min`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'b`
    ///
    /// **Exceptions**
    ///
    let min<'a, 'b when 'b: comparison> (predicate: Quotations.Expr<'a -> 'b>) (q: IQueryable<'a>) =
        predicate |> Lambda.ofArity1 |> q.Min

    /// **Description**
    ///
    /// Invokes a projection function on each element of a generic System.Linq.IQueryable1` and returns the maximum resulting value.
    /// Equivalent to `queryable.Max`
    ///
    /// **Parameters**
    ///   * `predicate` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `'b`
    ///
    /// **Exceptions**
    ///
    let max<'a, 'b when 'b: comparison> (predicate: Quotations.Expr<'a -> 'b>) (q: IQueryable<'a>) =
        predicate |> Lambda.ofArity1 |> q.Max

    /// **Description**
    ///
    /// Sorts the elements of a sequence in ascending order according to a key.
    /// Equivalent to `queryable.OrderBy`
    ///
    /// **Parameters**
    ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IOrderedQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let orderBy<'a, 'b when 'b: comparison> (keySelector: Quotations.Expr<'a -> 'b>) (q: IQueryable<'a>) =
        keySelector |> Lambda.ofArity1 |> q.OrderBy

    /// **Description**
    ///
    /// Sorts the elements of a sequence in descending order according to a key.
    /// Equivalent to `queryable.OrderByDescending`
    ///
    /// **Parameters**
    ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IOrderedQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let orderByDescending<'a, 'b when 'b: comparison> (keySelector: Quotations.Expr<'a -> 'b>) (q: IQueryable<'a>) =
        keySelector |> Lambda.ofArity1 |> q.OrderByDescending

    /// **Description**
    ///
    /// Performs a subsequent ordering of the elements in a sequence in ascending order according to a key.
    /// Equivalent to `queryable.ThenBy`
    ///
    /// **Parameters**
    ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `oq` - parameter of type `IOrderedQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IOrderedQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let thenBy (keySelector: Quotations.Expr<'a -> 'b>) (oq: IOrderedQueryable<'a>) =
        keySelector |> Lambda.ofArity1 |> oq.ThenBy

    /// **Description**
    ///
    /// Performs a subsequent ordering of the elements in a sequence in descending order, according to a key.
    /// Equivalent to `queryable.ThenByDescending`
    ///
    /// **Parameters**
    ///   * `keySelector` - parameter of type `Quotations.Expr<('a -> 'b)>`
    ///   * `oq` - parameter of type `IOrderedQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IOrderedQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let thenByDescending (keySelector: Quotations.Expr<'a -> 'b>) (oq: IOrderedQueryable<'a>) =
        keySelector |> Lambda.ofArity1 |> oq.ThenByDescending

    /// **Description**
    ///
    /// Bypasses a specified number of elements in a sequence and then returns the remaining elements.
    /// Equivalent to `queryable.Skip`
    ///
    /// **Parameters**
    ///   * `amount` - parameter of type `int`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let skip (amount: int) (q: IQueryable<'a>) = q.Skip(amount)
    /// **Description**
    ///
    /// Returns a specified number of contiguous elements from the start of a sequence.
    /// Equivalent to `queryable.Take`
    ///
    /// **Parameters**
    ///   * `amount` - parameter of type `int`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let take (amount: int) (q: IQueryable<'a>) = q.Take(amount)

    /// **Description**
    ///
    /// Bypasses a specified number of elements in a sequence, then returns a specified number of contiguous elements from the start of a sequence.
    /// Equivalent to `queryable.Skip().Take()`
    ///
    /// **Parameters**
    ///   * `skipped` - parameter of type `int`
    ///   * `takeAmount` - parameter of type `int`
    ///   * `q` - parameter of type `IQueryable<'a>`
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    /// **Exceptions**
    ///
    let paging (skipped: int) (takeAmount: int) (q: IQueryable<'a>) = q |> skip skipped |> take takeAmount

    /// **Description**
    ///
    /// Fetches a single related document.else
    ///
    /// **Parameters**
    ///   * `selector` - foreign key selector
    ///   * `action` - related document callback
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/include.html#join-a-single-document
    ///
    /// **Exceptions**
    ///
    let includeSingle<'a, 'b> (selector: Quotations.Expr<'a -> obj>) (action: 'b -> unit) (q: IQueryable<'a>) =
        q.Include(selector |> Lambda.ofArity1, action)

    /// **Description**
    ///
    /// Fetches a collection of related documents.
    /// Appends the documents to your specified `IList<'b>` parameter
    ///
    /// **Parameters**
    ///   * `selector` - foreign key selector
    ///   * `list` - the list the related documents will be appended to.
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/include.html#join-many-documents
    ///
    /// **Exceptions**
    ///
    let includeList<'a, 'b>
        (selector: Quotations.Expr<'a -> obj>)
        (list: Collections.Generic.IList<'b>)
        (q: IQueryable<'a>)
        =
        q.Include(selector |> Lambda.ofArity1, list)

    /// **Description**
    ///
    /// Fetches a collection of related documents.
    /// Appends the document's primary key and data to the dictionary.
    ///
    /// **Parameters**
    ///   * `selector` - foreign key selector
    ///   * `dict` - the dictionary the related documents will be appended to.
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `IQueryable<'a>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/include.html#join-many-documents
    ///
    /// **Exceptions**
    ///
    let includeDict<'a, 'b, 'c>
        (selector: Quotations.Expr<'a -> obj>)
        (dict: Collections.Generic.Dictionary<'b, 'c>)
        (q: IQueryable<'a>)
        =
        q.Include(selector |> Lambda.ofArity1, dict)

    /// **Description**
    ///
    /// Fetches a paginated list of elements.
    ///
    /// **Parameters**
    ///   * `pageNumber` - the page number.
    ///   * `pageSize` - the page size
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `IPagedList<'a>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/paging.html#paging
    ///
    /// **Exceptions**
    ///
    let pagedList (pageNumber: int) (pageSize: int) (q: IQueryable<'a>) = q.ToPagedList(pageNumber, pageSize)

    /// **Description**
    ///
    /// Fetches a paginated list of elements asynchronously.
    ///
    /// **Parameters**
    ///   * `pageNumber` - the page number.
    ///   * `pageSize` - the page size.
    ///   * `ct` - the cancellation token.
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `Task<IPagedList<'a>>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/paging.html#paging
    ///
    /// **Exceptions**
    ///
    let pagedListTask (pageNumber: int) (pageSize: int) (ct: CancellationToken) (q: IQueryable<'a>) =
        q.ToPagedListAsync(pageNumber, pageSize, ct)

    /// **Description**
    ///
    /// Fetches a paginated list of elements asynchronously.
    ///
    /// **Parameters**
    ///   * `pageNumber` - the page number.
    ///   * `pageSize` - the page size.
    ///   * `q` - your `IQueryable` parameter
    ///
    /// **Output Type**
    ///   * `Async<IPagedList<'a>>`
    ///
    ///  **Reference**
    /// https://martendb.io/documents/querying/linq/paging.html#paging
    ///
    /// **Exceptions**
    ///
    let pagedListAsync (pageNumber: int) (pageSize: int) (q: IQueryable<'a>) =
        async {
            let! cancellationToken = Async.CancellationToken
            return! q.ToPagedListAsync(pageNumber, pageSize, cancellationToken) |> Async.AwaitTask
        }
