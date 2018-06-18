namespace Marten

open System

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

    module Task =
        open System.Threading.Tasks
        let map f (t : Task<_>) = t.ContinueWith(fun (x : Task<_>) -> x.Result |> f)



module Option =
    /// Creates an option from a potentially null record type
    let ofNullableRecord record =
        if (record |> box |> isNull) then None
        else Some record


open System
open System.Linq.Expressions
open System.Threading

module Lambda =
    open System.Linq.Expressions
    open Microsoft.FSharp.Quotations

    open Microsoft.FSharp.Linq
    open Microsoft.FSharp.Linq.RuntimeHelpers
    open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
    let rec translateExpr (linq:Expression) =
        match linq with
        | :? MethodCallExpression as mc ->
            match mc.Arguments.[0] with
            | :? LambdaExpression as le ->
                let args, body = translateExpr le.Body
                le.Parameters.[0] :: args, body
            | :? System.Linq.Expressions.MemberExpression as me ->
                // Not sure what to do here.  I'm sure there will be hidden bugs
                [], linq
            | _ as unknown ->
                // Not sure what to do here.  I'm sure there will be hidden bugs
                // x.GetType() |> printfn "x: %A"
                [], linq
        | _ -> [], linq

    let inline toLinq<'a> expr =
        let args, body = expr |> QuotationToExpression  |> translateExpr
        Expression.Lambda<'a>(body, args |> Array.ofList)
    let inline ofArity1 (expr : Expr<'a -> 'b>) =
        toLinq<Func<'a,'b>> expr
    let inline ofArity2 (expr : Expr<'a -> 'b -> 'c>) =
        toLinq<Func<'a,'b,'c>> expr


module Session =
    type PrimaryKey =
        | Guid of Guid
        | String of string
        | Int of int32
        | Int64 of int64

    let deleteEntity (entity : 'a) (session : IDocumentSession) =
        session.Delete(entity)
    let deleteByGuid<'a> (id : Guid) (session : IDocumentSession) =
        session.Delete<'a>(id)
    let deleteByString<'a> (id : string) (session : IDocumentSession) =
        session.Delete<'a>(id)
    let deleteByInt<'a> (id : int) (session : IDocumentSession) =
        session.Delete<'a>(id)
    let deleteByInt64<'a> (id : int64) (session : IDocumentSession) =
        session.Delete<'a>(id)

    let delete<'a> (pk : PrimaryKey) (session : IDocumentSession) =
        session
        |> match pk with
           | Guid g -> deleteByGuid<'a> g
           | String s -> deleteByString<'a> s
           | Int i -> deleteByInt<'a> i
           | Int64 i -> deleteByInt64<'a> i

    let deleteBy<'a> (f : Quotations.Expr<'a -> bool>) (session : IDocumentSession) =
        f
        |> Lambda.ofArity1
        |> session.DeleteWhere


    let loadByGuid<'a> (id : Guid) (session : IQuerySession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByInt<'a> (id : int) (session : IQuerySession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByInt64<'a> (id : int64) (session : IQuerySession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByString<'a> (id : string) (session : IQuerySession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord

    let load<'a> (pk : PrimaryKey) (session : IQuerySession) =
        session
        |> match pk with
           | Guid g -> loadByGuid<'a> g
           | String s -> loadByString<'a> s
           | Int i -> loadByInt<'a> i
           | Int64 i -> loadByInt64<'a> i

    let loadByGuidTaskCt<'a> (cancellationToken : CancellationToken) (id : Guid) (session : IQuerySession) =
        session.LoadAsync<'a>(id,cancellationToken)
        |> Task.map Option.ofNullableRecord

    let loadByGuidTask<'a> (id : Guid) (session : IQuerySession) =
        loadByGuidTaskCt<'a> CancellationToken.None id session

    let loadByIntTaskCt<'a> (cancellationToken : CancellationToken) (id : int32) (session : IQuerySession) =
        session.LoadAsync<'a>(id,cancellationToken)
        |> Task.map Option.ofNullableRecord

    let loadByIntTask<'a> (id : int32) (session : IQuerySession) =
        loadByIntTaskCt<'a> CancellationToken.None id session

    let loadByInt64TaskCt<'a> (cancellationToken : CancellationToken) (id : int64) (session : IQuerySession) =
        session.LoadAsync<'a>(id,cancellationToken)
        |> Task.map Option.ofNullableRecord

    let loadByInt64Task<'a> (id : int64) (session : IQuerySession) =
        loadByInt64TaskCt<'a> CancellationToken.None id session

    let loadByStringTaskCt<'a> (cancellationToken : CancellationToken) (id : string) (session : IQuerySession) =
        session.LoadAsync<'a>(id,cancellationToken)
        |> Task.map Option.ofNullableRecord

    let loadByStringTask<'a> (id : string ) (session : IQuerySession) =
        loadByStringTaskCt<'a> CancellationToken.None id session

    let loadByGuidAsync<'a> (id : Guid) (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> loadByGuidTaskCt<'a> ct id
            |> Async.AwaitTask
    }

    let loadByIntAsync<'a> (id : int32) (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> loadByIntTaskCt<'a> ct id
            |> Async.AwaitTask
    }

    let loadByInt64Async<'a> (id : int64) (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> loadByInt64TaskCt<'a> ct id
            |> Async.AwaitTask
    }

    let loadByStringAsync<'a> (id : string ) (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> loadByStringTaskCt<'a> ct id
            |> Async.AwaitTask
    }


    let loadTaskCt<'a> (cancellationToken : CancellationToken) (pk : PrimaryKey) (session : IQuerySession) =
        session
        |>  match pk with
            | Guid g -> loadByGuidTaskCt<'a> cancellationToken g
            | String s -> loadByStringTaskCt<'a> cancellationToken s
            | Int i -> loadByIntTaskCt<'a> cancellationToken i
            | Int64 i -> loadByInt64TaskCt<'a> cancellationToken i

    let loadTask<'a> (pk : PrimaryKey) (session : IQuerySession) =
        loadTaskCt CancellationToken.None pk session

    let loadAsync<'a> (pk : PrimaryKey) (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> loadTaskCt<'a> ct pk
            |> Async.AwaitTask
    }

    let query<'a> (session : IQuerySession) =
        session.Query<'a>()

    let sql<'a> string parameters (session : IQuerySession)  =
        session.Query<'a>(string, parameters)

    let sqlTaskCt<'a> (cancellationToken : CancellationToken) string parameters  (session : IQuerySession)=
        session.QueryAsync<'a>(string, cancellationToken, parameters=parameters)

    let sqlTask<'a> string parameters  (session : IQuerySession)=
        sqlTaskCt CancellationToken.None string parameters session

    let sqlAsync<'a> string parameters (session : IQuerySession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> sqlTaskCt<'a> ct string parameters
            |> Async.AwaitTask
    }


    let saveChanges (session : IDocumentSession) =
        session.SaveChanges()

    let saveChangesTaskCt (cancellationToken : CancellationToken) (session : IDocumentSession) =
        session.SaveChangesAsync(cancellationToken)

    let saveChangesTask (session : IDocumentSession) =
        saveChangesTaskCt CancellationToken.None

    let saveChangesAsync (session : IDocumentSession) = async {
        let! ct = Async.CancellationToken
        return!
            session
            |> saveChangesTaskCt ct
            |> Async.AwaitTask
    }

    let storeSingle entity (session : IDocumentSession) =
        session.Store([|entity|])
    let storeMany (entities : #seq<_>) (session : IDocumentSession) =
        entities
        |> Seq.toArray
        |> session.Store

    // PATCH.
    let patch<'a> (id : Guid) (session : IDocumentSession) =
        session.Patch<'a>(id)

    module Patch =
        let set<'a, 'b> (part : Quotations.Expr<'a -> 'b>) (newVal : 'b) (pExpr : Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 part
            pExpr.Set<'b>(func, newVal)
        let inc<'a> (part : Quotations.Expr<'a -> int>) (pExpr : Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 part
            pExpr.Increment(func, 1)
        let incPlural<'a> (part : Quotations.Expr<'a -> int>) inc (pExpr : Patching.IPatchExpression<'a>) =
            let func = Lambda.ofArity1 part
            pExpr.Increment(func, inc)

module Queryable =
    open System.Linq
    open Marten.Linq
    // not supported
    // let aggregate (f : Quotations.Expr<'a -> 'a -> 'a>) (q : IQueryable<'a>) =
    //     f
    //     |> Lambda.ofArity2
    //     |> q.Aggregate

    // let fold (seed :'a) (f : Quotations.Expr<'a -> 'b -> 'a>) (q : IQueryable<'b>) =
    //     q.Aggregate(
    //         seed,
    //         f|> Lambda.ofArity2)

    let exactlyOne (q : IQueryable<'a>) =
        q.Single()

    let exactlyOneTaskCt (cancellationToken : CancellationToken) (q : IQueryable<'a>) =
        q.SingleAsync(cancellationToken)

    let exactlyOneTask (q : IQueryable<'a>) =
        exactlyOneTaskCt CancellationToken.None q

    let exactlyOneAsync (q : IQueryable<'a>) = async {
        let! ct = Async.CancellationToken
        return!
            q
            |> exactlyOneTaskCt ct
            |> Async.AwaitTask
    }

    let filter (f : Quotations.Expr<'a -> bool>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Where

    let head (q : IQueryable<'a>) =
        q.First()

    let headTaskCt (cancellationToken : CancellationToken)  (q : IQueryable<'a>) =
        q.FirstAsync(cancellationToken)

    let headTask (q : IQueryable<'a>) =
        headTaskCt CancellationToken.None q

    let headAsync (q : IQueryable<'a>) = async {
        let! ct = Async.CancellationToken
        return!
            q
            |> headTaskCt ct
            |> Async.AwaitTask
    }

    let map (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Select

    let toList (q : IQueryable<'a>) =
        q.ToList()

    let toListTaskCt (cancellationToken : CancellationToken) (q : IQueryable<'a>) =
        q.ToListAsync(cancellationToken)

    let toListTask (q : IQueryable<'a>) =
        toListTaskCt CancellationToken.None q

    let toListAsync (q : IQueryable<'a>) = async {
        let! ct = Async.CancellationToken
        return!
            q
            |> toListTaskCt ct
            |> Async.AwaitTask
    }

    let tryExactlyOne (q : IQueryable<'a>) =
        q.SingleOrDefault()
        |> Option.ofNullableRecord

    let tryExactlyOneTaskCt (cancellationToken : CancellationToken) (q : IQueryable<'a>) =
        q.SingleOrDefaultAsync(cancellationToken)
        |> Task.map Option.ofNullableRecord

    let tryExactlyOneTask (q : IQueryable<'a>) =
        tryExactlyOneTaskCt CancellationToken.None q

    let tryExactlyOneAsync (q : IQueryable<'a>) = async {
        let! ct = Async.CancellationToken
        return!
            q
            |> tryExactlyOneTaskCt ct
            |> Async.AwaitTask
    }

    let tryHead (q : IQueryable<'a>) =
        q.FirstOrDefault()
        |> Option.ofNullableRecord

    let tryHeadTaskCt (cancellationToken : CancellationToken) (q : IQueryable<'a>) =
        q.FirstOrDefaultAsync(cancellationToken)
        |> Task.map Option.ofNullableRecord

    let tryHeadTask (q : IQueryable<'a>) =
        tryHeadTaskCt CancellationToken.None q

    let tryHeadAsync (q : IQueryable<'a>) = async {
        let! ct = Async.CancellationToken
        return!
            q
            |> tryHeadTaskCt ct
            |> Async.AwaitTask
    }

    let count<'a> (f : Quotations.Expr<'a -> bool>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Count

    let countAsync<'a> (q: IQueryable<'a>) = q.CountAsync()
    let countAsyncWhere<'a> f (q: IQueryable<'a>) = q.CountAsync(Lambda.ofArity1 f)

    let min<'a, 'b when 'b : comparison> (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Min

    let max<'a, 'b when 'b : comparison> (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Max

    let orderBy<'a, 'b when 'b : comparison> (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.OrderBy

    let orderByDescending<'a, 'b when 'b : comparison> (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.OrderByDescending

    let thenBy (f : Quotations.Expr<'a -> 'b>) (oq : IOrderedQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> oq.ThenBy

    let thenByDescending (f : Quotations.Expr<'a -> 'b>) (oq : IOrderedQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> oq.ThenByDescending

    let skip (amount : int) (q : IQueryable<'a>) =
        q.Skip(amount)
    let take (amount : int) (q : IQueryable<'a>) =
        q.Take(amount)
    let paging (skipped : int) (takeAmount : int) (q : IQueryable<'a>) =
        q
        |> skip skipped
        |> take takeAmount
