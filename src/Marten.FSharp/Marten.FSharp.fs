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
        match pk with
        | Guid g -> deleteByGuid<'a> g
        | String s -> deleteByString<'a> s
        | Int i -> deleteByInt<'a> i
        | Int64 i -> deleteByInt64<'a> i

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
        match pk with
        | Guid g -> loadByGuid<'a> g
        | String s -> loadByString<'a> s
        | Int i -> loadByInt<'a> i
        | Int64 i -> loadByInt64<'a> i


    let loadByGuidTask<'a> (id : Guid) (session : IQuerySession) =
        session.LoadAsync<'a>(id)
        |> Task.map Option.ofNullableRecord

    let loadByIntTask<'a> (id : int32) (session : IQuerySession) =
        session.LoadAsync<'a>(id)
        |> Task.map Option.ofNullableRecord

    let loadByInt64Task<'a> (id : int64) (session : IQuerySession) =
        session.LoadAsync<'a>(id)
        |> Task.map Option.ofNullableRecord

    let loadByStringTask<'a> (id : string ) (session : IQuerySession) =
        session.LoadAsync<'a>(id)
        |> Task.map Option.ofNullableRecord

    let loadByGuidAsync<'a> (id : Guid) (session : IQuerySession) =
        session
        |> loadByGuidTask<'a>(id)
        |> Async.AwaitTask

    let loadByIntAsync<'a> (id : int32) (session : IQuerySession) =
        session
        |> loadByIntTask<'a>(id)
        |> Async.AwaitTask

    let loadByInt64Async<'a> (id : int64) (session : IQuerySession) =
        session
        |> loadByInt64Task<'a>(id)
        |> Async.AwaitTask

    let loadByStringAsync<'a> (id : string ) (session : IQuerySession) =
        session
        |> loadByStringTask<'a>(id)
        |> Async.AwaitTask

    let loadTask<'a> (pk : PrimaryKey) (session : IQuerySession) =
        session
        |>  match pk with
            | Guid g -> loadByGuidTask<'a> g
            | String s -> loadByStringTask<'a> s
            | Int i -> loadByIntTask<'a> i
            | Int64 i -> loadByInt64Task<'a> i
    let loadAsync<'a> (pk : PrimaryKey) (session : IQuerySession) =
        session
        |> loadTask<'a> pk
        |> Async.AwaitTask

    let query<'a> (session : IQuerySession) =
        session.Query<'a>()

    let sql<'a> string parameters (session : IQuerySession)  =
        session.Query<'a>(string, parameters)
    let sqlTask<'a> string parameters  (session : IQuerySession)=
        session.QueryAsync<'a>(string, parameters=parameters)
    let sqlAsync<'a> string parameters (session : IQuerySession) =
        session
        |> sqlTask<'a>  string parameters
        |> Async.AwaitTask


    let saveChanges (session : IDocumentSession) =
        session.SaveChanges()
    let saveChangesTask (session : IDocumentSession) =
        session.SaveChangesAsync()
    let saveChangesAsync (session : IDocumentSession) =
        session
        |> saveChangesTask
        |> Async.AwaitTask

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
    let exactlyOneTask (q : IQueryable<'a>) =
        q.SingleAsync()
    let exactlyOneAsync (q : IQueryable<'a>) =
        q
        |> exactlyOneTask
        |> Async.AwaitTask

    let filter (f : Quotations.Expr<'a -> bool>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Where

    let head (q : IQueryable<'a>) =
        q.First()
    let headTask (q : IQueryable<'a>) =
        q.FirstAsync()
    let headAsync (q : IQueryable<'a>) =
        q
        |> headTask
        |> Async.AwaitTask


    let map (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Select

    let toList (q : IQueryable<'a>) =
        q.ToList()
    let toListTask (q : IQueryable<'a>) =
        q.ToListAsync()
    let toListAsync (q : IQueryable<'a>) =
        q
        |> toListTask
        |> Async.AwaitTask

    let tryExactlyOne (q : IQueryable<'a>) =
        q.SingleOrDefault()
        |> Option.ofNullableRecord

    let tryExactlyOneTask (q : IQueryable<'a>) =
        q.SingleOrDefaultAsync()
        |> Task.map Option.ofNullableRecord
    let tryExactlyOneAsync (q : IQueryable<'a>) =
        q
        |> tryExactlyOneTask
        |> Async.AwaitTask

    let tryHead (q : IQueryable<'a>) =
        q.FirstOrDefault()
        |> Option.ofNullableRecord

    let tryHeadTask (q : IQueryable<'a>) =
        q.FirstOrDefaultAsync()
        |> Task.map Option.ofNullableRecord
    let tryHeadAsync (q : IQueryable<'a>) =
        q
        |> tryHeadTask
        |> Async.AwaitTask

    let count<'a> (f : Quotations.Expr<'a -> bool>) (q : IQueryable<'a>) =
        f
        |> Lambda.ofArity1
        |> q.Count

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
