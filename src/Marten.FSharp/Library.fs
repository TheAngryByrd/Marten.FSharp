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


module Option =
    /// Creates an option from a potentially null record type
    let ofNullableRecord record =
        if (record |> box |> isNull) then None else Some record


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
            let le = mc.Arguments.[0] :?> LambdaExpression
            let args, body = translateExpr le.Body
            le.Parameters.[0] :: args, body
        | _ -> [], linq

    let inline toLinq<'a> expr =
        let args, body = expr |> QuotationToExpression  |> translateExpr
        Expression.Lambda<'a>(body, args |> Array.ofList) 
    let inline ofArity1 (expr : Expr<'a -> 'b>) =
        toLinq<Func<'a,'b>> expr
    let inline ofArity2 (expr : Expr<'a -> 'b -> 'c>) =
        toLinq<Func<'a,'b,'c>> expr

    



module Doc = 
    open Marten
    open System.Linq
    open Marten.Linq


    let deleteEntity (entity : 'a) (session : IDocumentSession) =
        session.Delete<'a>(entity)
    let deleteByGuid<'a> (id : Guid) (session : IDocumentSession) =
        session.Delete<'a>(id)
    let deleteByString<'a> (id : string) (session : IDocumentSession) =
        session.Delete<'a>(id)
    let deleteByInt<'a> (id : int) (session : IDocumentSession) =
        session.Delete<'a>(id)

    
    let deleteByInt64<'a> (id : int64) (session : IDocumentSession) =
        session.Delete<'a>(id)

        
    let loadByGuid<'a> (id : Guid) (session : IDocumentSession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByInt<'a> (id : int) (session : IDocumentSession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByInt64<'a> (id : int64) (session : IDocumentSession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord
    let loadByString<'a> (id : string) (session : IDocumentSession) =
        session.Load<'a>(id)
        |> Option.ofNullableRecord

    let loadByGuidAsync<'a> (id : Guid) (session : IDocumentSession) = async {
        
        let! result =  session.LoadAsync<'a>(id) |> Async.AwaitTask
        return result |> Option.ofNullableRecord
    }
        
    let loadByInt64Async<'a> (id : int64) (session : IDocumentSession) = async {
        let! result =  session.LoadAsync<'a>(id) |> Async.AwaitTask
        return result |> Option.ofNullableRecord
    }
    let loadByStringAsync<'a> (id : string ) (session : IDocumentSession) = async {
        let! result =  session.LoadAsync<'a>(id) |> Async.AwaitTask
        return result |> Option.ofNullableRecord
    }


    let saveChanges (session : IDocumentSession) =
        session.SaveChanges()

    let saveChangesAsync (session : IDocumentSession) =
        session.SaveChangesAsync() |> Async.AwaitTask


    // not supported
    // let aggregate (f : Quotations.Expr<'a -> 'a -> 'a>) (q : IQueryable<'a>) =
    //     f
    //     |> Lambda.ofArity2
    //     |> q.Aggregate

    // let fold (seed :'a) (f : Quotations.Expr<'a -> 'b -> 'a>) (q : IQueryable<'b>) =
    //     q.Aggregate(
    //         seed, 
    //         f|> Lambda.ofArity2)
    let query<'a> (session : IDocumentSession) =
        session.Query<'a>()

    let exactlyOne (q : IQueryable<'a>) = q.Single()
    let exactlyOneAsync (q : IQueryable<'a>) = 
        q.SingleAsync()
        |> Async.AwaitTask

    let filter (f : Quotations.Expr<'a -> bool>) (q : IQueryable<'a>) =
        f  
        |> Lambda.ofArity1
        |> q.Where

    let head (q : IQueryable<'a>) = q.First()

    let headAsync (q : IQueryable<'a>) = 
        q.FirstAsync() 
        |> Async.AwaitTask

    let map (f : Quotations.Expr<'a -> 'b>) (q : IQueryable<'a>) =
        f  
        |> Lambda.ofArity1
        |> q.Select

    let storeSingle (session : IDocumentSession) entity  =
        session.Store([|entity|])
    let storeMany (session : IDocumentSession) (entities : #seq<_>)  =
        entities |> Seq.toArray |> session.Store
    let toList (q : IQueryable<'a>) =
        q.ToList()

    let toListAsync (q : IQueryable<'a>) =
        q.ToListAsync()
        |> Async.AwaitTask

    let tryExactlyOne (q : IQueryable<'a>) = 
        q.SingleOrDefault()
        |> Option.ofNullableRecord
    let tryExactlyOneAsync (q : IQueryable<'a>) = 
        q.SingleOrDefaultAsync()
        |> Async.AwaitTask
        |> Async.map Option.ofNullableRecord

    let tryHead (q : IQueryable<'a>) = 
        q.FirstOrDefault() 
        |> Option.ofNullableRecord
    let tryHeadAsync (q : IQueryable<'a>) = 
        q.FirstOrDefaultAsync() 
        |> Async.AwaitTask
        |> Async.map Option.ofNullableRecord

        