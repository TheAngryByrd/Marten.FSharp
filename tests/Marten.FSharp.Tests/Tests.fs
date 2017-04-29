module Tests


open Expecto
module Expecto =
    module Expect =
        open Expecto.Logging
        open Expecto.Logging.Message
        let throwsTAsync<'texn> f message = async {
            try
                do! f 
                Tests.failtestf "%s. Expected f to throw." message
            with
            | e when e.GetType() <> typeof<'texn> ->
                Tests.failtestf "%s. Expected f to throw an exn of type %s, but one of type %s was thrown."
                                message
                                (typeof<'texn>.FullName)
                                (e.GetType().FullName)
            | e ->
                ()
        }

open System
open System.Linq

open Expecto
open Marten
open Marten.Linq
open Npgsql

module TestHelpers =
    let execNonQuery connStr commandStr =
        use conn = new NpgsqlConnection(connStr)
        use cmd = new NpgsqlCommand(commandStr,conn)
        conn.Open()
        cmd.ExecuteNonQuery()
        
    let createDatabase connStr databaseName =
        databaseName
        |> sprintf "CREATE database \"%s\" ENCODING = 'UTF8'"
        |> execNonQuery connStr
        |> ignore
    let dropDatabase connStr databaseName =
        //kill out all connections
        databaseName
        |> sprintf "select pg_terminate_backend(pid) from pg_stat_activity where datname='%s';"
        |> execNonQuery connStr
        |> ignore

        databaseName
        |> sprintf "DROP database \"%s\""
        |> execNonQuery connStr
        |> ignore
        
    let createConnString host user pass database =
        sprintf "Host=%s;Username=%s;Password=%s;Database=%s" host user pass database
        |> NpgsqlConnectionStringBuilder


    type DisposableDatabase (superConn : NpgsqlConnectionStringBuilder, databaseName : string) =
        static member Create(connStr) =   
            let databaseName = System.Guid.NewGuid().ToString("n")
            createDatabase (connStr |> string) databaseName     
            
            new DisposableDatabase(connStr,databaseName)
        member x.SuperConn = superConn
        member x.Conn = 
            let builder = x.SuperConn |> string |> NpgsqlConnectionStringBuilder
            builder.Database <- x.DatabaseName
            builder
        member x.DatabaseName = databaseName    
        interface IDisposable with
            member x.Dispose() = 
                dropDatabase (superConn |> string) databaseName

let getEnv str =
    System.Environment.GetEnvironmentVariable str

let host () = getEnv "POSTGRES_HOST"
let user () = getEnv "POSTGRES_USER"
let pass () = getEnv "POSTGRES_PASS"
let db () = getEnv "POSTGRES_DB"
let superUserConnStr () = TestHelpers.createConnString (host ()) (user ()) (pass()) (db())

let getNewDatabase () = superUserConnStr () |>  TestHelpers.DisposableDatabase.Create

let getStore (database : TestHelpers.DisposableDatabase) = database.Conn |> string |> DocumentStore.For


[<CLIMutable>]
type Dog = {
    Id : Guid
    Name : string
    FavoriteChewToy : string
}

let newDog name favoriteChewToy =
     { Id = Guid.NewGuid(); Name = name; FavoriteChewToy = favoriteChewToy}

let saveDog  (store : IDocumentStore) (dog : Dog) =
    use session = store.OpenSession()
    session.Store(dog)
    session.SaveChanges()
    dog
let saveDog' (store : IDocumentStore)  =
    let dog = newDog "Spark" "Macbook"
    saveDog store dog

open TestHelpers

let withDatabase f () =
    use database =  getNewDatabase ()
    f database

open System.Reflection
open Microsoft.FSharp.Reflection
let withDatabaseAndStore (f : _ -> _ -> _) () = async {
    use database =  getNewDatabase ()
    use store = getStore database
    let retval = f database store 
    return! (retval |> box :?> Async<unit>)
}

[<Tests>]
let ``Litmus Tests`` =
  testList "litmus Tests" [

    yield! testFixture withDatabase [
        "Can create/destroy database", ignore
        "Can Save/Load Record", 
            fun db ->
                use store = getStore db
                let expectedDog = saveDog' store

                use session2 = store.OpenSession()  
                let actualDog = session2.Load<Dog>(expectedDog.Id)
                Expect.equal expectedDog actualDog "Same dog!"

    ]
  ]



let inline testFixture' setup =
    Seq.map (fun (testType, name, partialTest) ->
        partialTest
        |> setup
        |> testType name 
    )

let inline testCase' name (test )= 
    let test = test >> Async.RunSynchronously
    TestLabel(name, TestCase (Sync test,Normal), Normal)
let inline testCaseAsync' name (test : unit -> Async<unit>) = 
    let asyncWrapper = async.Delay test
    TestLabel(name, TestCase (Async asyncWrapper,Normal), Normal)



let loadByGuidTests =   [
    testCase', "loadByGuid, Get Some Record back" ,
                fun db store -> async {
                    let expectedDog = saveDog' store
                    use session = store.OpenSession() 
                    let actualDog =
                        session
                        |> Doc.loadByGuid<Dog> expectedDog.Id
                    Expect.equal (Some expectedDog) actualDog  "Same dog!"
                }
    testCase', "loadByGuid, Get None Record back" ,
        fun db store -> async {
            let _ = saveDog' store
            use session = store.OpenSession() 
            let actualDog =
                session
                |> Doc.loadByGuid<Dog> (Guid.NewGuid())
            Expect.equal None actualDog  "Should be no dog!"
        }
    testCaseAsync', "loadByGuidAsync, get Some record back",
        fun db store -> async {
            let expectedDog = saveDog' store
            use session = store.OpenSession() 
            let! actualDog =
                session
                |> Doc.loadByGuidAsync<Dog> expectedDog.Id

            Expect.equal (Some expectedDog) actualDog  "Same dog!"
        }
    testCaseAsync', "loadByGuidAsync, get None record back",
        fun db store -> async {
            let expectedDog = saveDog' store
            use session = store.OpenSession() 
            let! actualDog =
                session
                |> Doc.loadByGuidAsync<Dog> (Guid.NewGuid())

            Expect.equal None actualDog  "Should be no dog!"
        }
    ]

let exactlyOnceTests = [
    testCase', "exactlyOne, Get Record Back" ,
                fun db store -> async {
                    let expectedDog = saveDog' store
                    use session = store.OpenSession() 
                    let actualDog =
                          session
                            |> Doc.query<Dog>
                            |> Doc.exactlyOne
                    Expect.equal expectedDog actualDog  "Should be one dog!"
                }

    testCase', "exactlyOne, Throws if more than one" ,
        fun db store -> async {
            Expect.throwsT<InvalidOperationException>(fun _ ->
                let _ = saveDog' store
                let _ = saveDog' store
                use session = store.OpenSession() 
                let actualDog =
                    session
                        |> Doc.query<Dog>
                        |> Doc.exactlyOne
                ()
            ) "Should be too many dogs!"
    
        }
    testCase', "exactlyOne, Throws if none" ,
        fun db store -> async {
            Expect.throwsT<InvalidOperationException>(fun _ ->
                use session = store.OpenSession() 
                let actualDog =
                    session
                        |> Doc.query<Dog>
                        |> Doc.exactlyOne
                ()
            ) "Should be no dog!"
    
        }
    testCaseAsync', "exactlyOneAsync, Get Record Back" ,
        fun db store -> async {
            let expectedDog = saveDog' store
            use session = store.OpenSession() 
            let! actualDog =
                    session
                    |> Doc.query<Dog>
                    |> Doc.exactlyOneAsync
            Expect.equal expectedDog actualDog  "Should be one dog!"
        }
    testCaseAsync', "exactlyOneAsync, Throws if more than one" ,
        fun db store -> async {
            do!
                Expect.throwsTAsync<AggregateException>(async {
                    let _ = saveDog' store
                    let _ = saveDog' store
                    use session = store.OpenSession() 
                    let! actualDog =
                        session
                            |> Doc.query<Dog>
                            |> Doc.exactlyOneAsync
                    ()
                }) "Should be too many dogs!"
    
    }
    testCaseAsync', "exactlyOneAsync, Throws if none" ,
        fun db store -> async {
            do!
                Expect.throwsTAsync<AggregateException>(async {
                    use session = store.OpenSession() 
                    let! actualDog =
                        session
                            |> Doc.query<Dog>
                            |> Doc.exactlyOneAsync
                    ()
                }) "Should be no dogs!"
    
        }
]

[<Tests>]
let ``API Tests`` =
    testList "API Tests" [
        yield! testFixture' withDatabaseAndStore [
            yield! loadByGuidTests
            yield! exactlyOnceTests
            
 
        ]
    ]