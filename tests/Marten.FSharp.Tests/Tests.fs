module Tests

open System
open Npgsql
open Marten
open Expecto

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

[<Tests>]
let tests =
  testList "litmus tests]" [
    testCase "Can create/destroy database`" <| fun _ ->
      use database =  getNewDatabase ()
      ()
    testCase "Can Save/Load Entity" <| fun _ ->
        use database =  getNewDatabase ()
        use store = getStore database

        let dog = saveDog' store

        use session2 = store.OpenSession()  
        let dog2 = session2.Load<Dog>(dog.Id)
        Expect.equal dog dog2 "Same dog!"
  ]