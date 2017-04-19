module Tests

open System
open Npgsql
open Marten
open Xunit


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
     { Id = Guid.NewGuid(); Name = "Spark"; FavoriteChewToy = "Macbook"}


let saveDog  (store : IDocumentStore) (dog : Dog) =
    use session = store.OpenSession()
    session.Store(dog)
    session.SaveChanges()
    dog
let saveDog' (store : IDocumentStore)  =
    let dog = newDog "Spark" "Macbook"
    saveDog store dog

open TestHelpers
module Litmus =
    //litmus tests
    [<Fact>]
    let ``Can create/destroy database`` () =   
        use database =  getNewDatabase ()
        ()

    [<Fact>]
    let ``Can Save and Get Record`` () =
        use database =  getNewDatabase ()
        use store = getStore database

        let dog = saveDog' store

        use session2 = store.OpenSession()  
        let dog2 = session2.Load<Dog>(dog.Id)
        Assert.Equal(dog,dog2)



module FSharp_Tests =

    open Marten.FSharp
    [<Fact>]
    let ``Load by Guid Some`` () = 
        use database =  getNewDatabase ()
        use store = getStore database
        let dog = saveDog' store

        use session = store.OpenSession() 
        let dog2 =
            session
            |> Marten.FSharp.loadByGuid<Dog> dog.Id
        
        Assert.Equal(Some dog,dog2)

    [<Fact>]
    let ``Load by Guid None`` () = 
        use database =  getNewDatabase ()
        use store = getStore database

        use session = store.OpenSession() 
        let dog2 =
            session
            |> Marten.FSharp.loadByGuid<Dog> (Guid.NewGuid())
            
        
        Assert.Equal(None,dog2)

    module Linq_Tests =
        open System.Linq
        open Marten.Linq


        [<Fact>]
        let ``Query exactlyOne`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.exactlyOne 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query exactlyOne fails`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store
            let dog = saveDog' store
            use session = store.OpenSession() 
            Assert.Throws<System.InvalidOperationException>(
                fun () -> session
                            |> Doc.query<Dog>
                            |> Doc.exactlyOne 
                            |> ignore
                )



        [<Fact>]
        let ``Query filter`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 

            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.filter(<@fun d -> d.Name = "Spark" @> )
                |> Doc.head 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query head`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.head 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query head multiple`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            let dog' = newDog "Dot" "Shoes" |> saveDog store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.head 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query map`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store
            use session = store.OpenSession() 
            
            let dog2Name =
                session
                |> Doc.query<Dog>
                |> Doc.map(<@fun s -> s.Name@> )
                |> Doc.head
            
            Assert.Equal(dog.Name,dog2Name)


        [<Fact>]
        let ``Query tryHead None`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
       
            use session = store.OpenSession() 
            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.tryHead 

            Assert.Equal(None,dog2)

        [<Fact>]
        let ``Query tryHead Some`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> Doc.query<Dog>
                |> Doc.tryHead 
            Assert.Equal(Some dog,dog2)

        [<Fact>]
        let ``Query toList`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let length = 5
            [1..length] 
            |> Seq.iter (fun _ -> saveDog' store |> ignore)
   
            use session = store.OpenSession() 
            let dogs =
                session
                |> Doc.query<Dog>
                |> Doc.toList

            Assert.Equal(length,dogs.Count)

        [<Fact>]
        let ``Query toListAsync`` () = async {
            use database =  getNewDatabase ()
            use store = getStore database
            let length = 5
            [1..length] 
            |> Seq.iter (fun _ -> saveDog' store |> ignore)
   
            use session = store.OpenSession() 
            let! dogs =
                session
                |> Doc.query<Dog>
                |> Doc.toListAsync

            Assert.Equal(length,dogs.Count)
        }