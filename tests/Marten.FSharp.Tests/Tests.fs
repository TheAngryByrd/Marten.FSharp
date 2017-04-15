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
        

    let execNonQueryAsync connStr commandStr = async {

        use conn = new NpgsqlConnection(connStr)
        use cmd = new NpgsqlCommand(commandStr,conn)
        do! conn.OpenAsync() |> Async.AwaitTask
        return! cmd.ExecuteNonQueryAsync() |> Async.AwaitTask
    }

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


let superUserConnStr () = TestHelpers.createConnString "localhost" "jimmybyrd" "postgres" "postgres"

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
        open Marten.FSharp.Linq


        [<Fact>]
        let ``Query exactlyOne`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> query<Dog>
                |> exactlyOne 

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
                            |> query<Dog>
                            |> exactlyOne 
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
                |> query<Dog>
                |> filter(<@fun d -> d.Name = "Spark" @> )
                |> head 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query head`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> query<Dog>
                |> head 

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
                |> query<Dog>
                |> head 

            Assert.Equal(dog,dog2)

        [<Fact>]
        let ``Query map`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store
            use session = store.OpenSession() 
            
            let dog2Name =
                session
                |> query<Dog>
                |> map(<@fun s -> s.Name@> )
                |> head
            
            Assert.Equal(dog.Name,dog2Name)


        [<Fact>]
        let ``Query tryHead None`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
       
            use session = store.OpenSession() 
            let dog2 =
                session
                |> query<Dog>
                |> tryHead 

            Assert.Equal(None,dog2)

        [<Fact>]
        let ``Query tryHead Some`` () = 
            use database =  getNewDatabase ()
            use store = getStore database
            let dog = saveDog' store

            use session = store.OpenSession() 
            let dog2 =
                session
                |> query<Dog>
                |> tryHead 
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
                |> query<Dog>
                |> toList

            Assert.Equal(length,dogs.Count)

        [<Fact>]
        let ``Query toListAync`` () = async {
            use database =  getNewDatabase ()
            use store = getStore database
            let length = 5
            [1..length] 
            |> Seq.iter (fun _ -> saveDog' store |> ignore)
   
            use session = store.OpenSession() 
            let! dogs =
                session
                |> query<Dog>
                |> toListAsync

            Assert.Equal(length,dogs.Count)
        }