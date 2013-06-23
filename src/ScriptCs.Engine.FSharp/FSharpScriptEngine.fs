namespace ScriptCs.Engine.FSharp
open ScriptCs
open Common.Logging
open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Interactive.Shell
open ExtCore
open System.Linq

type Result = Success of String | Error of string | Incomplete

type FSharpEngine(host:ScriptHost) = 
    let oin = Console.OpenStandardInput()
    let oout = Console.OpenStandardOutput()
    let oerr = Console.OpenStandardError()
     
    let stdinStream = new CompilerInputStream()
    let stdin = new StreamReader(stdinStream)
 
    let stdoutStream = new CompilerOutputStream()
    let stdout = StreamWriter.Synchronized(new StreamWriter(stdoutStream, AutoFlush=true))
 
    let stderrStream = new CompilerOutputStream()
    let stderr = StreamWriter.Synchronized(new StreamWriter(stderrStream, AutoFlush=true))
     
    let getOutput (stream:CompilerInputStream) code = 

        let rec tryget() = 
            async{ do! Async.Sleep(100)

                   let error = stderrStream.Read()
                   if error.Length > 0 then return Error(error) else

                   let result = stdoutStream.Read()
                   
                   if result |> String.isNullOrEmpty then return! tryget()
                   else return Success result }

        stream.Add(code + "\n")
        if code.EndsWith ";;" then tryget() 
        else async.Return Incomplete
        
    let commonOptions = [| "fsi.exe"; "--nologo"; "--readline-";|]
    let session = FsiEvaluationSession(commonOptions, stdin, stdout, stderr)

    // Start the session in the background
    do Async.Start <| async {session.Run()}
    do session.Interrupt()
    do stdoutStream.Read() |> ignore

    let (>>=) (d1:#IDisposable) (d2:#IDisposable) = 
        {new IDisposable with member x.Dispose() = d1.Dispose(); d2.Dispose()}

    member x.Require<'a>() = host.Require()

    member x.Execute(code, ?timeout) = 
       let timeout = defaultArg timeout 10000
       Async.RunSynchronously((getOutput stdinStream code), timeout)

    member x.AddReference(ref) =
        stdinStream.Add( sprintf "#r @\"%s\";;\n" ref)

    member x.SilentAddReference(ref) = 
        let rec wait() = 
           let res = stdoutStream.Read()
           if res = String.empty || res = "\r\n" || res = "> " then
              Async.Sleep(25) |> Async.RunSynchronously
              wait()
        x.AddReference(ref)
        wait()

    member x.ImportNamespace(namespace') =
        stdinStream.Add(sprintf "open %s;;\n" namespace')

    member x.SilentImportNamespace(namespace') =
       let rec wait() = 
           let res = stdoutStream.Read()
           if res = String.empty then
              Async.Sleep(25) |> Async.RunSynchronously
              wait()
       x.ImportNamespace(namespace')
       wait()

    interface IDisposable with
       member x.Dispose() =
          (stdinStream >>= stdin >>= stdoutStream >>= stdout >>= stderrStream >>= stderr).Dispose()          
                      
type  FSharpScriptEngine( scriptHostFactory:IScriptHostFactory, logger: ILog) =
    let mutable baseDir = String.empty
    let [<Literal>]sessionKey = "F# Session"
   
    interface IScriptEngine with
        member x.BaseDirectory with get() = baseDir and  set value = baseDir <- value
        member x.Execute(code: String, scriptArgs, references, namespaces, scriptPackSession) =
            let distinctReferences = references.Union(scriptPackSession.References).Distinct()
            let sessionState = 
                match scriptPackSession.State.TryGetValue sessionKey with
                | false, _ -> let host = scriptHostFactory.CreateScriptHost(ScriptPackManager(scriptPackSession.Contexts), scriptArgs)
                              logger.Debug("Creating session")
                              let session = new FSharpEngine(host)
                     
                              distinctReferences |> Seq.iter (fun r -> logger.DebugFormat("Adding reference to {0}", r)
                                                                       session.SilentAddReference r )
                     
                              namespaces.Union(scriptPackSession.Namespaces).Distinct() 
                              |> Seq.iter (fun ns -> logger.DebugFormat("Importing namespace {0}", ns)
                                                     session.SilentImportNamespace ns)
                     
                              let sessionState = SessionState<_>(References = distinctReferences, Session = session)
                              scriptPackSession.State.Add(sessionKey, sessionState)
                              sessionState 
                | true, res -> logger.Debug("Reusing existing session") 
                               let sessionState = res :?> SessionState<FSharpEngine>
                               
                               let newReferences = match sessionState.References with
                                                   | null -> distinctReferences
                                                   | s when Seq.isEmpty s -> distinctReferences
                                                   | s ->  distinctReferences.Except s
                               newReferences |> Seq.iter (fun r -> logger.DebugFormat("Adding reference to {0}", r)
                                                                   sessionState.Session.AddReference r ) 
                               sessionState      

            match sessionState.Session.Execute(code) with
            | Success result -> let cleaned = 
                                   result.Split([|"\r"; "\n";|], StringSplitOptions.RemoveEmptyEntries)
                                   |> Array.filter (fun str -> not(str = "> "))
                                   |> String.concat "\r\n"
                                ScriptResult(ReturnValue = cleaned)
            | Error e -> ScriptResult(CompileException = exn e )
            | Incomplete -> ScriptResult()