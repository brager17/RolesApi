module PermissionPipeLine
open Suave.Successful
open Suave
open Suave.Filters
open Suave.Operators
open Thoth.Json.Net
open Roles

    module RolesInfrastructure = 
        let permissionHandler roles getPermissionInfo handler ctx= 
            let authorizationClaim = 
                ctx.request.headers 
                |> List.tryFind(fun (key,_) -> key = "authorization")
                |> Option.bind(fun (_,value) -> Some (Jwt.verify value ))
            
            let claimResult = match authorizationClaim with
                                | (Some (Jwt.VerifyResult.Success claims)) -> Ok claims
                                |_ ->  Error "Authorize 401"
            
            match claimResult with
                |Ok ok when (roles |> List.contains(ok.Role)) -> 
                        async{
                          let! permissionInfo = getPermissionInfo ctx
                          let! r =  handler (permissionInfo,ctx)
                          match r with
                          | Some (_,ctx) ->  return Some ctx
                          | None -> return! RequestErrors.UNAUTHORIZED "" ctx
                        }
                | _ -> RequestErrors.UNAUTHORIZED "" ctx

        let adapter originalfunc modifiedContext=
            let (data,ctx) = modifiedContext
            async{
                let! originalFuncResult = originalfunc ctx
                return (originalFuncResult |> Option.map (fun x-> (data,x)))
            }

        let pathStart (str:string) ctx =
            let path = ctx.request.rawPath
            if path.StartsWith str 
            then 
                let path' =  path 
                                |> Seq.skip str.Length 
                                |> System.String.Concat
                {ctx with request = {ctx.request with rawPath = path' }}
                |> Some |> async.Return
            else async.Return None 

        let get<'a>  = adapter GET 
        
        let ok str =  str |> OK |> adapter 

        let pathS str = str |> pathStart |> adapter 

        let rec chooseP (webparts:(('a*HttpContext) -> Async<('a*HttpContext) option>) list)  context= 
             async{
             match webparts with
                        | [head] -> return! head context
                        | head::tail  -> 
                            let! result = head context
                            match result with
                            | Some _-> return result
                            | None -> return! choose tail context
                        | [] -> return failwith "empty"
             }
      

    open RolesInfrastructure
    type Jwt = {Jwt:string}
    module Handlers=

        let AdminPart h=
            let getAdminInfo ctx = 
                async.Return {ActiveUsersEmails=["user1";"user2"];BanUsersEmails=["user3";"user4"]}
            permissionHandler  [Admin] getAdminInfo h

        let AccountPart h= 
            let getUserInfo ctx = 
                async.Return {Name="name";Surname="surname"}
            permissionHandler [User;Admin] getUserInfo  h
                    
        let adminAuth = OK (Encode.Auto.toString<Jwt> (4,{Jwt = Jwt.generate {Email="admin@example.com";Role = Admin}}))
        let userAuth  = OK  (Encode.Auto.toString<Jwt> (4,{Jwt = Jwt.generate {Email="user@example.com";Role = User}}))
        
        let  getActiveUsers (ctx:(AdminInfo*HttpContext)) =
            let (adminInfo,_) = ctx
            ok (Encode.Auto.toString<string list> (4,adminInfo.ActiveUsersEmails)) ctx

        let getBannedUser (ctx:(AdminInfo*HttpContext)) = 
             let (adminInfo,_) = ctx
             ok (Encode.Auto.toString<string list> (4,adminInfo.BanUsersEmails)) ctx

        let getName (ctx:UserInfo*HttpContext) = 
            let (userInfo,_) = ctx
            ok(Encode.toString 4 (Encode.object["name", Encode.string userInfo.Name])) ctx

        let getSurname (ctx:UserInfo*HttpContext) = 
            let (userInfo,_) = ctx
            ok(Encode.toString 4 (Encode.object["surname", Encode.string userInfo.Surname])) ctx
   




