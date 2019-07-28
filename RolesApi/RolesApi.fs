module RolesApi
open Suave
open Suave.Filters
open Suave.WebPart
open Suave.Operators
open PermissionPipeLine.RolesInfrastructure
open PermissionPipeLine.Handlers
open Roles
let webPart = 
        GET >=> 
          choose[
                pathStart "/auth" 
                        >=> 
                            choose [  pathStart "/admin" >=> adminAuth
                                      pathStart "/user" >=> userAuth]

                pathStart "/admin" 
                        >=>
                             AdminPart ( chooseP [ pathS "/activeUsers" >=> getActiveUsers
                                                   pathS "/bannedUsers" >=> getBannedUser])

                pathStart "/account" 
                        >=>
                             AccountPart (chooseP [ pathS "/name" >=> getName
                                                    pathS "/surname" >=> getSurname])                                                                     
                 ]

[<EntryPoint>]
let main argv =
    startWebServer {defaultConfig with bindings  = [ HttpBinding.createSimple  HTTP  "0.0.0.0" 8080]} (webPart)
    0 // return an integer exit code


   