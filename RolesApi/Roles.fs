module Roles 
open Thoth.Json.Net

    type AdminInfo = {
            ActiveUsersEmails: string list
            BanUsersEmails : string list                  
        }

    type UserInfo = {
            Name:string
            Surname:string
        }

    type Role =
        |Admin
        |User

    type ClaimDto = {
        Email:string;
        Role:Role
    }
    with static member Decoder =
            Decode.object(fun get -> {Email = get.Required.Field "Email" Decode.string;
                                Role = get.Required.Field "Role" (Decode.field "Case" Decode.string|> 
                                                                                        Decode.andThen (function
                                                                                                            |"Admin" -> Decode.succeed Admin
                                                                                                            | "User" -> Decode.succeed User
                                                                                                            | _ -> failwith ""))})
                                                                                                            
                                                                                                            
        
         static member Encoder(dto:ClaimDto) = 
            Encode.object["email",Encode.string dto.Email;"role",(Encode.string (match dto.Role with |Admin -> "Admin" |User -> "User" ))]
