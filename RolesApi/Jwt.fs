[<RequireQualifiedAccessAttribute>]
module Jwt 
open JWT.Algorithms;
open JWT.Builder
open JWT
open Thoth.Json.Net
open System
open Roles


type VerifyResult = 
    | Success of ClaimDto
    | SignatureError
    | ExpiredError

[<Literal>]
let Secret ="eofjiiiiiscklxscas"; 


let verify token = 
    try 
        let json = JwtBuilder().WithAlgorithm(HMACSHA256Algorithm())
                    .MustVerifySignature()
                    .WithSecret(Secret)
                    .Decode(token) 
                    |> Decode.fromString (Decode.field "userInfo" ClaimDto.Decoder)
       
        match json with
        |Ok ok -> ok |> Success 
        
        |Error r -> failwithf "decode error %s" r
   
    with
        | :? SignatureVerificationException ->  SignatureError 
        | :? Exception  ->   SignatureError 

let generate (payload:ClaimDto) = 
    JwtBuilder()
     .WithAlgorithm(HMACSHA256Algorithm())
     .MustVerifySignature()
     .WithSecret(Secret)
     .AddClaim("userInfo",payload).Build()