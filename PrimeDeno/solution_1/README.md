# Deno solution by GerbenRampaart

## Run instructions

## Deno
With Deno (https://deno.land/) installed do 'deno run PrimeDeno.ts'

## Docker
docker build . -t primedeno
docker run primedeno:latest

### Note 1
This is almost a one to one copy of the TypeScript solution of Primes.
Basically only the Buffer is an Uint8Array in Deno and writing to stdout is a promise.

### Note 2
Since Deno uses TypeScript you need to tell vscode that the Deno language service needs to 
parse the .ts files instead of the tsc if you have that installed. That's why the .vscode is
included because that tells the Deno vscode plugin that the current project is a Deno project.

## Output
Developed using Deno 1.11.5 as of 2021-7-8

My output on a mac mini 2018 i5:
Passes: 4561, Time: 5, Avg: 0.0010962508221881166, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
marghidanu;4561;5;1;algorithm=base,faithful=yes

In the container:
Passes: 4157, Time: 5, Avg: 0.0012027904738994466, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: true
marghidanu;4157;5;1;algorithm=base,faithful=yes
