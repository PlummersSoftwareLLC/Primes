# APL tradfn solution by Garklein
Thanks to [dzaima](https://github.com/dzaima) for helping optimize and for guiding me through this project (I'm pretty bad at APL).
There are 2 different ways to make functions in APL: tradfns and dfns. Tradfns, short for _Traditional Function_, was the original way to create functions. Dfns, short for _Direct Function/Dynamic Function_, were invented in 1996. Some modern APLs have dropped tradfns alltogether in favour of dfns. Wikipedia has [a comparison of tradfns vs dfns](https://en.wikipedia.org/wiki/Direct_function#Dfns_versus_tradfns), but I'll also summarise here.
- Tradfns need to be named, while dfns can be anonymous
- Tradfns allow for if statements and various types of loops, while dfns only have guards and recursion
- Dfn variables are local, tradfns variables are only local if declared local
- The arguments of a dfn are symbols (⍺ and ⍵ for right and left args), and the arguments of a tradfn are named

## Algorithm
Since tradfns have more usual control flow syntax and this solution was mostly taken from [solution\_1](../solution_1/readme.md), I'm not going to cover it in depth. Anything you don't understand will probably be covered over there. I would advise you to read it anyways since there is information such as how to work the repl that I'm not going to repeat here. There is one quirk of the tradfn solution I will cover.
### Quirk: Variables
You'll notice that the `runSieve` method in the PrimeSieve class doesn't actually run the sieve, but instead sets `bits` to the result of the `computeSieve` function. In the `runSieve` method, `bits` refers to a class field. In the `computeSieve` method, `bits` doesn't refer to the class field but instead to a local variable (it's declared as getting returned on the function declaration line). We found that, for whatever reason, if we just use the class field, it slows the program down by about 4x. Our best guess is that, when clearing bits on line 31, it can't do it in place, so it needs to duplicate the variable (and also take up more memory). We have no clue why this doesn't happen with local variables (maybe class fields have a fixed spot in memory while local variables can change memory locations?).

## Run Instructions
This only seems to work on Linux, Dyalog APL doesn't really want to run from the command line.  
See [solution\_1's readme](../solution_1/readme.md) for instructions to run in the repl.
Make sure you have [Dyalog APL](https://www.dyalog.com/download-zone.htm?p=download) installed, and added to the path.  
Run `run.sh`

## Output
`garklein-tradfn;2368;5.001;1;algorithm=base,faithful=yes,bits=1`
