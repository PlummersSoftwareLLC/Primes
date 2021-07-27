# APL dfn solution by Garklein
This is an APL implementation of the prime sieve.  
Thanks to [dzaima](https://github.com/dzaima) for helping optimize and for guiding me through this project (I'm pretty bad at APL).

## Using the repl to run the sieve/seeing full output (with validity)
If you are on Windows, like me, or are too lazy to install Docker and whatnot, here are instructions for running the sieve in the Dyalog APL repl.
1. Install [Dyalog APL](https://www.dyalog.com/download-zone.htm?p=download) 
2. Open the application
3. use the `]CD` command to change directories to wherever this repo is on your computer (.....Github/Primes/PrimeAPL/solution\_1). Every time you call it (and if you call it with no argument), it also outputs the current path.
4. Type `]LOAD ./PrimeSieve.aplc` to load the `PrimeSieve` class
5. Open ./main.apl in Notepad or a similar application
6. Copy lines 2-13 (the whole main function) into the repl, then press enter
7. Type `main` into the repl and press enter to run the test
8. You can edit the `main` function or the `PrimeSieve` class by typing in the repl `)ed main|PrimeSieve`

To see the full output, change the `0` on line 10 (right after where it says `sieve.printResults`) to a `1`.

## Algorithm
First, here are some basics about APL. I will provide some examples in JavaScript as well.

### Basics
- Booleans are represented by 0 and 1
- Variables don't need to be declared, and the assignment operator is `←` (as opposed to `=` or `:=` in most languages). `a←5` sets `a` to 5. Since all the intrinsic functions are symbols, there are no reserved keywords.
- There are 2 ways to call dfns and intrinsic glyphs: monadically (with 1 argument) and dyadically (with 2 arguments). To use an example from math (the `-` sign), writing the expression `5 - 2` is using it diadially. In this case, it means "return the difference between the left argument and the right argument". The expression `-7` uses the `-` sign monadically. In this case, it means "return the difference between 0 and the right argument (or return the right argument negated)".
- There is no order of operations in APL. When functions are called, the left argument (if present) is the number (or parenthesized expression) directly to the left, and the right argument is everything to the right of the function. For example, `3 × 7 - 5` will be parsed as `3 × (7 - 5)` (3 times everything to the right).
- The special character `¯`, called a high minus, is used to represent negative numbers (`¯3` in APL is `-3` in most programming languages)
- Strings are arrays of characters (meaning that you can use any function that you would normally use on an array with them). They __must__ be surrounded by single quotes.
- Arrays are represented by numbers with spaces between them (`1 2 3` in APL is the equivalent of `[1, 2, 3]` in many programming languages).

More basics and concepts will be introduced as they appear in the code.

### [main.apl](./main.apl)
Line 1 gets the `PrimeSieve` class and fixes it (adds it to the repl space). This is not a very good way to do it, but idk any better ways.  
Line 2 starts a tradfn.  
Line 3 sets a `passes` variable to 0.  
Line 4 sets the `tEnd` variable to the time (in ms since 1970) that is 5 seconds from when the variable is assigned. Here's how it works:
- `⎕TS` gets the timestamp as an array (in Y/M/D/H/S/MS format). All functions that start with `⎕` (quad) are system functions, and usually vary across different APLs.
- `⊂`, when using monadically, is called enclose and encloses an array into a scalar. For example, `⊂1 2` will make a scalar (or rank 0 array) where the one value is a nested array with 2 elements.
- `⎕DT` is a function that converts between time formats. On the right it takes an enclosed date, and on the left it takes a format id. 12 is the format id for JavaScript time (aka the number of ms since January 1st, 1970).
- 5000 is added to get the timestamp in 5 seconds (which is when the loop will finish).

There are 2 types of functions, tradfns and dfns. There is an overview in the [readme for solution 2](../solution_2/readme.md). This solution uses dfns, so I need to cover some basic dfn syntax.

#### Dfn syntax
- Dfns are enclosed between curly brackets (`{}`).
- Dfns can be assigned to variables (although after the first time a variable is assigned, if it's a dfn it can't be assigned to a non-dfn, and vice versa).
- The left argument of a dfn is represented in the dfn by ⍵ (omega), and the right argument (which may not always be present) is represented by ⍺ (alpha).
- Dfns return on the first non-assignment statement, and they return that value.
- Different statements can either be separated by newlines, or the ⋄ (statement separator) symbol.
- Control flow can only be achieved by guards (`:`), error guards (`::`, basically a try/catch) and recursion (`∇`).
- Guards are basically a one line if statement. They go in this form: `condition: result`, where everything after the colon until the next statement gets evaluated and returned from the dfn if the condition is true. You can read about error guards [here](https://aplwiki.com/wiki/Dfn#Error-guards) (I won't be covering them since they aren't used in this program).
- Del (`∇`) is used for recursion. Writing `∇` with a right argument (and a left argument if the dfn takes it) will recurse.

With this in mind we can build a simple recursive factorial dfn: `{⍵≤1:⍵⋄⍵×∇⍵-1}`.  
In JavaScript, this would look like
```JavaScript
(function (n) {                       // {
    if (n <= 1) return n;             // ⍵≤1:⍵
    return n * arguments.callee(n-1); // ⍵×∇⍵-1
})                                    // }
```
If `⍵` (the right argument) is less than or equal to 1, return it. Otherwise, return `⍵` × the result of the function called with `⍵-1`. It can be called by either typing that out and putting the argument after the closing curly brace, or assigning that to a variable and then writing `varName n`, where varName is the function variable and n is the argument.  

Let's go back to main.apl.  

On line 6, it makes a new dfn that gets called immediately (by the `0` on line 12).  
On line 7, it sets `sieve` to a new `PrimeSieve` with the limit of 1,000,000.  
On line 8, it assigns `_` to the result of running the sieve. We need to assign a variable to the result because dfns return at the first non-assignment statement.  
On line 9, `passes` gets incremented by 1. Like how you can do `+=` or `*=` in most languages, in Dyalog APL you can do `+←` or `×←`, although in Dyalog APL you can also use any intrinsic glyph, tradfn or dfn (provided it takes 2 aruguments).  
Line 10 is a guard. If the current time (converted to JS representation, see above) is greater than the ending time we calculated (the variable `tEnd`), call the `printResults` method on the current `PrimeSieve` instance. The `printResults` method takes 3 arguments, in the form of a list. The first is whether to print the whole output, the second is the total time, and the third is the # of passes. The 1st and 3rd arguments are straightforward, so we'll cover the 2nd argument.  
To find the total time elapsed, we take the current time, subtract `tEnd`, and add 5000 milliseconds. We then need to divide by 1000 to convert to seconds (instead of milliseconds). `5000+(12⎕DT⊂⎕TS)-tEnd` does everything described above except dividing by 1000. To do that, we prefix that expression with `1000÷⍨`. `⍨` is called switch and will swap the arguments of a function. `(15+3)÷2` and `2÷⍨15+3` are equivalent, except that the second is one character shorter and looks nicer imo (if you completely removed the brackets from the first version, it would be interpreted as `15+(3÷2)`.  
Line 11 just recurses (with the argument 0, since this function doesn't need arguments) if the guard wasn't triggered.  
Line 12 ends and calls the dfn, line 13 ends the `main` tradfn, and line 14 calles the `main` tradfn.  
Then, there are some blank lines because Dyalog APL won't run from the command line without them (who knows).

### [PrimeSieve.aplc](./PrimeSieve.aplc)
Got all that above? Good.  
Since this file only contains the `PrimeSieve` class, it gets the file extension .aplc (for APL classes).  
On lines 2 and 3 the `n` (sieve size) and `bits` fields are declared.  
On line 5, `⎕IO` is set to 0. This makes the index origin start from 0 (by default it starts at 1).  
On lines 6 and 7, 2 shared fields are declared (static properties in other languages). These are the historical validation data.  
`sieveResults` is pretty straightforwards.  
Since `sieveSizes` is just 10^1, 10^2 ... 10^10, it can be made more compact with APL. `⍳`, when used monadically, is called the index generator and will generate an array of numbers from 0..n-1 or 1..n (depending on index origin). In this case, it generates 0..9. We need to make this 1..10, so we add 1 to the array. In APL, operations with a scalar and an array will apply the operation to each member of the array using the scalar as the other operand. For example, `1+1 2 3 4` will compute to `2 3 4 5`. `*` is the APL sign for exponentiation, so we raise 10 to the power of the array 1..10 (using scalar/array operations like above).  
On line 9 is the first class method. Notice that class methods must be tradfns and cannot be dfns. It's pretty simple. It just says it's the constructor, says it's public, and sets `n` (the class field) to `limit` (the constructor parameter).  
On line 15 is the `runSieve` method, which just calls the `runSieveDfn` dfn with the sieve limit as its parameter, and sets `bits` (the class field) to the result. It also returns 1, because line 8 of main.apl will complain if we don't return anything.

#### runSieveDfn
Firstly, it sets the index origin to 0, `n` to the argument (which is the sieve limit), and `factor` to 3.  
`bits` will be an array of numbers from 0 (the index origin) to n-1, where all primes will be 1s and all non-primes will be 0. It actually marks 1 as prime and 2 as non primes, but that still returns the right number of primes and we just need to account for it when printing primes later.  
We `bits` to an alternating pattern of 0s and 1s, because we can immediately knock off all even numbers. `⍴` is called shape when used monadically. `2 3⍴1 2 3 8 1 7` shapes the second array into a 2 by 3 matrix (`[[1, 2, 3], [8, 1, 7]]` in some programming languages). If the left side is bigger than the right side, it will repeat the right side until it reaches the size needed, and that is what's being used here. `n⍴0 1` will create an array of size n with alternating 0s and 1s. Since index origin is 0, this means that all even numbers are represented by 0 and are "crossed off".  
`q`, the loop limit, is then set to `n*0.5` (remember than `*` is used for exponentiation, so this means the sqrt of n), rounded up (`⌈`).  
Then, we start the real loop. Like before, this is an "anonymous" dfn that is called instantly. The loop first clears multiples, then finds the next factor. If the factor is less than or equal to `q`, it recurses (and repeats the loop). Otherwise, it finishes the loop (and returns 1). Then, to finish `runSieveDfn`, `bits` is returned.

##### Clearing Bits
Where bits are cleared is also another "anonymous", immediately called dfn. We found that by using a combination of different bit-clearing methods, we can get more speed. If `factor` is greater than 50, we index `bits` by all multiples of the factor, and set all those elements to 0. If `factor` is less than or equal to 50, we use bit magic.

###### `factor`>50
`⌈n÷factor` is self explanatory (remember than `⌈`, when used monadically, takes the ceiling). Then, using the index generator, we generate a (0-indexed) list of numbers up to that number minus 1 (because zero indexed). `↓`, when used dyadically, is called drop, and drops elements from arrays. Since we can start clearing at `factor*2`, we can drop the first `factor` elements. Then, we multiply this array by `factor`, and those are the indices to clear.  
The JavaScript equivalent:  
```JavaScript
let bitClearIndices = [];                                  // )
for (let i = 0; i < Math.ceil(n / factor); i++) {          //  ) ⍳⌈n÷factor
    bitClearIndices.push(i);                               //  )
}                                                          // )

for (let i = 0; i < factor; i++) {                         // )
    bitClearIndices.shift();                               //  ) factor↓
}                                                          // )

bitClearIndices = bitClearIndices.map(x => x * factor);    // factor*
bitClearIndices.forEach(index => bits[index] = false);     // bits[ ... ]←0
```

###### `factor`≤50
`↑`, when used dyadically, is called take, and it takes elements from an array. For example, `1↑1 2 3` would return `1`, and `¯3↑1 2 3 4 5` would return `3 4 5`. If you try to take more than the array has, it pads it with 0s (for example, `4↑1 2` would return `1 2 0 0`). `factor↑1` makes a bit array of length `factor` where the first element is a 1 and all others are 0s. Then, `⍴` is used dyadically with `n` to repeat this up to `n`. To optimize, we want to start clearing at `factor*2`, so we want to mask out the first `factor` 1s. `(factor*2)⍴1` will make a mask of `factor*2` 1s. Adding `n↑` to the beginning pads it until length `n` with 0s. To mask out the first `factor` 1s, we make an array that is `array2<array1`, where `array1` is the original array of 1s for multiples of `factor` and `array2` is the "mask". As a smaller example, if we had an array `1 1`, and we wanted to mask out the first 1, we could do `1 0<1 1`. We use the same concept (but this time the `>` sign) to mask this array out of `bits`. This is done using compound assignment. Compound assignment assigns in the original scope, while normal assignment doesn't. We need to add `1⊣` to the beginning, otherwise the dfn will be annoyed with us because it returns no value. When given 2 values, `⊣` always returns the left one.  
JavaScript time!  
```JavaScript
let arr = [];                                           // )
for (let i = 0; i < factor; i++) {                      //  )
    if (i === 0) arr.push(true);                        //   ) factor↑1
    else arr.push(false);                               //  )
}                                                       // )

for (let i = factor; i < n; i++) {                      // )
    arr.push(undefined);                                //  )
}                                                       //   ) n⍴ 
for (let i = factor; i < n; i++) {                      //   )
    arr[i] = arr[i % factor];                           //  )
}                                                       // )

let mask = [];                                          // )
for (let i = 0; i < factor ** 2; i++) {                 //  ) (factor*2)⍴1
    mask.push(true);                                    //  )
}                                                       // )

for (let i = factor ** 2; i < n; i++) {                 // )
    mask.push(false);                                   //  ) n↑
}                                                       // )

let bitsToClear = mask.map((elem, i) => elem < arr[i]); // <

bits = bits.map((elem, i) => elem > bitsToClear[i]);    // 1⊣bits>← (1 is also returned from the APL anonymous dfn that determines which method to use to clear the bits, but that doesn't make any sense in JS)
```

##### Finding the Next Factor
`∘` is called bind, and it composes functions. `2∘+` makes a function that, when called with one argument, adds 2 to it. `f⍣g A` repeats the function `f` on `A` until `g A` returns 1 (or `(f A) g A`, if `g` takes 2 arguments). `⊃`, when used dyadically is called pick, and it takes the ⍺th element of the array ⍵ (and is sensitive to `⎕IO`). In APL, functions in isolation (either assigned to a variable or separated from code by brackets) are called trains, and a 3-train, which is what we have here, is called a fork. With one argument, `(f g h)⍵` will become `(f⍵)g(h⍵)`. f can also be an array, in which case `(A g h)⍵` becomes `A g (h⍵)`. Pick needs an array on the right, but forks can only have an array on the left, so we need to use `⍨`. `(bits⊃⍨⊣)⍵` will become `bits⊃⍨(⊣⍵)`, which reduces to `⍵⊃bits`, allowing us to make a tacit function (as in, a function that doesn't reference it's arguments) that returns the `⍵`th element of `bits`. So, what this line is doing is incrementing the value of `factor` by 2, checking if `bits[the new value]` is prime (1 in the bit array), and if it is, returning it. Then, we set `factor` to the value returned. We need to use compound assignment with `⊢` (when used dyadically called right, returns the right argument) to change `factor` in the outer scope.  
In JS, it this like would look like this:  
```JavaScript
let val = factor;
do {
    val += 2;        // 2∘+
} while (!bits[val]) // ⍣(bits⊃⍨⊣)factor
factor = val;        // factor⊢← (assignment in JS is already in the scope where the variable was defined)
```

#### countPrimes and countPrimesDfn
`f/⍵` will reduce `⍵` by putting `f` between each element. `+/⍵` will sum up each value of the array `⍵`. This is just doing that with the `bits` array, which will count the amount of primes it has.

#### validateResults and validateResultsDfn
Index origin is set to 0.  
`⍷` (find) returns an array of booleans where `⍺` was found in `⍵`. For example, `3⍷1 5 8 3 9` would return `0 0 0 1 0`. `⍸`, when used monadically, is called where and will return an array made out of the indices of truthy values in an array, sensitive to index origin. With `⎕IO←0`, `⍸0 2 0 1 0 1` will return `1 1 3 5` (there was 2 at index 1, and 1 at indices 3 and 5). It returns an empty vector (`⍬`) if there are no truthy values. Line 50 of PrimeSieve.aplc sets `resIndex` to the index of `n` in `sieveSizes`. The next line is a guard: if `resIndex` is `⍬` (meaning that `n` isn't in `sieveSizes`), it returns 0 (false). Otherwise, it returns true if the result of `countPrimes` is the same as the data we already have for that size (and false otherwise).

#### printResults, printResultsDfn and showResultsDfn
Are all self explanatory. All you need to know is that `(a b)←1 2` sets `a` to 1 and `b` to 2, assigning to `⎕` prints data with a newline, and assigning to `⍞` prints data without a newline.

## Run Instructions
This only seems to work on Linux, Dyalog APL doesn't really want to run from the command line.  
See above instructions for running in the repl.
Make sure you have Dyalog APL installed, and added to the path.  
Run `run.sh`

## Output
`garklein-dfn;2654;5.001;1;algorithm=base,faithful=yes,bits=1`
