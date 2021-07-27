⎕FIX⊃⎕NGET'./PrimeSieve.aplc' 1
∇main
    passes←0
    tEnd←5000+12⎕DT⊂⎕TS

    {
        sieve←⎕NEW PrimeSieve 1000000
        _←sieve.runSieve
        passes+←1
        (12⎕DT⊂⎕TS)≥tEnd:sieve.printResults 0 (1000÷⍨5000+(12⎕DT⊂⎕TS)-tEnd) passes
        ∇0
    }0
∇
main

