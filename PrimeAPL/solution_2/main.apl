⎕FIX⊃⎕NGET'./PrimeSieve.aplc' 1
∇main
    passes←0
    tEnd←5000+12⎕DT⊂⎕TS
    
    :While 1
        sieve←⎕NEW PrimeSieve 1000000
        sieve.runSieve
        passes+←1
        :If (12⎕DT⊂⎕TS)≥tEnd
            sieve.printResults 0 (1000÷⍨5000+(12⎕DT⊂⎕TS)-tEnd) passes
            :Leave
        :EndIf
    :EndWhile
∇
main

