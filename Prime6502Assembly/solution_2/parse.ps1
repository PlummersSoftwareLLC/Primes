c1541 .\primes.d64 -read output,s .\output.txt | Out-Null
Get-Content output.txt | ForEach-Object {
    $fields = $_.ToString().Split(' ');
    if ($fields[0] -eq "VALID" -and $fields[1] -ne "Y") {
        "ERROR: prime count incorrect"
        break
    }
    elseif ($fields[0] -eq "TIME") {
        "rbergen-pet;1;$([Convert]::ToInt32($fields[1])/60);1;algorithm=base,faithful=no,bits=1"
    }
}
Remove-Item output.txt
