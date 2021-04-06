function getBit {
    param([Parameter(Mandatory=$True)][int]$getBitIndex)
    if ($getBitIndex % 2 -eq 0){
        return $false
    }else{
        return $bitarray[$getBitIndex / 2 + .5]
    }
}

function clearBit() {
    param([Parameter(Mandatory=$True)][int]$clearBitIndex)
    if (( $clearBitIndex % 2 -eq 0 )){
		Write-Host "You are setting even bits, which is sub-optimal"
		return $false
        }
    $bitArray[$clearBitIndex / 2 + .5] = $false
}

function runSieve() {
    param([Parameter(Mandatory=$True)][int]$localsieveSize, [bool]$writedata=$false)
    $factor = 3
    $q = [math]::Sqrt($localsieveSize)
    while ($factor -le $q){
        Write-Host "Factor:$factor"
        for ($num = $factor; $num -lt $localsieveSize; $num +=2){
            if ($writedata){Write-Host "GetBitNum:$num"}
            if (getBit -getBitIndex $num){
                $factor = $num
                if ($writedata){Write-Host "$num Breaking"}
                break
            }
        }
        for ($num = ($factor * $factor); $num -lt $localsieveSize; $num += ($factor*2)){
            if ($writedata){Write-Host "ClearBitNum:$num"}
            clearBit -clearBitIndex $num
        }
    $factor += 2
    }
}

Function countPrimes(){
    $count = 0
    for ($i = 1; $i -lt $bitArray.Count; $i++){
        if ($bitArray[$i]){
            $count++;
            }
        }
return $count;
}

###initialize bitarry###
$sieveSize = 100000

$mysieveSize = ($sieveSize / 2)
$bitarray = New-Object System.Collections.ArrayList(,(1..($mysieveSize  + 1)))  
$bitarray[1]=$false

$index = 0
while ($index -lt $mysieveSize){
$index ++
$bitarray[$index] = $true
}


cls
Write-Host "Finding primes up to $sieveSize"
runSieve -localsieveSize $sieveSize #-writedata $true

Write-Host "Number of primes found:$(countPrimes)"