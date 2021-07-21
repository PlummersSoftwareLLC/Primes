import sys.net.Host;
import haxe.ValueException;
import haxe.exceptions.ArgumentException;
import haxe.ds.List;

class PrimeSieve {
    var sieveSize:Int;
    var rawbits:Array<Bool>;
    var primeCounts:Map<Int,Int> = [
        10 => 4,
        100 => 25,
        1000 => 168,
        10000 => 1229,
        100000 => 9592,
        1000000 => 78498,
        10000000 => 664579,
        100000000 => 5761455
    ];

    public function new(limit:Int):Void {
        sieveSize = limit;
        rawbits = [for (i in 0...Std.int((limit+1)/2)) true];
    }

    function ValidateResults():Bool {
        if (primeCounts[sieveSize] != null) return primeCounts[sieveSize] == countPrimes()
        else return false;
    }

    function GetBit(index:Int):Bool {
        if (index % 2 == 0) return false;
        else return rawbits[Std.int(index/2)];
    }

    function clearBit(index:Int):Void {
        if (index % 2 == 0) throw new ArgumentException("index","If you're setting even bits, you're sub-optimal for some reason!");
        else rawbits[Std.int(index/2)] = false;
    }

    public function runSieve():Void {
        var factor = 3;
        var q = Math.sqrt(sieveSize);

        while (factor < q) {
            for (num in factor...sieveSize) {
                if (GetBit(num)) {
                    factor = num;
                    break;
                }
            }

            var num = factor * 3;
            while (num < sieveSize) {
                num += factor * 2;
                clearBit(num);
            }
            factor += 2;
        }
    }

	function countPrimes():Int {
		return rawbits.filter((b) -> b).length;
	}

    public function printResults(showResults:Bool, duration:Float, passes:Int) {
        if (showResults) Sys.print("2, ");
        var count = 1;
        for (num in 3...sieveSize) {
            if (GetBit(num)) {
                if (showResults) Sys.print(Std.string(num) + ", ");
                count += 1;
            }
        }
        if (count != this.countPrimes()) throw new ValueException("Terrible error");

        Sys.println("\nPasses: " + Std.string(passes) + ", Time " + Std.string(duration) + ", Avg: " + Std.string(duration/passes) + ", Limit: " + Std.string(this.sieveSize) + ", Count: " + Std.string(count) + ", Valid: " + Std.string(this.ValidateResults()));
    }
}

class Main {
    static public function main():Void {
        var tStart:Float = Sys.time();
        var passes:Int = 0;
        var sieve:PrimeSieve = null;
        while (Sys.time() - tStart < 5) {
            sieve = new PrimeSieve(1000000);
            sieve.runSieve();
            passes += 1;
        }
        var tD:Float = Sys.time() - tStart;
        //sieve.printResults(false,tD,passes);
        #if interp
           var language = 'HaxeEval';
        #elseif cpp
            var language = "C++";
        #elseif python
            var language = "Python";
        #else 
            var language = "UnknownTarget";
        #end
        Sys.println("TayIorRobinson_Haxe_" + language + ";" + Std.string(passes) + ";" + Std.string(tD) + ";1;algorithm=base,faithful=yes");
    }
}