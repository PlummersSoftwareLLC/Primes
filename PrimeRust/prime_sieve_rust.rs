//Rust Prime Sieve
use std::vec::Vec;
use std::time::{Instant};
use std::collections::HashMap;

fn validateResults(sievesize:i64, primecount:i64)->bool{
    // to be found under some limit, such as 168 primes under 1000
    // Historical data for validating our results - the number of primes
    let results_map: HashMap<i64, i64> = 
    [(10, 4),               
    (100, 25),               
    (1000, 168),
    (10000, 1229),
    (100000, 9592),
    (1000000, 78498),
    (10000000, 664579),
    (100000000, 5761455),
    (1000000000, 50847534),
    (10000000000, 455052511)].iter().cloned().collect();

    if results_map[&sievesize] == primecount{
        return true;

    }
    else
    {
        return false;
    }
}

fn prime_seive(size: usize) -> Vec<bool>{
    let seivesize = size;
    let mut primevec:Vec<bool> = Vec::new();
    primevec = vec![true; (seivesize+1)/2];
    return primevec
}

fn GetBit(index: i64, primevec: &Vec<bool>) -> bool{
    if index % 2 == 0 {
        return false;
    }
    else {
        return primevec[((index/2) as usize)];
    }
}

fn ClearBit(index:i64, mut primevec: Vec<bool>){
        if index % 2 == 0 {
            println!("You are setting even bits, which is sub-optimal");
            return;
        }
        primevec[(index / 2) as usize ] = false;      

}

fn runSieve(sievesize: i64) -> Vec<bool>{
    let mut factor:i64 = 3;
    let q = (sievesize as f64).sqrt() as i64;
    let mut primevec = prime_seive(sievesize as usize);
    while factor < q {
        for num in (factor .. sievesize) {

            if GetBit(num, &primevec) == true{
                
                factor = num;
                
                break;
            }
        }
        let stepsize:usize = (factor * 2) as usize;
        for num in ((factor * 3) .. sievesize).step_by(stepsize){
            //ClearBit(num, primevec)
            if num % 2 ==0 {
                println!("You are setting even bits, which is sub-optimal");
            }
            primevec[(num/2) as usize] = false; 
        }

        factor = factor + 2;
    }
    return primevec



}

fn printResults(sievesize:i64, primevec: &Vec<bool>, showResults:bool, passes:i64, duration:u64){
    if showResults{
        //println!("2, ");
        let mut count:i64 = 1;
        for num in 3..sievesize{
            if GetBit(num, &primevec) {
                    if showResults {
                        //println!("{}, ", num);
                        count = count + 1;
                    }
            }
            
            if showResults{
                
                
                //print!("{}:{}|", num, GetBit(num, &primevec));
                
            
                    //println!("");
            }
        }
        println!("Passes: {}, Time: {}, Avg: {}, Limit: {}, Count: {}, Valid: {}",passes, duration, (duration as i64 / passes), sievesize, count, validateResults(sievesize, count));
    }
}


fn main() {
    let start = Instant::now();
    
    
    let mut passes: i64 = 0;
    //let prime_sieve: Vec<bool> = vec![false;0];
    let sievesize: i64 = 1000000;
    let mut primevec: Vec<bool> = prime_seive(sievesize as usize);
    
    while start.elapsed().as_secs() < 10 {
        //println!("{}", start.elapsed().as_secs());
        primevec= runSieve(sievesize);
        passes = passes + 1;
    }

    let duration = start.elapsed();
    printResults(sievesize, &primevec, true, passes, duration.as_secs());

}
