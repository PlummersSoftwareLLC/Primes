//Rust Prime Sieve
use std::vec::Vec;
use std::time::{Instant};
use std::collections::HashMap;

fn validate_results(sievesize:i64, primecount:i64)->bool{
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
    let primevec = vec![true; (seivesize+1)/2];
    return primevec
}

fn get_bit(index: i64, primevec: &Vec<bool>) -> bool{
    if index % 2 == 0 {
        return false;
    }
    else {
        return primevec[((index/2) as usize)];
    }
}

//fn clear_bit(index:i64, mut primevec: Vec<bool>){
//       if index % 2 == 0 {
//            println!("You are setting even bits, which is sub-optimal");
//            return;
//        }
//        primevec[(index / 2) as usize ] = false;      
//
//}

fn run_sieve(sievesize: i64) -> Vec<bool>{
    let mut factor:i64 = 3;
    let q = (sievesize as f64).sqrt() as i64;
    let mut primevec = prime_seive(sievesize as usize);
    while factor < q {
        for num in factor .. sievesize {

            if get_bit(num, &primevec) == true{
                
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

fn print_results(sievesize:i64, primevec: &Vec<bool>, show_results:bool, passes:i64, duration:u64){
    if show_results{
        print!("2, ");
        let mut count:i64 = 1;
        for num in 3..sievesize{
            if get_bit(num, &primevec) {
                    if show_results {
                        print!("{}, ", num);
                        count = count + 1;
                    }
            }
            

        }
        if show_results{
            println!("");
        }
        println!("Passes: {}, Time: {}, Avg: {}, Limit: {}, Count: {}, Valid: {}",passes, duration,
         ((duration as f64) / (passes as f64)), sievesize, count, validate_results(sievesize, count));
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
        primevec= run_sieve(sievesize);
        passes = passes + 1;
    }

    let duration = start.elapsed();
    print_results(sievesize, &primevec, true, passes, duration.as_secs());

}
