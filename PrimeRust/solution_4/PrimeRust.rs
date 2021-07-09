//------------------------------------------------------------------------------------------
// PrimeRust.rs : Ported to Rust by Joshua Allen Based on Dave's Garage Prime Sieve - No warranty for anything!
// Also note:  I tried to follow the structure of the CPP version as close as possible and may not be in a perfect Rust style
//------------------------------------------------------------------------------------------

use std::collections::HashMap;
use std::time::{Duration, Instant};


struct PrimeSieve {
    sieve_size: usize,
    results_dictionary: HashMap<usize,usize>,
    bits: Vec<bool>,
}

//builder function for prime sieve
fn build_prime_sieve (sieve_size: usize) -> PrimeSieve {

    // Historical data for validating our results - the number of primes
    // to be found under some limit, such as 168 primes under 1000
    let results_dictionary: HashMap<usize,usize>  = [
        (          10, 4         ),              
        (         100, 25        ),              
        (        1000, 168       ),
        (       10000, 1229      ),
        (      100000, 9592      ),
        (     1000000, 78498     ),
        (    10000000, 664579    ),
        (   100000000, 5761455   ),
        (  1000000000, 50847534  ),
        ( 10000000000, 455052511 ),       
    ].iter().cloned().collect();  //this converts the array to a collection
    
    let n_bits = sieve_size;

    PrimeSieve {
        sieve_size: sieve_size,
        results_dictionary: results_dictionary,
        bits: vec![true; n_bits] //fill up the bits with true based on the sieve size
    }
}

impl PrimeSieve {

    fn validate_results(&self) -> bool {
        let _num_primes: usize;
        match self.results_dictionary.get(&self.sieve_size) {
            Some(_prime) => return true,
            None => return false
        }
       
       
    }
    
    fn run_sieve(&mut self) {
        let mut factor: usize = 3;
        let float_sieve_size: f64 = self.sieve_size as f64;
        let q =  float_sieve_size.sqrt() as usize;
        
   
        while factor <= q {

            let mut num = factor;
            while num < q {

                if self.bits[num]{
                    factor = num;
                    break;
                }

                num +=2;
            }
           
            let mut num = factor * factor;
       
            while num < self.sieve_size {
                self.bits[num] = false;
                num += factor *2;
            }

            factor += 2;
           
        }
    }

    fn count_primes(&self)->usize{
        let mut count = 0;
        if self.sieve_size >= 2{
            count = 1;
        }
        let mut i = 3;
        loop {
            if self.bits[i]{
                count +=1;
            }
            i +=2;
            if i >= self.sieve_size -1{
                break
            }
        }
        return count;
    }

    fn print_results(&self, show_results: bool, duration: Duration, passes: usize){
        if show_results {
            print!("2, ");
        }

        let mut _count = 0;
        if self.sieve_size >= 2{
            _count = 1;
        }
        let mut num = 3;
        loop {
            if self.bits[num]{
                if show_results{
                    print!("{}, ",num);
                }
                _count+=1;
            }

            num +=2;
            if num >= self.sieve_size -1{
                break
            }
        }

        if show_results {
            println!("");
        }
        println!("joshallen64;{};{};1;algorithm=base,faithful=yes",
                passes,
                duration.as_secs_f32()
            
            )
    
    }
    

  }

fn main() {
    let mut passes = 0;
    let time_start = Instant::now();
   
    loop{
        let mut prime_sieve = build_prime_sieve(1000000);
        prime_sieve.run_sieve();
        passes += 1;
        if Instant::now() - time_start >= Duration::new(5,0){
            prime_sieve.print_results(false, Instant::now()-time_start, passes);
            break;
        }
    }
    
}