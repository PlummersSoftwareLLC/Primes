use std::time::Instant;

struct PrimeSieve{
    bits: Vec<bool>,
    size: usize // usize means unsigned 32-bit or 64-bit integer ().
}
impl PrimeSieve{
    pub(crate) fn new(n: usize) -> Self{
        return Self{bits: vec![true; n], size: n}
    }
    pub(crate) fn run_sieve(&mut self){
        let mut factor: usize = 3;
        let q = (self.size as f32).sqrt() as usize;
        while factor <= q { 
            while factor < q{  // search for the  next number that is true (a prime)
                if self.bits[factor]{
                    break;
                }
                factor +=2;
            }
            for num in (factor*factor..=self.size).step_by(factor*2){
                self.bits[num] = false;  // mark all multiples of this number as not prime
            }
            factor += 2;
        }
    }
    pub(crate) fn print_results(&self, duration: f64, passes: u32){
        eprintln!("Author; Passes; Time; Threads");  
        println!("Blui42;{};{};1;algorithm=base,faithful=yes", passes, duration);  // print required output to standard out
        eprintln!("{} Passes per second", (100.0*(passes as f64)/duration).round()/ 100.0);
    }
    #[allow(dead_code)]
    pub(crate) fn eprint_detailed(&self){
        eprint!("2"); 
        for num in (3..=self.size).step_by(2){
            if self.bits[num] {
                eprint!(", {}", num);  // print every prime to standard error
            }
        }
        eprintln!();
    }
    pub(crate) fn count_primes(&self) -> u32{
        let mut count: u32 = 0; 
        for x in self.bits.iter().skip(1).step_by(2) {
            if *x {
                count += 1
            }
        }
        return count
    }
}

fn main() {
    let mut passes: u32 = 0; // unsigned 32-bit integer, is mutable
    let start_time = Instant::now();
    loop{
        let mut sieve = PrimeSieve::new(1_000_000);
        sieve.run_sieve();
        passes += 1;
        if Instant::now().duration_since(start_time).as_secs() >= 5 {
            sieve.print_results(Instant::now().duration_since(start_time).as_secs_f64(), passes);
            // sieve.eprint_detailed();  // comment this out, it takes forever to finish
            eprintln!("For Validation: {} Primes were found", sieve.count_primes());
            return
        }
    }
}
