with recursive 
    naturals(n)
-- init of the array
as (
    select 2
    union all
        select n+1 from naturals where n=2
    union all
        select n+2 from naturals where n>2 and n+2<=1000000
),
    product (num,not_prime)
as (
    -- n*n is the start.
    -- so if prime number is 3 we start at 9 with crossing off
    -- for prime 5 we start at 25
    -- what about 15 then in this case? 15 is already crossed of by
    -- the processing of prime 3
    -- downside of the approach below is that we are also crossing off for multiplications 
    -- of not primes
    select n, n*n as sqr
      from naturals
      where 
            sqr <= (select max(n) from naturals)
        and n !=2 -- this filters out all the recursive calls for evennumbers!
    union all -- all because we know there is no overlap between the two sets, is a bit faster than just union
    select 
      num, -- because recursive does not allow to reuse n
      not_prime+2*num as prod --2*num because we know that every other number must be even
    from
      product
    where
      prod <= (select max(n) from naturals)
  )
select count(*) from (
    select 
        n,
        case
            when not_prime is null then 1
        else 0
        end as isPrime
    from naturals
    left join (select distinct not_prime from product) unique_list on (
        naturals.n = unique_list.not_prime
    )
) sieve
where
    sieve.isPrime = 1
;