FROM ubuntu:20.04

WORKDIR ./

RUN wget https://github.com/terralang/terra/releases/download/release-1.0.6/terra-Linux-x86_64-6184586.tar.xz &&
    mkdir Terra &&
    tar -C "Terra" -xvf terra-Linux-x86_64-6184586.tar.xz --strip-components 1 && 
    PATH=~/Terra/bin:$PATH &&
    terra Primes.t