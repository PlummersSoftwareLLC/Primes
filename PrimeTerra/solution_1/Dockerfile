FROM ubuntu:20.04

RUN wget https://github.com/terralang/terra/releases/download/release-1.0.6/terra-Linux-x86_64-6184586.tar.xz &&
    tar -xvf terra-Linux-x86_64-6184586.tar.xz && 
    PATH=~/terra-Linux-x86_64-6184586/bin:$PATH &&
    terra Primes.t