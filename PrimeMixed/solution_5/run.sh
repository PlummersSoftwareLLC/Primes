#!/bin/bash

g++ -O3 -Wall -std=c++11 main.cpp -lm -lpthread -o cppasm

./cppasm
