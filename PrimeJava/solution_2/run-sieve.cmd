@echo off

java -version 1>nul 2>nul || (
   echo "Requires JDK 11+"
   exit /b 2
)

java "./src/PrimeSieveJavaBitSet.java"
