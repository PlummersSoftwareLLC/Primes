@echo off

java -version 1>nul 2>nul || (
   echo "Requires Java 11+"
   exit /b 2
)

java "./src/main/java/PrimeSieveJavaBitSet.java"
