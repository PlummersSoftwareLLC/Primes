echo "Compiling the class"
javac PrimeSieveJava.java
echo "Running sieve"
java -cp . PrimeSieveJava "$1" "$2"
