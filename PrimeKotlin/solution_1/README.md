# Kotlin solution by Xvolks

This implementation takes advantage of Koltin ability to produce
code/bytecode for different targets.

As an example, I have included in the `build.gradle.kts` file
the generation of macOS, Linux and Windows executables and 
a version for the JVM.



## Run instructions

The easy way.

There are 2 run-xxxx.sh files.
`./run-native.sh` will build the kexe (executable file) then run it natively.

`./run-on-jvm.sh` wil build the JAR file then run it on a JVM


**OR**

The command (`gradlew.bat` for windows)
`./gradlew build fatJar`

will build the current platform native artifact, and JVM artifact.
You can find them in `build` subdirectory.
You can then run each artifact as usual for your platform or Java.

**OR**

To run the application use the provided `gradlew` script.

`./gradlew runReleaseExecutableNative`

This should download the build tools, build the native executable
for your current platform and run it.

## Output

### Hardware
- MacBook Pro (16 pouces, 2019) 
- 2,3 GHz Intel Core i9


### Results
#### Native
    ./build/bin/native/releaseExecutable/PrimeKotlin.kexe
    xvolks-native;1334;5.002;1;algorithm=base;faithful=yes,bits=unknown

#### JVM
    java -jar build/libs/PrimeKotlin-fat-1.0.0-SNAPSHOT.jar                                                                           16.6s ? Sat Jul 24 08:08:44 2021
    xvolks-jvm;5241;5.0;1;algorithm=base;faithful=yes,bits=unknown

### Note
The results in native compilation is painfully slow. This is still in development at
https://github.com/JetBrains/kotlin




