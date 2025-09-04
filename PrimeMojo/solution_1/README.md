# Mojo solution by Evan Lucas-Currie

There are two primary solutions both of which follow the base algorithm and stay faithful:
* 1bit - This is my attempt to make it 1:1 with the video that Dave has on youtube.
* 8bit - Here we use a boolean array which are 8bits per byte and we simplify the logic but cutting back some calcualtions. Fundementally 

## Run instructions
```
docker build -f DockerFile -t mojosieve .
docker run --rm -it mojosieve
```

(M2 pro Mac)

## Output

```
ELucasCurrie_1Bit;2247;5.0;1;algorithm=base,faithful=yes,bit=1
ELucasCurrie_8bit;12844;5.0;1;algorithm=base,faithful=yes,bit=8
```
