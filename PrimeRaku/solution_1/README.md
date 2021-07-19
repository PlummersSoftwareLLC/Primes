# Raku solution by draco1006

Direct translation from the original c++ implementation to the programming language Raku

## Run instructions

interprete with the rakudo compiler using the best optimization level.

`$ raku --optimize=0 prime.rk`

Alternatively, you can use docker to run this code from a container.

```
$ docker build -t drag-race .
$ docker run drag-race
```

## Output

```
$ docker run drag-race
draco1006;3;7.034031;1;algorithm=base,faithful=yes
```
