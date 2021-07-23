# Go solution by zanicar

1-bit multi-threaded Go solution (adapted from solution provided by ssovest)

## Run instructions

 - Install [Go](https://golang.org/)

 - Run:
```
go run main.go [args]
```
Command line args:

`-limit X`: Limit. Default is 1000000

`-time X`: Duration, in [Go duration format](https://golang.org/pkg/time/#ParseDuration). Default is "5s"

`-routines X`: Go routines, starts X-1 additional go routines

## Output

CPU: Intel i7-4720HQ (8) @ 3.6GHz

Memory: 15924MiB

OS: Garuda Linux x86_64

go version go1.16.5 linux/amd64

Single Threaded
```
zanicar-go;5838;5.000763462;1;algorithm=base,faithful=yes,bits=1
```

Multi Threaded
```
zanicar-go;20547;5.00282151s;8;algorithm=base,faithful=yes,bits=1
```
