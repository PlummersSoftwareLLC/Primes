# Go solution by ssovest

Collection of single-threaded Go solutions.

## Run instructions

 - Install [Go](https://golang.org/)

 - Generate code
```
cd path/to/solution
go generate
```

 - (Optional) Run tests
```
go test
```

 - Build
```
go build -gcflags="-B"
```

 - Run
```
./primego [args]
```

### Command line args:

`-drag-race=X`: Run the solution in the drag-race mode. If `true`, it will run implementations from the preset defined in `configs/drag-race.json` and produce output in the "official" format. Other flags do nothing in the drag-race mode. Default is `true`.

`-limit=X`: Limit. Default is `1_000_000`.

`-time=X`: Duration, in [Go duration format](https://pkg.go.dev/time@go1.17#ParseDuration). Default is `"5s"`.

`-run=X`: Run only implementations that match a provided [Go regex](https://pkg.go.dev/regexp/syntax@go1.17). Default is `"."`.

`-block=X`: Block size for `blocks` and `segmented` versions, *in bits*. Default is `128_000`.

`-prof`: If set, running implementations will be profiled, profile data will be written to the `profile` folder to a file with a name corresponding to the implementation's label. Refer to https://go.dev/blog/pprof for some info on how to use Go profiling tools.

## Output

AMD A4-3305M 1.9 GHz, Windows 7 64 bit
```
ssovest-go-other-u64;16367;5.0028852;1;algorithm=other,bits=1,faithful=yes
ssovest-go-other-u32-seg-16k;12225;5.0142868;1;algorithm=other,bits=1,faithful=yes
ssovest-go-other-u32-block-16k;11817;5.0012861;1;algorithm=other,bits=1,faithful=yes
ssovest-go-other-u32-rblock-16k;11486;5.0022861;1;algorithm=other,bits=1,faithful=yes
ssovest-go-stride-u32;9644;5.0022861;1;algorithm=base,bits=1,faithful=yes
ssovest-go-stride-u32-block-16k;8194;5.0022861;1;algorithm=base,bits=1,faithful=yes
ssovest-go-stride-u32-rblock-16k;8259;5.0022861;1;algorithm=base,bits=1,faithful=yes
ssovest-go-simple-u32;3005;5.0018852;1;algorithm=base,bits=1,faithful=yes
```
