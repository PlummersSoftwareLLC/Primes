//go:generate go run ./tools/typegen.go

package main

import (
	"flag"
	"os"
	"regexp"
	"time"
)

type FactoryProps struct {
	blockSize uint64
}

func NewFactoryPropsFromFlags(flags RunFlags) FactoryProps {
	return FactoryProps{flags.BlockSize}
}

func NewFactoryPropsFromConfig(ce RaceConfigEntry) FactoryProps {
	return FactoryProps{ce.BlockSize}
}

type RunFlags struct {
	Limit     uint64
	Duration  time.Duration
	BlockSize uint64
	Filter    *regexp.Regexp
	Dragrace  bool
	Prof      bool
}

const DefaultLimit uint64 = 1_000_000
const DefaultDuration float64 = 5
const DefaultBlockSize uint64 = 128_000
const DragRaceConfigPath string = "configs/drag-race.json"

func main() {

	runner := DragRaceRunner{os.Stdout}

	var flags RunFlags
	var regex string
	var benchmarks []*Benchmark
	var err error

	flag.Uint64Var(&flags.Limit, "limit", 1_000_000, "limit")
	flag.DurationVar(&flags.Duration, "time", 5*time.Second, "duration")
	flag.Uint64Var(&flags.BlockSize, "block", 128_000, "Block size for 'blocks' and 'segmented' implementations, in bits")
	flag.StringVar(&regex, "run", ".", "Run only implementations that match a provided regexp")
	flag.BoolVar(&flags.Dragrace, "drag-race", true, "Drag-race mode. If true, all other flags do nothing.")
	flag.BoolVar(&flags.Prof, "prof", false, "Profile each implementation being run. Profile info will be written to a file named after implementation's label in the 'profile' folder")

	flag.Parse()

	flags.Filter = regexp.MustCompile(regex)

	if flags.BlockSize == 0 {
		flags.BlockSize = DefaultBlockSize
	}

	if flags.Dragrace {

		flags = RunFlags{
			Limit:    DefaultLimit,
			Duration: time.Duration(DefaultDuration) * time.Second,
			Dragrace: true,
		}

		in, err := os.Open(DragRaceConfigPath)
		if err != nil {
			in.Close()
			os.Stderr.WriteString(err.Error() + "\n")
			return
		}

		conf, err := ReadConfig(in)
		in.Close()
		if err != nil {
			os.Stderr.WriteString(err.Error() + "\n")
			return
		}

		benchmarks, err = MakeBenchmarks(conf)
		if err != nil {
			os.Stderr.WriteString(err.Error() + "\n")
			return
		}

	} else {

		benchmarks = AllBenchmarks(flags.Filter, NewFactoryPropsFromFlags(flags))

	}

	if err = runner.Run(benchmarks, flags); err != nil {
		os.Stderr.WriteString(err.Error() + "\n")
		return
	}
}
