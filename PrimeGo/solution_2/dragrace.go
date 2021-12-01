package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/pprof"
	"sort"
	"strconv"
	"strings"
)

// RaceConfigEntry represents a parsed config entry containing all
// necessary information to make and run a drag-race benchmark
type RaceConfigEntry struct {
	StorageName string   `json:"storage"`
	Label       string   `json:"label"`
	Sieve       string   `json:"sieve"`
	Algorithm   string   `json:"algorithm"`
	Faithful    bool     `json:"faithful"`
	Bits        int      `json:"bits"`
	StorageMods []string `json:"storage_mods"`
	BlockSize   uint64   `json:"block_size"`
}

// Tags represents an implementation's tags used in the drag-race mode
type Tags map[string]string

// MakeTags makes Tags from a config entry
func MakeTags(ce RaceConfigEntry) Tags {
	tags := make(Tags)

	tags["algorithm"] = ce.Algorithm

	if ce.Bits > 0 {
		tags["bits"] = strconv.Itoa(ce.Bits)
	}

	if ce.Faithful {
		tags["faithful"] = "yes"
	} else {
		tags["faithful"] = "no"
	}

	return tags
}

// String returns string representation of Tags to be used in the output
func (t Tags) String() string {
	items := make([]string, 0, len(t))
	for key := range t {
		items = append(items, key+"="+t[key])
	}
	// Go maps intentionally return keys in random order, so we need
	// to do some sorting, just for readability.
	sort.Strings(items)
	return strings.Join(items, ",")
}

// DragRaceRunner provides high-level interface for running benchmarks and
// outputting their results.
type DragRaceRunner struct {
	OutStream io.Writer
}

// ReadConfig reads and parses a config containing implementations
// participating in the drag-race. If successful, it returns a slice of
// parsed config entries.
func ReadConfig(r io.Reader) ([]RaceConfigEntry, error) {
	conf, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}

	result := make([]RaceConfigEntry, 0)
	if err := json.Unmarshal(conf, &result); err != nil {
		return nil, err
	}

	return result, nil
}

// Run runs all bencmarks from the provided slice and outputs their results
func (d DragRaceRunner) Run(benchmarks []*Benchmark, flags RunFlags) error {
	for _, bench := range benchmarks {
		res, err := d.RunOne(bench, flags)
		if err != nil {
			return err
		}
		if err := d.OutputResult(res, flags); err != nil {
			return err
		}
	}
	return nil
}

// RunOne runs one benchmark and returns its result. If -prof flag is provided,
// it will also write cpu profile info to a file on disk.
func (d DragRaceRunner) RunOne(bench *Benchmark, flags RunFlags) (result BenchmarkResult, err error) {
	if flags.Prof {
		var f io.WriteCloser
		if f, err = os.Create(filepath.Join("profile", bench.Label)); err != nil {
			return
		}
		defer func() {
			err = f.Close()
		}()

		if err = pprof.StartCPUProfile(f); err != nil {
			return
		}
		defer pprof.StopCPUProfile()
	}
	result = bench.Run(flags.Limit, flags.Duration)
	return
}

func padRight(s string, w int) string {
	if len(s) >= w {
		return s
	}
	return s + strings.Repeat(" ", w-len(s))
}

// OutputResult prints benchmark result to the DragRaceManager's output stream.
func (d DragRaceRunner) OutputResult(res BenchmarkResult, flags RunFlags) error {
	if flags.Dragrace {
		fmt.Fprintf(d.OutStream, "%v;%v;%v;1;%v\n", res.Label, res.Passes, res.Duration, res.Tags)
	} else {
		fmt.Fprintf(d.OutStream, "%v\t%v\t%v\t%d\t%v\n",
			padRight(res.Label, 16), res.Passes, res.Duration, res.Count, res.Valid)
	}
	return nil
}
