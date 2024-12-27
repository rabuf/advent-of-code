package main

import (
	"errors"
	"fmt"
	"main/aoc2015"
	"os"
	"sync"
)

type Solver interface {
	Solve(string)
}

func main() {
	if _, err := os.Stat(os.Args[1]); errors.Is(err, os.ErrNotExist) {
		println(err.Error())
	}
	var years = make(map[int][]Solver)
	years[2015] = []Solver{aoc2015.Day01{}, aoc2015.Day02{}, aoc2015.Day03{}, aoc2015.Day04{}, aoc2015.Day05{}}
	years[2016] = []Solver{}
	wg := sync.WaitGroup{}
	for year, days := range years {
		fmt.Printf("Year %d\n", year)
		for i, d := range days {
			f := fmt.Sprintf("%s/%d/%02d.txt", os.Args[1], year, i+1)
			wg.Add(1)
			go func(d Solver) {
				d.Solve(f)
				wg.Done()
			}(d)
		}
		wg.Wait()
	}
}
