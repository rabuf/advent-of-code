package aoc2015

import (
	"errors"
	"fmt"
	"os"
)

type Day01 struct{}

func (d Day01) Solve(path string) {
	file, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		println(path + " does not exist.")
		println(err.Error())
	}
	floor, index := 0, 0
	for i, c := range file {
		if c == '(' {
			floor++
		} else {
			floor--
		}
		if floor == -1 && index == 0 {
			index = i + 1
		}
	}
	fmt.Printf("\tDay 01\n\t\tPart 1: %d\n\t\tPart 2: %d\n", floor, index)
}
