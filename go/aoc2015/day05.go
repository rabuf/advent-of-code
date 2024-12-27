package aoc2015

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"slices"
)

type Day05 struct{}

func (d Day05) Solve(path string) {
	file, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		println(path + " does not exist.")
		println(err.Error())
	}
	scanner := bufio.NewScanner(file)
	p1, p2 := 0, 0
	if err != nil {
		println(err.Error())
	}
	for scanner.Scan() {
		line := scanner.Text()

		r1 := 0
		r2 := false
		r3 := true
		r4 := false
		r5 := false
		for i := range len(line) {
			if slices.Contains([]byte{'a', 'e', 'i', 'o', 'u'}, line[i]) {
				r1++
			}
			if i < len(line)-1 && line[i] == line[i+1] {
				r2 = true
			}
			if i < len(line)-1 {
				r3 = r3 && line[i:i+2] != "ab" && line[i:i+2] != "cd" && line[i:i+2] != "pq" && line[i:i+2] != "xy"
			}
			if i < len(line)-2 && line[i] == line[i+2] {
				r4 = true
			}
			if i < len(line)-3 {
				for j := range len(line) - i - 3 {
					if i+j+2 < len(line) && line[i] == line[i+j+2] && line[i+1] == line[i+j+3] {
						r5 = true
					}
				}
			}
		}
		if r1 >= 3 && r2 && r3 {
			p1++
		}
		if r4 && r5 {
			p2++
		}
	}
	fmt.Printf("\tDay 05\n\t\tPart 1: %d\n\t\tPart 2: %d\n", p1, p2)
}
