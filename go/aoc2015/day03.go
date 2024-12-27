package aoc2015

import (
	"errors"
	"fmt"
	"os"
)

type Day03 struct{}

func (d Day03) Solve(path string) {
	file, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		println(path + " does not exist.")
		println(err.Error())
	}
	houses := make(map[complex128]int)
	houses[0] = 0
	robo := make(map[complex128]int)
	robo_position := make([]complex128, 2)
	var position complex128 = 0
	for i, c := range file {
		switch c {
		case '^':
			position += 1i
			robo_position[i%2] += 1i
		case 'v':
			position -= 1i
			robo_position[i%2] -= 1i
		case '>':
			position += 1
			robo_position[i%2] += 1
		case '<':
			position -= 1
			robo_position[i%2] -= 1
		}
		houses[position] += 1
		robo[robo_position[i%2]] += 1
	}
	fmt.Printf("\tDay 03\n\t\tPart 1: %d\n\t\tPart 2: %d\n", len(houses), len(robo))
}
