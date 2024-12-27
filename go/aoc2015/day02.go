package aoc2015

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Day02 struct{}

func (d Day02) Solve(path string) {
	file, err := os.Open(path)
	if errors.Is(err, os.ErrNotExist) {
		println(path + " does not exist.")
		println(err.Error())
	}
	r, _ := regexp.Compile("\\d+")
	b := bufio.NewScanner(file)
	paper, ribbon := 0, 0
	for b.Scan() {
		line := b.Text()
		nums := make([]int, 0)
		for _, n := range r.FindAllString(line, -1) {
			num, _ := strconv.Atoi(n)
			nums = append(nums, num)
		}
		paper += paperSize(nums)
		ribbon += ribbonLength(nums)
	}
	fmt.Printf("\tDay 02\n\t\tPart 1: %d\n\t\tPart 2: %d\n", paper, ribbon)
}

func paperSize(nums []int) int {
	return 2*(nums[0]*nums[1]+nums[1]*nums[2]+nums[0]*nums[2]) + min(nums[0]*nums[1], nums[1]*nums[2], nums[0]*nums[2])
}

func ribbonLength(nums []int) int {
	return nums[0]*nums[1]*nums[2] + 2*min(nums[0]+nums[1], nums[0]+nums[2], nums[1]+nums[2])
}
