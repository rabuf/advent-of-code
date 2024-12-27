package aoc2015

import "testing"

func Test_paperSize(t *testing.T) {
	type args struct {
		nums []int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{"2x3x4", args{[]int{2, 3, 4}}, 58},
		{"1x1x10", args{[]int{1, 1, 10}}, 43},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := paperSize(tt.args.nums); got != tt.want {
				t.Errorf("paperSize() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_ribbonLength(t *testing.T) {
	type args struct {
		nums []int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{"2x3x4", args{[]int{2, 3, 4}}, 34},
		{"1x1x10", args{[]int{1, 1, 10}}, 14},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := ribbonLength(tt.args.nums); got != tt.want {
				t.Errorf("ribbonLength() = %v, want %v", got, tt.want)
			}
		})
	}
}
