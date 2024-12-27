package aoc2015

import (
	"crypto/md5"
	"fmt"
)

type Day04 struct{}

func (d Day04) Solve(_ string) {
	key := "ckczppom"
	nonce5, nonce6 := 0, 0
	for i := 0; nonce5 == 0 || nonce6 == 0; i++ {
		nonced := fmt.Sprintf("%s%d", key, i)
		hash := md5.Sum([]byte(nonced))

		if hash[0] == 0 && hash[1] == 0 && hash[2] < 16 && nonce5 == 0 {
			nonce5 = i
		}
		if hash[0] == 0 && hash[1] == 0 && hash[2] == 0 && nonce6 == 0 {
			nonce6 = i
		}
	}
	fmt.Printf("\tDay 04\n\t\tPart 1: %d\n\t\tPart 2: %d\n", nonce5, nonce6)
}
