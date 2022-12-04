#include <gtest/gtest.h>
#include "day03.h"

std::string sample = "vJrwpWtwJgWrhcsFMMfFFhFp\n"
                     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n"
                     "PmmdzqPrVvPwwTWBwg\n"
                     "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n"
                     "ttgJtRGJQctTZtZT\n"
                     "CrZsJsPPZsGzwwsLwLmpwMDw";

std::vector<int> sample_priorities{16, 38, 42, 22, 20, 19};

TEST(Day03, RucksackPriority) {
    int index = 0;
    std::istringstream iss{sample};
    for (std::string line; getline(iss, line); ) {
        EXPECT_EQ(sample_priorities[index], rucksack_priority(line));
        index++;
    }
}

TEST(Day03, RucksackPrioritySum) {
    std::istringstream iss{sample};
    std::vector<std::string> rucksacks;
    day03_parse(iss, rucksacks);
    EXPECT_EQ(157, rucksack_sum(rucksacks));
}