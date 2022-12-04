#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>
#include <set>

#include "day03.h"

int priority_value(char c) {
    if (std::isupper(c)) {
        return 27 + c - 'A';
    } else {
        return 1 + c - 'a';
    }
}

int rucksack_priority(std::string &rucksack) {
    auto mid = rucksack.begin() + std::distance(rucksack.begin(), rucksack.end()) / 2;
    std::set<char> c1{rucksack.begin(), mid};
    std::set<char> c2{mid, rucksack.end()};
    std::set<char> intersection;
    std::set_intersection(c1.begin(), c1.end(), c2.begin(), c2.end(),
                          std::inserter(intersection, intersection.begin()));
    return priority_value(*intersection.begin());
}

int rucksack_sum(const std::vector<std::string> &rucksacks) {
    std::vector<std::string> copy;
    std::copy(rucksacks.begin(), rucksacks.end(), std::back_inserter(copy));
    return std::transform_reduce(copy.begin(), copy.end(), 0,
                                 std::plus{}, rucksack_priority);
}

int group_priority(auto begin, auto end) {
    std::vector<std::set<char>> bags;
    std::for_each(begin, end, [&bags](auto it) {
        std::set<char> bag{it.begin(), it.end()};
        bags.push_back(bag);
    });

    auto result = std::reduce(bags.begin() + 1, bags.end(), *bags.begin(),
                              [](auto acc, auto bag) {
                                  std::set<char> next;
                                  std::set_intersection(bag.begin(), bag.end(), acc.begin(), acc.end(),
                                                        std::inserter(next, next.begin()));
                                  return next;
                              });
    return priority_value(*result.begin());
}

int badge_sum(const std::vector<std::string> &rucksacks) {
    int sum = 0;
    for (auto it = rucksacks.begin(); it < rucksacks.end(); it += 3) {
        sum += group_priority(it, it + 3);
    }
    return sum;
}

void day03_parse(std::istream &in, std::vector<std::string> &rucksacks) {
    for(std::string line; std::getline(in, line); ) {
        rucksacks.push_back(line);
    }
}

void day03(const std::string &filename) {
    std::ifstream in{filename};
    std::vector<std::string> rucksacks;
    day03_parse(in, rucksacks);
    auto a = rucksack_sum(rucksacks);
    auto b = badge_sum(rucksacks);
    std::cout << "Day 02:\n";
    std::cout << "\tPart 1: " << a << std::endl;
    std::cout << "\tPart 2: " << b << std::endl;
}
