#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <regex>

#include "day04.h"

typedef std::pair<int, int> range_t;
typedef std::pair<range_t, range_t> range_pair_t;

bool contains (range_pair_t pair) {
    auto [left, right] = pair;
    auto [a, b] = left;
    auto [c, d] = right;
    return (a <= c && d <= b) || (c <= a && b <= d);
}

bool overlaps (range_pair_t pair) {
    auto [left, right] = pair;
    auto [a, b] = left;
    auto [c, d] = right;
    return a <= d && c <= b;
}

// Verbose, but I was curious to try out the regex library.
void day04_parse(std::istream &in, std::vector<range_pair_t> &ranges) {
    const std::regex re{"\\d+"};
    for(std::string line; std::getline(in, line); ) {
        auto pos =
                std::sregex_iterator(line.begin(), line.end(), re);
        auto a = std::stoi(pos->str()); pos++;
        auto b = std::stoi(pos->str()); pos++;
        auto c = std::stoi(pos->str()); pos++;
        auto d = std::stoi(pos->str());
        auto left = std::make_pair(a,b);
        auto right = std::make_pair(c, d);
        ranges.push_back(std::make_pair(left, right));
    }
}

void day04(const std::string &filename) {
    std::ifstream in{filename};
    std::vector<range_pair_t> ranges;
    day04_parse(in, ranges);
    auto a = std::count_if(ranges.begin(), ranges.end(), contains);
    auto b = std::count_if(ranges.begin(), ranges.end(), overlaps);;
    std::cout << "Day 04:\n";
    std::cout << "\tPart 1: " << a << std::endl;
    std::cout << "\tPart 2: " << b << std::endl;
}
