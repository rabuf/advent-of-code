#include <vector>
#include <unordered_map>
#include <fstream>
#include <numeric>
#include <iostream>

#include "day02.h"

std::unordered_map<rps, rps> beats = {
        {rps::rock,     rps::scissors},
        {rps::paper,    rps::rock},
        {rps::scissors, rps::paper},
};

int score(rps left, rps right) {
    return std::to_underlying(right) + 3 * ((left == right) + 2 * (left == beats[right]));
}

std::pair<int, int> day02_solve(std::vector<std::pair<rps, std::pair<rps, rps>>> input) {
    auto result = std::accumulate(input.begin(), input.end(), std::make_pair(0, 0),
                                  [](auto acc, auto play) {
                                      auto [them, me] = play;
                                      auto [first, second] = me;
                                      return std::make_pair(acc.first + score(them, first),
                                                            acc.second + score(them, second));
                                  });
    return result;
}

void day02_parse(std::istream &in, std::vector<std::pair<rps, std::pair<rps, rps>>> &result) {
    for (std::string line; std::getline(in, line);) {
        rps left{(line[0] - 'A') + 1};
        std::pair<rps, rps> right;
        right.first = rps{line[2] - 'X' + 1};
        switch (right.first) {
            case rps::rock:
                right.second = beats[left];
                break;
            case rps::paper:
                right.second = left;
                break;
            case rps::scissors:
                right.second = beats[beats[left]];
                break;
        }
        result.emplace_back(std::make_pair(left, right));
    }
}

void day02(const std::string &filename) {
    std::ifstream in{filename};
    std::vector<std::pair<rps, std::pair<rps, rps>>> plays;
    day02_parse(in, plays);
    auto [a, b] = day02_solve(plays);
    std::cout << "Day 02:\n";
    std::cout << "\tPart 1: " << a << std::endl;
    std::cout << "\tPart 2: " << b << std::endl;
}
