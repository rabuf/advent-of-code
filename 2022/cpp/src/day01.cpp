#include <vector>
#include <fstream>
#include <numeric>
#include <iostream>

std::pair<int, int> day01_solve(std::vector<int> input) {
    std::nth_element(input.begin(), input.begin() + 2, input.end(), std::greater());
    auto a = std::max(input[0], input[1]);
    auto b = std::accumulate(input.begin(), input.begin() + 3, 0);
    return {a, b};
}

void day01_parse(std::istream &in, std::vector<int> &result) {
    for (std::string line; std::getline(in, line);) {
        int elf = 0;
        while(!line.empty()) {
            elf += std::stoi(line);
            if(!getline(in, line)) {
                break;
            }
        }
        result.push_back(elf);
    }
}

void day01(const std::string &filename) {
    std::ifstream in{filename};
    std::vector<int> elves;
    day01_parse(in, elves);
    auto [a,b] = day01_solve(elves);
    std::cout << "Day 01:\n";
    std::cout << "    Part 1: " << a << std::endl;
    std::cout << "    Part 2: " << b << std::endl;
}
