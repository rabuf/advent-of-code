#include <iostream>
#include <numeric>
#include <fstream>
#include <vector>

void day01(const std::string &filename) {
    std::ifstream in{filename};
    std::vector<int> elves{0};
    for (std::string line; std::getline(in, line); ) {
        if (!line.empty()) {
            elves.back() += std::stoi(line);
        } else {
            elves.emplace_back(0);
        }
    }
    std::nth_element(elves.begin(), elves.begin()+2, elves.end(), std::greater{});
    std::cout << "Day 01:\n";
    std::cout << "    Part 1: " << std::max(elves[0], elves[1]) << std::endl;
    std::cout << "    Part 2: " << std::accumulate(elves.begin(), elves.begin()+3, 0) << std::endl;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "aoc2022: Must supply input file directory." << std::endl;
        std::exit(64);
    }
    std::string input_dir{argv[1]};
    day01(input_dir+ "/01.txt");
    return 0;
}
