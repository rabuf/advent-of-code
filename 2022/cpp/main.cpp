#include <iostream>
#include <numeric>
#include <fstream>
#include <vector>

void day01(std::string filename) {
    std::ifstream in{filename};
    std::vector<int> elves{0};
    for (std::string line; std::getline(in, line); ) {
        if (line != "") {
            elves.back() += std::stoi(line);
        } else {
            elves.emplace_back(0);
        }
    }
    std::nth_element(elves.begin(), elves.begin()+2, elves.end(), std::greater{});
    std::cout << "Day 01:\n";
    std::cout << "    Part 1: " << elves[0] << std::endl;
    std::cout << "    Part 2: " << std::accumulate(elves.begin(), elves.begin()+3, 0) << std::endl;
}

int main(int argc, char** argv) {
    std::string input_dir{argv[1]};
    day01(input_dir+ "/01.txt");
    return 0;
}
