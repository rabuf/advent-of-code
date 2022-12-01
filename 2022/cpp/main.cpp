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
    std::cout << elves[0] << std::endl;
    std::cout << std::accumulate(elves.begin(), elves.begin()+3, 0) << std::endl;
}

int main() {
    day01("../../input/01.txt");
    return 0;
}
