#include <iostream>
#include <sysexits.h>
#include "day01.h"
#include "day02.h"

int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "aoc2022: Must supply input file directory." << std::endl;
        std::exit(EX_USAGE);
    }
    const std::string input_dir{argv[1]};
    day01(input_dir + "/01.txt");
    day02(input_dir + "/02.txt");

    return 0;
}
