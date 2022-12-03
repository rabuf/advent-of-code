#ifndef AOC2022_DAY02_H
#define AOC2022_DAY02_H

enum class rps {
    rock = 1,
    paper,
    scissors,
};

void day02(const std::string &filename);

int score(rps left, rps right);
std::pair<int, int> day02_solve(std::vector<std::pair<rps, std::pair<rps, rps>>> input);
void day02_parse(std::istream &in, std::vector<std::pair<rps, std::pair<rps, rps>>> &result);

#endif //AOC2022_DAY02_H
