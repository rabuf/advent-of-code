#include <iostream>
#include <string>
#include <fstream>
#include <numeric>
#include <vector>
#include <map>



int main(int argc, char** argv) {
    std::ifstream ifs{"../input/2024/01.txt"};
    std::vector<int64_t> left;
    std::vector<int64_t> right;
    while (ifs) {
        int64_t l, r;
        if (ifs >> l >> r) {
            left.push_back(l);
            right.push_back(r);
        }
    }
    std::ranges::sort(left);
    std::ranges::sort(right);
    const auto p1 = std::transform_reduce(left.begin(), left.end(), right.begin(),
        0, std::plus{}, [](const int64_t a, const int64_t b) { return std::abs(a - b); });
    std::map<int64_t, int64_t> left_count, right_count;
    for (const auto l: left) {
        left_count[l]++;
    }
    for (const auto r: right) {
        right_count[r]++;
    }
    std::cout << "Part 1: " << p1 << std::endl;
    int64_t p2 = 0;
    for (auto [k, v]: left_count) {
        p2 += v * k * (right_count.contains(k) ? right_count[k] : 0);
    }
    std::cout << "Part 2: " << p2 << std::endl;
}