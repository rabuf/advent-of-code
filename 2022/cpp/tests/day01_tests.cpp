#include "gtest/gtest.h"
#include "day01.h"

class Day01Sample : public ::testing::Test {
protected:
    void SetUp() override {
        std::istringstream iss{sample_input};
        day01_parse(iss, elves);
    }

    // void TearDown() override {}

    std::vector<int> elves;
private:
    std::string sample_input =
            "1000\n"
            "2000\n"
            "3000\n"
            "\n"
            "4000\n"
            "\n"
            "5000\n"
            "6000\n"
            "\n"
            "7000\n"
            "8000\n"
            "9000\n"
            "\n"
            "10000";

};

TEST_F(Day01Sample, Day01Works) {
    auto [a,b] = day01_solve(elves);
    EXPECT_EQ(a, 24000);
    EXPECT_EQ(b, 45000);
}

TEST(Day01Input, EmptyInput) {
    std::istringstream in{""};
    std::vector<int> elves;
    day01_parse(in, elves);
    EXPECT_EQ(std::vector<int>{}, elves);
}

TEST(Day01Input, OneElf) {
    std::vector<std::tuple<std::string, std::vector<int>, int, int>> const inputs =
            {{"10", {10}, 10, 10},
             {"10\n20", {30}, 30, 30},
             {"10\n20\n30", {60}, 60, 60}};
    for (auto [file, expected, answer1, answer2] : inputs) {
        std::istringstream in{file};
        std::vector<int> elves;
        day01_parse(in, elves);
        EXPECT_EQ(elves, expected);
        auto [a, b] = day01_solve(elves);
        EXPECT_EQ(a, answer1);
        EXPECT_EQ(b, answer2);
    }
}

TEST(Day01Input, TwoElves) {
    std::vector<std::tuple<std::string, std::vector<int>, int, int>> const inputs =
            {{"10\n\n20", {10, 20}, 20, 30},
             {"10\n20\n\n30", {30, 30}, 30, 60},
             {"10\n20\n30\n\n40", {60, 40}, 60, 100}};
    for (auto [file, expected, answer1, answer2] : inputs) {
        std::istringstream in{file};
        std::vector<int> elves;
        day01_parse(in, elves);
        EXPECT_EQ(elves, expected);
        auto [a, b] = day01_solve(elves);
        EXPECT_EQ(a, answer1);
        EXPECT_EQ(b, answer2);
    }
}