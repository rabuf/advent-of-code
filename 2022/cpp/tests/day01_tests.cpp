#include "gtest/gtest.h"
#include "day01.h"

class Day01Test : public ::testing::Test {
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

TEST_F(Day01Test, Day01Works) {
    auto [a,b] = day01_solve(elves);
    EXPECT_EQ(a, 24000);
    EXPECT_EQ(b, 45000);
}