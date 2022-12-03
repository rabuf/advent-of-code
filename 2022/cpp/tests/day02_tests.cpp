#include "gtest/gtest.h"
#include "day02.h"

class Day02Sample : public ::testing::Test {
protected:
    void SetUp() override {
        std::istringstream iss{sample_input};
        day02_parse(iss, plays);
    }

    // void TearDown() override {}

    std::vector<std::pair<rps, std::pair<rps, rps>>> plays;
private:
    std::string sample_input =
            "A Y\n"
            "B X\n"
            "C Z\n";
};

TEST_F(Day02Sample, Day02Works) {
    auto [a,b] = day02_solve(plays);
    EXPECT_EQ(a, 15);
    EXPECT_EQ(b, 12);
}

TEST(ScoringTest, IndividualMatches) {
    EXPECT_EQ(score(rps::rock, rps::rock), 4);
    EXPECT_EQ(score(rps::rock, rps::paper), 8);
    EXPECT_EQ(score(rps::rock, rps::scissors), 3);
    EXPECT_EQ(score(rps::paper, rps::rock), 1);
    EXPECT_EQ(score(rps::paper, rps::paper), 5);
    EXPECT_EQ(score(rps::paper, rps::scissors), 9);
    EXPECT_EQ(score(rps::scissors, rps::rock), 7);
    EXPECT_EQ(score(rps::scissors, rps::paper), 2);
    EXPECT_EQ(score(rps::scissors, rps::scissors), 6);
}
