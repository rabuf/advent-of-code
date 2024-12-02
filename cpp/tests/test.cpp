#include <catch2/catch_template_test_macros.hpp>

TEST_CASE("Equal numbers equal", "[equality]") {
    REQUIRE(1==1);
    REQUIRE(1==0);
    REQUIRE(2==2);
}