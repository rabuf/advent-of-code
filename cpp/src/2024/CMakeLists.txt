set(YEAR 2024)

foreach(DAY 01)
    add_executable(aoc_${YEAR}_day${DAY} day${DAY}.cpp)
endforeach ()