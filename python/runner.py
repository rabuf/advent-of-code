import argparse

from aoc_util import load_and_run


def main():
    parser = argparse.ArgumentParser(prog="aoc_runner")
    parser.add_argument("input_directory")
    parser.add_argument(
        "-y", "--year", type=int, required=True, choices=range(2015, 2026)
    )

    parser.add_argument("-d", "--day", type=int, required=True, choices=range(1, 26))
    args = parser.parse_args()
    print(f"Year {args.year}")
    load_and_run(args.year, args.day)


if __name__ == "__main__":
    main()
