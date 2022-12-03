import importlib


def main():
    modules = [f"day{day:02}" for day in range(1, 26)]
    for module in modules:
        try:
            m = importlib.import_module(module)
            m.main()
        except ModuleNotFoundError:
            pass


if __name__ == "__main__":
    main()
