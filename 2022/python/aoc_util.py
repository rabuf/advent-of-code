def chunk(lst, size):
    args = [iter(lst)] * size
    return zip(*args)
