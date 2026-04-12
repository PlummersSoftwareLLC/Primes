import sys

def read_memdump(argv, default_fn="fullmem_dump.bin"):
    if len(argv) == 1:
        fn = default_fn
    elif len(argv) == 2:
        fn = argv[1]
    else:
        print(f"Usage: {argv[0]} [memdump_file]", file=sys.stderr)
        sys.exit(2)

    with open(fn, "rb") as f:
        data = f.read()
        return data[2:] # First 2 bytes are the loading address