#!/bin/python3

import sys

file = sys.argv[1]
out = f"""\
// generated by get_urclos_fs.py for {file}
#pragma once
#include <stdint.h>
uint16_t fs[] = {{"""

with open(file, "rb") as f:
    while (b := f.read(2)):
        out += "0x%02x%02x," % (b[0], b[1])

out += "};"

with open("urclos_fs.h", "w") as f:
    f.write(out)