#!/usr/bin/env python3
"""Embed a data file into an object file using objcopy.

Usage: embed_objcopy.py <cc> <objcopy> <input> <output> <symbol>
"""
import subprocess, sys

cc, objcopy, input_file, output, symbol = sys.argv[1:6]

# Make an empty object file (in the right format for our build target)
subprocess.run([cc, '-x', 'c', '-c', '-o', output, '/dev/null'], check=True)

# Read input file and append null terminator
with open(input_file, 'rb') as f:
    data = f.read() + b'\0'

# Embed the script into a new section
subprocess.run([
    objcopy,
    '--add-section', '.embed=/dev/stdin',
    '--set-section-flags', '.embed=data,alloc,load',
    '--add-symbol', f'{symbol}=.embed:0,global,object',
    output,
], input=data, check=True)
