#!/usr/bin/env python3
import time
import sys

for i in range(100):
    sys.stdout.write(f"\r  progress: {i + 1}/100")
    sys.stdout.flush()
    time.sleep(0.1)

sys.stdout.write("\n")
