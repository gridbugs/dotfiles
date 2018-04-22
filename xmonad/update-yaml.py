#!/usr/bin/env python3
import sys
import yaml

def main():
  filename = sys.argv[1]
  with open(filename, "r") as f:
    data = yaml.load(f)
    flags = data.setdefault("flags", {})
    xmobar = flags.setdefault("xmobar", {})
    xmobar['all_extensions'] = True
    extra_deps = data.setdefault("extra-deps", [])
    iwlib = 'iwlib-0.1.0'
    if iwlib not in extra_deps:
      extra_deps.append(iwlib)
  with open(filename, "w") as f:
    f.write(yaml.dump(data))

if __name__ == "__main__":
  main()
