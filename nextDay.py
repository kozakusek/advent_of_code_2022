import os

EXT = ".py"
YEAR = "2022"
URL = "https://adventofcode.com"

def is_day(name: str) -> bool:
    return name.startswith("Day-")

def get_day_number(name: str) -> int:
    try:
        return int(name.split("-")[1])
    except IndexError:
        return 1

day = 1
for file in os.listdir():
    if os.path.isdir(file) and is_day(file):
       day = max(day, get_day_number(file) + 1)

dirname = f"Day-{day}"

os.mkdir(dirname)
open(dirname + "/first" + EXT, "x")
open(dirname + "/second" + EXT, "x")
with open(dirname + "/README.md", "x") as f:
    f.write(f"# Day-{day}  \n")
    f.write(f"## [Link]({URL}/{YEAR}/day/{day})  \n")