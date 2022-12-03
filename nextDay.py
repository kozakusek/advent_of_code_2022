import os
import shutil
import subprocess

EXT = ".scala"
YEAR = "2022"
URL = "https://adventofcode.com"

def is_day(name: str) -> bool:
    return name.startswith("day-")

def get_day_number(name: str) -> int:
    try:
        return int(name.split("-")[1])
    except IndexError:
        return 1

day = 1
for file in os.listdir():
    if os.path.isdir(file) and is_day(file):
       day = max(day, get_day_number(file) + 1)

dirname = f"day-{day}"

subprocess.run(["sbt", "new", "scala/scala3.g8"], input=dirname.encode(), capture_output=True)

p = "/src/main/scala"

open(dirname + p + "/First" + EXT, "x")
open(dirname + p + "/Second" + EXT, "x")
os.remove(dirname + p + "/Main.scala")
os.remove(dirname + "/README.md")
shutil.rmtree(dirname + "/src/test/")
with open(dirname + "/README.md", "x") as f:
    f.write(f"# Day-{day}  \n")
    f.write(f"## [Link]({URL}/{YEAR}/day/{day})  \n")

open(dirname + "/input.txt", "x")
