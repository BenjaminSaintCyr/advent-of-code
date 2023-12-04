import re
import sys

digits = re.compile(r'(?=(nine|eight|seven|six|five|four|three|two|one|\d))')

digit_to_int = {
    "nine": 9,
    "eight": 8,
    "seven": 7,
    "six": 6,
    "five": 5,
    "four": 4,
    "three": 3,
    "two": 2,
    "one": 1,
}

def parse_digit(num):
    try:
        return int(num)
    except ValueError:
        return digit_to_int[num]

# file = open('d1_2.txt')
def solution(file_name):
    total = 0
    with open(file_name) as file:
        for line in file:
            nums = digits.findall(line)
            first = parse_digit(nums[0])
            last = parse_digit(nums[-1])
            value = first * 10 + last
            total += value
    return total

if __name__ == "__main__":
    print(solution(sys.argv[1]))
