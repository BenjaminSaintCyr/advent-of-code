import re
pattern = re.compile("pattern")
sum = 0
with open('d1.txt') as file:
    for line in file:
        nums = re.findall("[0-9]", line)
        sum += int(nums[0]) * 10 + int(nums[-1])
    print(sum)
