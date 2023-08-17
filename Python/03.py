filename = "day_3_input.txt"

sum_priorities = 0

with open(filename) as f:
    for line in f:
        first_half = set(line[0:len(line)//2])
        second_half = set(line[len(line)//2:])
        intersection = first_half.intersection(second_half)
        repeated_item = "a"
        for item in intersection:
            repeated_item = item
        priority = ord(repeated_item) - ord("a") + 1 if ord(repeated_item) >= ord("a") else ord(repeated_item) - ord("A") + 1 + 26
        sum_priorities += priority

print(sum_priorities)



sum_priorities = 0

with open(filename) as f:
    while True:
        elf_1 = f.readline()
        if not elf_1:
            break
        elf_2 = f.readline()
        elf_3 = f.readline()
        intersection = set(elf_1).intersection(set(elf_2), set(elf_3))
        repeated_item = "a"
        for item in intersection:
            repeated_item = item
        priority = ord(repeated_item) - ord("a") + 1 if ord(repeated_item) >= ord("a") else ord(repeated_item) - ord("A") + 1 + 26
        sum_priorities += priority

print(sum_priorities)