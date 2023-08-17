import re

filename = "day_5_input.txt"

def find_number_of_stacks(filename):
    with open(filename) as f:
        while True:
            line = f.readline()
            if line[1] != "1":
                continue
            n_stacks = line[-3]
            return int(n_stacks)

N_stacks = find_number_of_stacks(filename)


def infer_stacks(filename, N_stacks):
    with open(filename) as f:
        STACKS = [[] for _ in range(N_stacks)]
        
        while True:
            line = f.readline()
            if not "[" in line:
                return [list(reversed(stack)) for stack in STACKS]
            
            for i in range(N_stacks):
                if line[i*4 + 1] != " ":
                    STACKS[i].append(line[i*4 + 1])


STACKS = infer_stacks(filename, N_stacks)


def move_one_from_i_to_j(i, j, STACKS):
    item = STACKS[i - 1].pop()  # because lists start with 0
    STACKS[j - 1].append(item)


def move_n_from_i_to_j(i, j, n, STACKS):
    items = STACKS[i - 1][-n:]
    STACKS[i - 1] = STACKS[i - 1][:-n]
    STACKS[j - 1] += items


def process_line_command(line):
    extract_integers = re.findall(r'\d+', line)
    return extract_integers


with open(filename) as f:
    for line in f:
        if line[0] != "m":
            continue
        command = process_line_command(line)
        N_items = int(command[0])
        N_from = int(command[1])
        N_to = int(command[2])
        for _ in range(N_items):
            move_one_from_i_to_j(N_from, N_to, STACKS)

answer9000 = ""

for stack in STACKS:
    answer9000 += stack[-1]

print(answer9000)



STACKS = infer_stacks(filename, N_stacks)

with open(filename) as f:
    for line in f:
        if line[0] != "m":
            continue
        command = process_line_command(line)
        N_items = int(command[0])
        N_from = int(command[1])
        N_to = int(command[2])
        move_n_from_i_to_j(N_from, N_to, N_items, STACKS)

answer9001 = ""

for stack in STACKS:
    answer9001 += stack[-1]

print(answer9001)