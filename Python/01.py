filename = "day_1_input.txt"
top_three = [0, 0, 0]
current_dwarf = 0
with open(filename) as f:
    for line in f:
        if len(line.strip()) == 0:
            if top_three[0] < current_dwarf:
                top_three[0] = current_dwarf
                top_three.sort()
            current_dwarf = 0
        else:
            current_dwarf += int(line)

print(str(top_three))
print(sum(top_three))