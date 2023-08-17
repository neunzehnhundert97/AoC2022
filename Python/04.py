filename = "day_4_input.txt"
N_containeed_pairs = 0
N_overlapped = 0

with open(filename) as f:
    for line in f:
        elf_1,elf_2 = line.split(",")
        e1_start, e1_end = elf_1.split("-")
        e2_start, e2_end = elf_2.split("-")
        
        e11, e12 = (int(e1_start), int(e1_end))
        e21, e22 = (int(e2_start), int(e2_end))
        
        if (e11 <= e21 and e12 >= e22) or (e21 <= e11 and e22 >= e12):
            N_containeed_pairs += 1
            N_overlapped += 1
        
        elif (e11 <= e21 and e21 <= e12) or (e11 <= e22 and e22 <= e12):
            N_overlapped += 1
        
        

print(str((N_containeed_pairs, N_overlapped)))