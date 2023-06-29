filename = "day_2_input.txt"
total_score_1 = 0
total_score_2 = 0

def round_score_1(line):
    opp, self = line.split()
    shape_score = ord(self) - ord("X") + 1
    winner_score = ((ord(self) - ord(opp)) % 3 == 0) * 6 + ((ord(self) - ord(opp)) % 3 == 2) * 3
    return shape_score + winner_score

def round_score_2(line):
    opp, result = line.split()
    shape_score = ((ord(opp) + ord(result) - 128) % 3 == 0) * 2 + ((ord(opp) + ord(result) - 128) % 3 == 1) * 3 + ((ord(opp) + ord(result) - 128) % 3 == 2) * 1
    winner_score = (ord(result) - ord("X")) * 3
    return shape_score + winner_score

with open(filename) as f:
    for line in f:
        if not len(line.strip()) == 0:
            total_score_1 += round_score_1(line)
            total_score_2 += round_score_2(line)

print(total_score_1, total_score_2)