from nltk.corpus import wordnet as wn
from nltk.corpus import wordnet_ic
import sys

brown_ic = wordnet_ic.ic('ic-brown.dat')

reversalInd = ["over", "back", "up", "flipping", "flipped", "reversed",
               "reversal", "in reverse", "reversing", "backward", "backwards",
               "back", "backed", "after recovery", "in retirement",
               "recovering", "returned", "returns", "around", "going around",
               "after going around", "raised", "raising", "reflected",
               "reflecting", "rejected", "revolve", "for revolution",
               "revolution", "on reflection", "roundabout", "round about",
               "upset", "upsets", "thrown over", "over", "springs back",
               "about", "turn", "turns", "turned", "riser"]
removeMiddleInd = ["empty", "emptied", "empties", "heartless", "disheartened",
                   "gutted", "gutless", "void", "voided", "sides of", "ends",
                   "ends of", "extremely", "extremes", "extremes of",
                   "peripherals", "emptied", "heartless", "disheartened",
                   "gutted", "dropping guts", "extremes", "unoccupied",
                   "without", "without guts"]

class Node:
    def __init__(self, word):
        self.word = word
        self.synsets = wn.synsets(word, pos = 'nv')

    def related_to(self, other_word):
        other_synsets = wn.synsets(other_word, pos = 'nv')
        def r(s):
            return list(map(lambda o: s.lin_similarity(o, brown_ic) if s.pos() == o.pos() else 0, other_synsets))
        rs = sum(list(map(r, self.synsets)), [])
        total = len(rs)
        half = total // 2
        success = len(list(filter(lambda x: x > 0.7, rs)))
        return any(map(lambda x: x > 0.9, rs)) or success > half

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return "{}".format(self.word)

class IndGraph:
    def __init__(self):
        self.nodes = []

    def add_word(self, word):
        self.nodes.append(Node(word))

    def related_to(self, word):
        for node in self.nodes:
            if node.related_to(word):
                return True
        return False

    def __str__(self):
        return "{}".format(self.nodes)

if __name__ == "__main__":
    graph = IndGraph()
    for ind in removeMiddleInd:
        graph.add_word(ind)

    print(graph.related_to(sys.argv[1]))
