#!/usr/bin/env python3
import civs
import random

civ_list = civs.load_civ_list()

chosen_civ = random.choice(civ_list)

print("civ is %s\n" % chosen_civ)

