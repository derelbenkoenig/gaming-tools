#!/usr/bin/env python3
import yaml
import random

with open("civs.yml") as civ_file:
    civ_list = yaml.safe_load(civ_file)

chosen_civ = random.choice(civ_list)

print("civ is %s\n" % chosen_civ)

