#!/usr/bin/env python3
import civs
import random
import yaml
from sys import stdout

civ_list = civs.load_civ_list()

random.shuffle(civ_list)

random_order = {'civ_list': civ_list, 'curr_idx': 0}

yaml.safe_dump(random_order, stdout)
