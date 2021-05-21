#!/usr/bin/env python3
import yaml
import sys

order_filename = sys.argv[1]

with open(order_filename) as infile:
    current_state = yaml.safe_load(infile)

curr_idx = current_state['curr_idx']
civ_list = current_state['civ_list']
curr_civ = civ_list[curr_idx]


print("Current civ is %s, which is index %d of the list of %d" %
        ( curr_civ
        , curr_idx
        , len(civ_list)))
