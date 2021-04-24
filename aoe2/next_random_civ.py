#!/usr/bin/env python3
import yaml
import sys

order_filename = sys.argv[1]

with open(order_filename) as infile:
    current_state = yaml.safe_load(infile)

curr_idx = current_state['next_idx']
civ_list = current_state['civ_list']
next_civ = civ_list[curr_idx]

current_state['next_idx'] = curr_idx + 1

with open(order_filename, 'w') as outfile:
    yaml.safe_dump(current_state, outfile)

print("Next civ is %s, which is index %d of the list of %d" %
        ( next_civ
        , curr_idx
        , len(civ_list)))
