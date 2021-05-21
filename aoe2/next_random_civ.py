#!/usr/bin/env python3
import yaml
import sys

order_filename = sys.argv[1]

with open(order_filename) as infile:
    current_state = yaml.safe_load(infile)

curr_idx = current_state['curr_idx']
next_idx = curr_idx + 1
civ_list = current_state['civ_list']
next_civ = civ_list[next_idx]

current_state['curr_idx'] = next_idx

with open(order_filename, 'w') as outfile:
    yaml.safe_dump(current_state, outfile)

print("Next civ is %s, which is index %d of the list of %d" %
        ( next_civ
        , next_idx
        , len(civ_list)))
