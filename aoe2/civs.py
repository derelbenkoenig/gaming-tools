import yaml
import random

def load_civ_list():
    with open("civs.yml") as civ_file:
        civ_list = yaml.safe_load(civ_file)
    return civ_list
