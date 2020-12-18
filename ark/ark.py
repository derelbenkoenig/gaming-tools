from math import ceil
from collections.abc import Mapping

def berries(max_torp, current_torp):
    """
    Given the max torpidity and current torpidity of a
    creature, return the number of berries to fill it to
    full and how many seconds it would take to fill. Does
    not include the time that the torpidity will take to go
    back down
    """
    num_berries = ceil((max_torp - current_torp) / 7.5)
    num_seconds = num_berries * 3
    return {"berries": num_berries, "seconds": num_seconds}

def minsec(seconds):
    """
    converts a number of seconds to a tuple of minutes and
    seconds
    """
    return (int(seconds/60), int((seconds % 60)))

def berries2(max_torp, current_torp):
    """
    like `berries`, but return the time in
    (minutes, seconds) rather than just seconds
    """
    res = berries(max_torp, current_torp)
    time = minsec(res['seconds'])
    return {"berries": ceil(res["berries"]), 'time': time}

def encumbered(weight):
    return weight * .85

class Recipe(Mapping):
    def __init__(self, **kwargs):
        self.components = dict(kwargs)

    def __mul__(self, other):
        result = {}
        for k, v in self.components.items():
            result[k] = v * other
        return Recipe(**result)

    def __add__(self, other):
        result = self.components.copy()
        for k, v in other.items():
            result[k] = v + result.get(k, 0)
        return Recipe(**result)

    def __sub__(self, other):
        result = self.components.copy()
        for k, v in other.items():
            result[k] = result.get(k, 0) - v
        return Recipe(**result)

    def __getitem__(self, key):
        return self.components.__getitem__(key)

    def keys(self):
        return self.components.keys()

    def __iter__(self):
        return self.components.__iter__()

    def __len__(self):
        return self.components.__len__()

    def __repr__(self):
        return 'Recipe(' + ", ".join(["%s=%d" % item for item in self.components.items()]) + ")"
