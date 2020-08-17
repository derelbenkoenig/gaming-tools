from math import ceil

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
