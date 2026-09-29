import json
from pathlib import Path

config_file = "config.json"
# Settings specific to a machine, untracked, overrides config_file
config_file_local = "config.local.json"

def mergeConfig(x, y):
    """Merges y into x, dicts are merged recursively, other values replaced"""
    for k, v in y.items():
        x[k] = mergeConfig(x[k], v) if isinstance(x.get(k), dict) and isinstance(v, dict) else v
    return x

def getConfig():
    folder = Path(__file__).parents[0]
    with open(folder/config_file) as json_file:
        data = json.load(json_file)
    if (folder/config_file_local).exists():
        with open(folder/config_file_local) as json_file:
            data = mergeConfig(data, json.load(json_file))
    return data
