
import json

def disable_warnings():
    import sys

    if not sys.warnoptions:
        import warnings
        warnings.simplefilter("ignore")

def printpath():
    import sys
    return (str(sys.path))
