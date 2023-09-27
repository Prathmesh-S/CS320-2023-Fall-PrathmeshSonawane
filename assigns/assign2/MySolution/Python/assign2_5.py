####################################################
import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

#Problem 5

def fnlist_make_fwork(func):
    newList = fnlist_nil()

    def work(node):
        newNode = fnlist_cons(x0, newList)
    work(func)
    newList = fnlist_reverse(newList)
    return(newList)