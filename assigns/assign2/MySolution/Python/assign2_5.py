####################################################
import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

#Problem 5

def fnlist_make_fwork(func):
    newList = fnlist_nil()

    def work(x0):
        nonlocal newList 
        newList =  fnlist_cons(1, newList, x0)
    func(work)
    newList= fnlist_reverse(newList)
    return(newList)