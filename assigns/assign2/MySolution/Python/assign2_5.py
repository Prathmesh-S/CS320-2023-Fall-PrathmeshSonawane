####################################################
import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

#Problem 5

def fnlist_make_fwork(func):
    newList = fnlist_nil()

    def work(node):
        nonlocal newNode 
        newNode =  fnlist_cons(x0, newList)
    func(work)
    newNode= fnlist_reverse(newNode)
    return(newList)