# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
########################################################################

#Declare the myList Class
#ID tags are used to represent the type of a list 
    # 0 ->null 
    # 1 --> Not Null

class mylist:
    id_tag = -1
    def get_id_tag(self):
        return self.id_tag
    
class mylist_nil(mylist):
    def __init__(self):
        self.id_tag = 0

class mylist_cons(mylist):
    def __init__(self, itm1, itm2):
        self.id_tag = 1
        self.itm1 = itm1
        self.itm2 = itm2
    def get_itm1(self):
        return self.itm1
    def get_itm2(self):
        return self.itm2
    
#Functions associated with myList Class: 
    
def mylist_sing(itm):
    return mylist_cons(itm, mylist_nil())

def mylist_reverse(lst):
    if (lst.get_id_tag()==0):
        return mylist_nil()
    else:
        return mylist_snoc(mylist_reverse(lst.get_itm2()), lst.get_itm1())

def mylist_snoc(lst, itm):
    if (lst.get_id_tag()==0):
        return mylist_sing(itm)
    else:
        return mylist_cons(lst.get_itm1(), mylist_snoc(lst.get_itm2(), itm))

def mylist_print(lst):
    if (lst.get_id_tag()==0):
        print("[]")
    else:
        print("[", end="")
        mylist_print_aux(lst)
        print("]")

def mylist_print_aux(lst):
    if (lst.get_id_tag()==0):
        return None
    elif (lst.get_itm2().get_id_tag()==0):
        print(lst.get_itm1(), end="")
    else:
        print(lst.get_itm1(), end=", ")
        mylist_print_aux(lst.get_itm2())

def mylist_append2(lst1, lst2):
    if (lst1.get_id_tag()==0):
        return lst2
    else:
        return mylist_cons(lst1.get_itm1(), mylist_append2(lst1.get_itm2(), lst2))
    
## Work Functions 

def mylist_foreach(lst, work_func):
    if (lst.get_id_tag()==0):
        return None
    else:
        work_func(lst.get_itm1())
        mylist_foreach(lst.get_itm2(), work_func)

def mylist_rforeach(lst, work_func):
    if (lst.get_id_tag()==0):
        return None
    else:
        mylist_rforeach(lst.get_itm2(), work_func)
        work_func(lst.get_itm1())