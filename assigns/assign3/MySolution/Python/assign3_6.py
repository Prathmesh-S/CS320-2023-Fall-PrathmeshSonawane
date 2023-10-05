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

def mylist_print(theList):
    if (theList.get_id_tag()==0):
        print("[]")
    else:
        print("[", end="")
        mylist_print_entire_list(theList)
        print("]")

def mylist_reverse(theList):
    if (theList.get_id_tag()==0):
        return mylist_nil()
    else:
        return mylist_snoc(mylist_reverse(theList.get_itm2()), theList.get_itm1())


def mylist_snoc(theList, itm):
    if (theList.get_id_tag()==0):
        return mylist_sing(itm)
    else:
        return mylist_cons(theList.get_itm1(), mylist_snoc(theList.get_itm2(), itm))

def mylist_print_entire_list(theList):
    if (theList.get_id_tag()==0):
        return None
    elif (theList.get_itm2().get_id_tag()==0):
        print(theList.get_itm1(), end="")
    else:
        print(theList.get_itm1(), end=", ")
        mylist_print_entire_list(theList.get_itm2())

def mylist_append2(theList1, theList2):
    if (theList1.get_id_tag()==0):
        return theList2
    else:
        return mylist_cons(theList1.get_itm1(), mylist_append2(theList1.get_itm2(), theList2))
    
# Work Functions 

def mylist_foreach(theList, work_func):
    if (theList.get_id_tag()==0):
        return None
    else:
        work_func(theList.get_itm1())
        mylist_foreach(theList.get_itm2(), work_func)

def mylist_rforeach(theList, work_func):
    if (theList.get_id_tag()==0):
        return None
    else:
        mylist_rforeach(theList.get_itm2(), work_func)
        work_func(theList.get_itm1())