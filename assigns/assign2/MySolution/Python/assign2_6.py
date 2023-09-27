#Problem 6

def string_merge(x,y):
    len1 = len(x)
    len2 = len(y)
    
    return(merge(x, y, len1, len2))



def merge(x,y,len1,len2):
    
    if (len1 <1):
        return(y)
    elif (len2<1):
        return (x)
    elif (x[0] <y[0]):
        return x[0] + merge(x[1:len(x)],y,len1-1,len2)
    else:
        return y[0] + merge(x,y[1:len(y)],len1,len2-1)    
    
    