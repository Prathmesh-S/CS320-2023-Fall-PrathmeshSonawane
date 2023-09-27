#Problem 6

def string_merge(x,y):
    len1 = string_len(x)
    len2 = string_len(y)
    
    return(merge(x, y, len1, len2))



def merge(x,y,len1,len2):
    
    if (len1 <1):
        return(y)
    elif (len2<1):
        return (x)
    elif (x[0] <y[0]):
        return x[0] + merge(string_get_at(x,1,string_len(x)),y,len1-1,len2)
    else:
        return y[0] + merge(x,string_get_at(y,1,string_len(y)),len1,len2-1)    
    

def string_len(x):
    return len(x)

def string_get_at(x, start, finish):
    return x[start:finish]

print(string_merge("135", "2468"))