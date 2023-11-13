
def string_fset_at(cs, i0, c0):
  def tabulate(length):
    return [c0 if i==i0 else cs[i] for i in range(length)]
  return tabulate(len(cs))

# The Alphabet = "abcdefghijklmnopqrstuvwxyz"
Alphabet = [chr(ord('a') + i) for i in range(26)]



#let
# list_of_buddies (word: string): string list =
#   let n0 = string_length(word) in
#   list_make_fwork(
#    fun work -> int1_foreach(n0)(fun i0 -> let c0 = string_get_at(word)(i0) in
#               string_foreach(alphabet)(fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;; 

def list_of_buddies(word):
  #Take string and replace i0 with c0
  def make_fwork(i0, c0):
    return lambda string: string_fset_at(string, i0, c0)
  
  def the_work(i0, c0):
    return [work(word) for c1 in Alphabet if c1 != c0 for work in [make_fwork(i0, c1)]]
  
  # Return the buddy by itterating through the length of word with their index values --> call buddyWork, 
  return [buddy for i0, c0 in enumerate(word) for buddy in the_work(i0, c0)]


#Testing 
#print(list_of_buddies("Computer Science"))

#def test(word):
#  for i0, c0 in enumerate(word):
#    print(c0)
#  return(None)
 
#test("hello")