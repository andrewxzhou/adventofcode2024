import functools
f = open("sample.txt", "r")
words = f.readline().rstrip().split(", ")
sentenceso = f.readlines()
sentences = [line.rstrip() for line in sentenceso]
def isWord(S):
  s = len(S)
  @functools.cache
  def match_sentence(i, j):
    # i is length of prefix word, j is length of overall string left to consider
    if i == 0 and j == 0:
      return 1 
    if i > j:
      return 0 
    
    if S[s - j:s-j+i] in words:
      return match_sentence(0, j - i) + match_sentence(i + 1, j)
    else:
      return match_sentence(i + 1, j)
  return match_sentence(0, s)
count = 0
for i in sentences:
  
  count += isWord(i)
print(count)
