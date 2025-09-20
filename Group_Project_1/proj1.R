#Naoise Daly s2848034

setwd(r"(C:\Users\Naoise Daly\OneDrive - University of Edinburgh\stat prog\stats_programming_autumn2025\Group_Project_1)")

#-------------Delete later
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
a <- a[4:108]
a <- tolower(a)

unique_words = unique(a);

# this replaces every word in the a, the text, with a number token
# the word is replaced by the location/index of that word in unique_words
a_tokenised = match(a, unique_words)
#----------change example later
#for example, the first 24 words are unique but the 25th is a repetition of
# "might" which already occurred as the 11th word 
# so the 25th entry will have the token "11"
cbind(a, a_tokenised)[11:25,]

#----------change to 1000 later
#the model will use only the K most common words as part of its vocabulary
K_most_common_words = 5

#token/index counts
#tabulate takes in the text tokenised as numbers 
#and spits out the counts for each number, in order of that number
#----------- change example later
#i.e #times "1" appears, #times "2" appears, ...
# the token "16" (representing "the") occurs 6 times in the a_tokenised
#so position 16 of the output is 6
token_counts = tabulate(a_tokenised)

#order tells you what way to rearrange the locations so that 
#the values are in decreasing order - biggest to smallest
#in our case it orders the tokens by their count
#so we can just take the first K of them 
#the k most common tokens can then tell us the k most common words
b = unique_words[
  order(token_counts, decreasing = T)[1:K_most_common_words]
]
head(b);sum(a == b[1]) #the is the most common word

#save space by deleting arrays we're done with
rm(token_counts,a_tokenised, unique_words)







