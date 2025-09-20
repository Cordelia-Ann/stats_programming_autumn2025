#Naoise Daly s2848034

setwd(r"(C:\Users\Naoise Daly\OneDrive - University of Edinburgh\stat prog\stats_programming_autumn2025\Group_Project_1)")

#-------------Delete later
a <- tolower(scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8"))
a <- a[4:108]

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
K_most_common_words = 20

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
head(b);sum(a == b[1]) #'the' is the most common word

#save space by deleting arrays we're done with
rm(token_counts,a_tokenised, unique_words)









#so we are tokenising the text (for a second time) based on the vector b
# each word in the text will be represented by a number token or NA
#if the word is not in b, not one of the K most common words,
# it is represented as NA
#if the word is in b, it is represented by its location in b
a_tokenised = match(a, b)


#-----------  delete later
a_tokenised = strsplit(
  "A cat sat on the mat last night", " "
)[[1]]

mlag = 3

n = length(a_tokenised) #total number of words
# I thought it was easier to manipulate M as an array first
# remember that the length of each of column is n-mlag
col_length = n-mlag
M = array(NA, col_length*(mlag+1) )



#you don't like loops but this loop has only mlag iterations
#and each iteration has constant time operations

#M can be viewed as sliding a window of length col_length
#over the vector a_tokenised
#the first column of M is the first col_length elements of a_tokenised
#the second column is the next col_length elements starting at the 2nd element
#the ith column is the next col_length elements starting at the ith element


for (i in 0:mlag){
  #the first col_length elements in M are column 1
  #the elements in position col_length+1  to 2*col_length are column 2
  column_i_indices = (i*(col_length) +1) : ((i+1)*col_length)
  #now that we know where column i is in M,
  #we insert the next tokens in the window
  M[ column_i_indices  ]  = a_tokenised[i + 1:col_length ]
}

#now just turn M from an array into a matrix
M <- matrix(M, n-mlag, mlag+1)






