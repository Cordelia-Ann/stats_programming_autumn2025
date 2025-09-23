#Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199
#Naoise built the functionality to cover #5-6. Todd built the data cleaning and filtering to cover #4. Cordelia built the functionality to implement 7-9.
setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_1\\stats_programming_autumn2025\\Group_Project_1") ## comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")

open.bracket <- grep("[", a, fixed=TRUE) #this finds all the locations in a where there is an open bracket
close.bracket <- grep("]", a, fixed=TRUE) #this finds all the locations in a where there is an close bracket

stage.directions <- c() #creating an empty vector to put the stage directions into
stage.index <-list() #i need to store the actual stage direction position values somewhere
counter <- 1 #this creates an index to reference below in the while loop to cycle through all values of open.bracket

while(counter <=length(open.bracket)) {     #i believe i have an error when the bracket indexes don't match 
                                            #(i.e. I pass over) a hanging bracket without a pair. I need to get the two to match always or pass over the hanging bracket. There are 3 open brackets than close.
  if (is.na(close.bracket[counter])) {      #this didnt return an error so I assume it got the extra 3 taken care of    
    bracket.balance <- open.bracket[counter]
  } else {
    bracket.balance <- close.bracket[counter]
  }
  
  if (open.bracket[counter]+100 >= bracket.balance) {        #this checks if the location of the close bracket is within 100 words of the open bracket
    stage.index[[length(stage.index) + 1]] <-open.bracket[counter]:bracket.balance #if it is, add the positions from the start to the end to our stage.index list
  } else { #can we remove the else from this?
    #do nothing
  }
  counter <- counter+1 #increase our index to step through the remaining words
}

stage.index.vector <- unlist(stage.index) #I need to use stage.index as a vector to delete the words at the included positions from our list of words
a.no.stage <- a[-stage.index.vector] #there are still 3 open brackets left in the text, should I remove these alone when I remove the punctuation, or fix?

stage.names <-c() #i need to store the actual stage names in a vector
name.counter <- 1 #this creates an index to reference below in the while loop to cycle through all values of a.no.stage
avoid.words <- c("a","i","A","I") # i need to avoid these words in their comparison to an all uppercase version of the word

while(name.counter <=length(a.no.stage)) {  #I need to step through the words in a.no.stage to check for stage names
  if (a.no.stage[name.counter] %in% avoid.words){ # this checks if the word is "i" or "a" and skips it
      name.counter <- name.counter +1
  next
    } 

  if (a.no.stage[name.counter] == toupper(a.no.stage[name.counter])) {        #this checks if the word (not "i" or "a" is equal to its fully uppercase value)
    stage.names[[length(stage.names) + 1]] <- name.counter # it then adds the position of this word to our stage.names list (which is a list for some reason and not a vector)
  } else { #can we remove the else from this?
    #do nothing
  }
  name.counter <- name.counter+1 #increase our index to step through all of our remaining words
}

stage.names <- unlist(stage.names) #unlist the stage.names into a vector of positions (still not sure why it starts as a list not vector)
a.no.names <- a.no.stage[-stage.names] #remove the all caps words from our list of words
a.no.underscore <- gsub("_", "", a.no.names, fixed=TRUE)

punctuation.vec <- c(",", ".", ";", "!", ":", "?") #this is a vector of all punctuation to check for in a.no.underscore

split_punct <- function(wordlist, punctuations) { #creating function to split punctuation 
  punct.counter <- 1 #create a counter to step through output list
  output <- list() #create a list to store the output of the function
  collapsed.punct <- paste0("[", paste(punctuations, collapse = ""), "]") #collapse the punctuation vector into a single string
  
  for (current.word in wordlist) { #for loop to run through the entire loop of the input wordlist 1 by 1
    last.char <- substr(current.word, nchar(current.word), nchar(current.word)) #store the last character of the list (we want to check if this is a punctuation mark)
    
      if (grepl(collapsed.punct, last.char)==TRUE){ #if the word has a punctuation
        no.punct.word <- substr(current.word, 1, nchar(current.word)-1) #make a variable for the word without the punctuation on the end
        output[[punct.counter]] <- no.punct.word #put the word without punctuation into the list
        output[[punct.counter +1]] <- last.char #put the punctuation mark into the list after the word
        punct.counter <- punct.counter + 2 #increase the counter because we increased the length of the output list by 2
        } else {
        output[[punct.counter]] <- current.word #there was no punctuation found, so just put the word next in the list
        punct.counter <- punct.counter + 1 #increase the counter by 1 because 
      }
  }
  return(unlist(output[1:(punct.counter-1)])) #unlists the output list into a vector over the length of the list minus one because the for loop added one extra to the counter
}
a.punct <- split_punct(a.no.underscore, punctuation.vec) #creates a new word list with the punctuations separated from the words using the split_punct function
a.clean.lower <- tolower(a.punct) #this makes every word

#setwd(r"(C:\Users\Naoise Daly\OneDrive - University of Edinburgh\stat prog\stats_programming_autumn2025\Group_Project_1)")


############ Q5 ############
#------------- example data - Delete later
#a <- tolower(scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
#                  fileEncoding="UTF-8"))[4:108]

unique_words = unique(a.clean.lower);
length(a.clean.lower);length(unique_words)
# this replaces every word in the a, the text, with a number token
# the word is replaced by the location/index of that word in unique_words
M1 = match(a.clean.lower, unique_words)
#----------change example later
#for example, the first 24 words are unique but the 25th is a repetition of
# "might" which already occurred as the 11th word 
# so the 25th entry will have the token "11"
cbind(a.clean.lower, M1)[11:25,]

#----------change to 1000 later
#the model will use only the K most common words as part of its vocabulary
K_most_common_words = 20

#token/index counts
#tabulate takes in the text tokenised as numbers 
#and spits out the counts for each number, in order of that number
#----------- change example later
#i.e #times "1" appears, #times "2" appears, ...
# the token "16" (representing "the") occurs 6 times in the M1
#so position 16 of the output is 6
token_counts = tabulate(M1)

#order tells you what way to rearrange the locations so that 
#the values are in decreasing order - biggest to smallest
#in our case it orders the tokens by their count
#so we can just take the first K of them 
#the k most common tokens can then tell us the k most common words
b = unique_words[
  order(token_counts, decreasing = T)[1:K_most_common_words]
]
head(b);sum(a.clean.lower == b[1]) #'the' is the most common word

#save space by deleting arrays we're done with
rm(token_counts, unique_words)



######### Q6 #############

#so we are tokenising the text (for a second time) based on the vector b
# each word in the text will be represented by a number token or NA
#if the word is not in b, not one of the K most common words,
# it is represented as NA
#if the word is in b, it is represented by its location in b
a_tokenised = match(a.clean.lower, b)


#----------- example data - delete later
# a_tokenised = strsplit(
#   "A cat sat on the mat last night", " "
# )[[1]]

mlag = 20

n = length(a_tokenised) #total number of words
# I thought it was easier to manipulate M as an array first
# remember that the length of each of column is n-mlag
col_length = n-mlag
M = array(NA, col_length*(mlag+1) )

#M can be viewed as sliding a window of length col_length
#over the vector a_tokenised
#the first column of M is the first col_length elements of a_tokenised
#the second column is the next col_length elements starting at the 2nd element
#the ith column is the next col_length elements starting at the ith element

#you don't like loops but this loop has only mlag iterations
#and each iteration has constant time operations
for (i in 0:mlag){
  #the first col_length elements in M are column 1
  #the elements in position col_length+1  to 2*col_length are column 2
  column_i_indices = (i*(col_length) +1) : ((i+1)*col_length)
  #now that we know where column i is in M,
  #we insert the next tokens in the window
  M[ column_i_indices  ]  = a_tokenised[i + 1:col_length ]
}

#now turn M from an array into a matrix
# by default the matrix functions fills in column by column
M <- matrix(M, n-mlag, mlag+1)
dim(M)