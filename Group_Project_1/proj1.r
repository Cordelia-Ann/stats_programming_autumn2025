#Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199
#Naoise built the functionality to cover #5-6. Todd built the data cleaning and filtering to cover #4. Cordelia built the functionality to implement 7-9.
setwd("/Users/cordeliabryant/Desktop/Stats_OR_MSc/Stats_Programming_Term1/stats_programming_autumn2025/Group_Project_1")
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
K_most_common_words = 1000

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

mlag = 4

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


"
Question 7

"

# The function next.word generates a token for the next word in the phrase

next.word <- function(key,M,M1,w=rep(1,ncol(M)-1)) {
  
  "
  This function returns a token for the next word in the generated
  Shakespearean phrase.
  
  Description of parameters:
  1. key is the word sequence we use to generate the next word
  2. M is the matrix of common word token sequences
  3. M1 is the vector of word tokens for the whole text
  4. w is the vector of mixture weights
  "
  
  ## Moderate length of key (compare to length of rows in M)
  # Calculate length of key and length of M (number of columns)
  len_key <- length(key)
  len_M <- dim(M)[2]
  # If key has more (or equal) words to M1, shorten key
  if (len_key >= len_M) {
    # Exclude beginning tokens from key vector
    key <- key[((len_key+1) - len_M):len_key]
    } else {
    key <- key
  }
  # key now (at least) one word shorter than M
  
  # Initialize a predicted word vector
  predicted_tokens <- c()
  
  # Compare rows of M to key to find predictions
  compare_key <- colSums(!(t(M[,1:length(key),drop=FALSE])==key))
  # If compare_key[i] is 0 and is finite, then  contains a match
  matched_key = which(compare_key ==0 & is.finite(compare_key))
  # Add the predictions (rows of M found above) to predicted_tokens
  predicted_tokens <- M[matched_key, length(key)+1]
  # This results in a vector containing the predictions of the next word in the phrase
  
  # If there are NO predictions, sample randomly from M1
  if ( is.null(predicted_tokens) == TRUE ){
    predicted_tokens <- append(predicted_tokens, sample(M1, 1))
  }
  
  # Associate a probability with each predicted token
  associated_probability <- rep(w[length(key)]/length(predicted_tokens), length(predicted_tokens))

  # Sampling a token from the observed tokens and weighted frequencies (NAOISE)
  # Taking resample function from ?sample
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  next_token <- resample(x = predicted_tokens, 1, prob = associated_probability)
  # Check whether the next_token is NA (REMEMBER TO exclude punctuation!!!!!!!!!!!!!!!!!)
  if ( is.na(next_token) == TRUE ){
    next_word <- sample(M1, 1)
    return(print(paste("This is the next word:", next_word)))
  }
  
  # Return the next word in the word phrase
  ### ASK IN TUTORIAL WHETHER WE CAN INCLUDE B WITHOUT ADDING INTO FUNCTION??? ###
  next_word <- b[next_token]
  
  return(print(paste("This is the next word:", next_word)))
}

# Start with your initial key
key <- c(85)   # or whatever starting token(s) you want

# Generate the next word
key <- next.word(key, M, M1)
key <- next.word(key, M, M1)
key <- next.word(key, M, M1)
key <- next.word(key, M, M1)

# Should BREAK here --> this is where it gets bigger than mlag+1 and causes issues, not super sure why
key <- next.word(key, M, M1)

" So what I have figured out with debugging is that my function will only
generate tokens up to mlag+1. Additionally, the code includes NAs as words"

"
Question 8
"

# THIS ONE WILL RANDOM GENERATE NAs

random_word_retrieval <- function(token_vector=M1, token_compare=b){
  "
  This function initializes the key vector with a random word token.
  Description of Parameters:
  1. M1 is the vector of word tokens for the whole text
  2. token_compare is the vector matching word tokens to words
  "
  
  # Initialise key vector
  key <- c()
  # Create word-token dataframe
  token_compare_df <- data.frame(
    word_token = seq_along(token_compare),  # gives 1,2,3,... length(v)
    given_word = token_compare
  )
  
  # Select a single word token (but not punctuation) at random from the text
  ### Not entirely sure how to omit punctuation given I don't know layout of actual stuff ###
  ### This one is pulling from NAs, too! ###
  random_token <- sample(M1, 1, replace=TRUE)
  
  # Look up the corresponding "seed" word by using which function (gives row number)
  random_word <- token_compare_df[(which(token_compare_df$word_token == random_token)),2]
  
  return(print(paste("This is the initialised key vector:", random_token,
                     "And the corresponding word:", random_word)))
}



### THIS ONE WORKS PROPERLY ###

specific_word_retrieval <- function(given_word, token_compare=b, token_vector=M1) {
  "
  This function retrieves the token for a specific word from the text.
  
  Description of parameters:
  1. given_word (a string) is the given word
  2. token_compare is the vector matching word tokens to words
  3. M1 is the token vector
  
  "
  # Initialise key vector
  key <- c()
  # Create word-token dataframe
  token_compare_df <- data.frame(
    word_token = seq_along(token_compare),  # gives 1,2,3,... length(v)
    given_word = token_compare
  )
  
  # Look up the corresponding token by using which function (gives row number)
  corresponding_token <- token_compare_df[(which(token_compare_df$given_word == given_word)),1]
  
  # Insert the random token into key vector
  key <- c(corresponding_token)
  
  return(print(paste("This is the initialised key vector:", corresponding_token,
                     "And the corresponding word:", given_word)))
}



"
Question 9
"

# Write code to simulate from model until full stop is reached

" Choose either random_word_retrieval OR token_word_retrieval to initialize
the key vector. This code chooses random_word_retrieval to start."

random_word_retrieval()
# specific_word_retrieval('wherefore')

# Enter a repeat loop of the next.word function until a full stop is reached
repeat{
  # Use the next.word function from Question 7 to predict the next word
  next.word(key,M,M1,w=rep(1,ncol(M)-1))
  # Identify final value in the vector
  final_token_key <- key[length(key)]
  
  # Break function if final token in key corresponds with full-stop token
  if (token_compare[(which(token_compare$given_word == ".")),1] == final_token_key)
    break
}

### THIS ONE WORKS, BUT DO WE NEED TO CAPITALIZE THE FIRST LETTER OF THE SENTENCE??? ###

print.shakespeare <- function(key, token_compare=b){
  "
  This function converts the final key (vector of tokens) into written text.
  
  Description of Parameters:
  1. key is the vector of generated word-tokens
  2. token_compare is the dataframe matching word tokens to words
  "
  
  # Initialize sentence vector (to correspond with key)
  final_sentence <- c()
  # Create word-token dataframe
  token_compare_df <- data.frame(
    word_token = seq_along(token_compare),  # gives 1,2,3,... length(v)
    given_word = token_compare
  )
  
  # Loop through each word in the key vector
  for (i in 1:(length(key))){
    # Append the word that corresponds with each token to end of sentence
    final_sentence <- append(final_sentence, token_compare_df[(which(token_compare_df$word_token == key[i])),2])
  }
  # Paste the words back together into one string
  ### I might have to deal with punctuation specifically for this... wait and see later ###
  paste(final_sentence, collapse=" ")
}

print.shakespeare(key)

### DO NOT FORGET THIS FINAL PART (NOT TECHNICALLY PART OF THE QUESTION) ###

" Compare the results to ‘sentences’ obtained by simply drawing common words at
random from the text until a full stop is drawn."
