### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd built the data cleaning and filtering to cover #4
# Naoise built the functionality to cover #5-6
# Cordelia built the functionality to implement #7-9.
# We all collectively debugged and reviewed each others sections 

# This code aims to create a sort of small language model that takes the entire works of Shakespeare
#referred to as "a", for short
# and uses a Markov Model to predict the next word in a given sequence, the key, using sentences from the text.
# The model has a limited vocabulary (b in the code) of the top 1000 most common words  
# It breaks down the text into sequences of length, say mlag,
# and assigns them a uniform probability of being the next word
# It repeats this for words sequences of length shorter than mlag
# the model then chooses the next word according to these probability, with a parameter w
# that allows for preferences for the words suggested by longer/shorter sequences

# setwd("/Users/cordeliabryant/Desktop/Stats_OR_MSc/Stats_Programming_Term1/stats_programming_autumn2025/Group_Project_1")
# setwd(r"(C:\Users\Naoise Daly\OneDrive - University of Edinburgh\stat prog\stats_programming_autumn2025\Group_Project_1)")
rm(list=ls())
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")


#stage directions are annotated in brackets and we want to remove those from consideration
#find the positions in a of all the open and close brackets and store them for removal
open.bracket <- grep("[", a, fixed=TRUE) 
close.bracket <- grep("]", a, fixed=TRUE) 

#store the locations of stage directions and create counters to process through all
#the open and close brackets
stage.directions <-c()
open.counter <- 1
close.counter <- 1 

# go through all the locations of [ or ] and
# if there is a [ followed by a ] nearby by, hardcoded as within 100 positions, 
#then we remove all text in between

while (open.counter <= length(open.bracket) && close.counter <= length(close.bracket)) { 
  
  open.position <- open.bracket[open.counter]
  close.position <- close.bracket[close.counter]
  
  if (close.position < open.position) {
    close.counter <- close.counter + 1
  } else {
    # Check if the close bracket within 100 positions of the open
    stage.gap <- close.position - open.position
    if (stage.gap <= 100) {
      #record the positions of the stage directions and then increase both counters
      stage.directions <- c(stage.directions, open.position:close.position)
      open.counter <- open.counter + 1
      close.counter <- close.counter + 1
    } else {
      #No corresponding close bracket, skip it.
      open.counter <- open.counter + 1
    }
  }
}
#there are some erroneous brackets in the text which could cause duplication in stage.directions
#hence, we make sure that the vector contains only unique positions
stage.directions <- unique(stage.directions[stage.directions <= length(a)])
a.no.stage <- a[-stage.directions] 

#We want to remove stage names which are all uppercase
#"A" and "I" are exceptions as they are technically already uppercase outside of stage names

special.case <- c("a","i","A","I","I,","I.","I;", "I!", "I:", "I?") 

a.no.names <- a.no.stage[!(a.no.stage==toupper(a.no.stage) & !(a.no.stage %in% special.case))]

#Remove all underscores and hypens
a.no.underscore <- gsub("_", "", a.no.names, fixed=TRUE)
a.no.underscore <- gsub("-", "", a.no.underscore, fixed=TRUE)


#create function which will take a word vector and a punctuation vector
#as inputs, and takes any punctuation off the ends of words
#and outputs a new vector with the punctuation as standalone words
#in the position immediately after the word they were attached to.
split_punct <- function(txt, punctuations) {
  #make a regex pattern to find punctuation marks
  collapsed.punct <- paste0("[", paste(punctuations, collapse = ""), "]$")
  
  #Find positions where there is punction
  ends.punct <- grepl(collapsed.punct, txt)
  
  #Trim off the punctuation where its present
  no.punct <- ifelse(ends.punct, substr(txt, 1, nchar(txt) - 1), txt)
  
  #record the punctuation that followed the word, if any
  punct.only <- ifelse(ends.punct,substr(txt, nchar(txt), nchar(txt)),"")
  #recombine the words with their trailing punctuation
  output <- c(rbind(no.punct, punct.only))

  output[output != ""]
}

#split off any punctuation
punctuation.vec <- c(",", ".", ";", "!", ":", "?") 
a.punct <- split_punct(a.no.underscore, punctuation.vec) 
#Then make all words lowercase
a.clean.lower <- tolower(a.punct)


create_b_from_text <- function(txt, K_most_common_words =1000){
  unique_words = unique(txt);
  # this replaces every word in the text, with a number token
  # the word is replaced by the location/index of that word in unique_words
  txt_fully_tokenised = match(txt, unique_words)
  
  #token/index counts
  #tabulate takes in the text tokenised as numbers 
  #and spits out the counts for each number, in order of that number
  #----------- change example later
  #i.e #times "1" appears, #times "2" appears, ...
  # the token "16" (representing "the") occurs 6 times in the M1
  #so position 16 of the output is 6
  token_counts = tabulate(txt_fully_tokenised)
  
  #order tells you what way to rearrange the locations so that 
  #the values are in decreasing order - biggest to smallest
  #in our case it orders the tokens by their count
  #so we can just take the first K of them 
  #the k most common tokens can then tell us the k most common words
  b = unique_words[
    order(token_counts, decreasing = T)[1:K_most_common_words]
  ]
  return(b)
}

b <- create_b_from_text(a.clean.lower, 1000)

############ Question 6 ############

create_M_from_text <- function(txt, b, mlag){
  #so we are tokenising the text based on the vector b
  #if the word is not in b, not one of the K most common words,
  # it is represented as NA
  #if the word is in b, it is represented by its location in b
  a_tokenised = match(a.clean.lower, b)
  
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
  return( matrix(M, n-mlag, mlag+1) )
}


############ Question 7 ############

# Set mlag = 4 (per instructions in assignment)
mlag = 4
# Create M
M <- create_M_from_text(a.clean.lower, b, mlag)
# Create M1
M1 <- match(a.clean.lower, b)
# Remove NAs from M1
M1 <- M1[!is.na(M1)]
a
# Create a vector of punctuation symbols
punctuation_vec <- c(",", ".", ";", "!", ":", "?")
# Initialise the vector of punctuation tokens
punctuation_tokens <- c()

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
  
  # Define the length of the key
  len_key <- length(key)
  # Define mlag through M (so not pulling externally from R environment)
  # dim(M)[2] gives the number of columns in M (and subtracting one from that gives mlag)
  mlag <- dim(M)[2]-1
  # Initialise predictions and weights vectors
  predictions <- c()
  predictions_weights <- numeric()
  
  
  # Now iterate mc through columns of M -- shortening both key and mc
  #shorten the key
  if (length(key) > mlag) key <- key[(length(key)-mlag+1):length(key)]
  #if the key is not of length mlag then simply shortening mlag allows
  #everything here to work the same
  if (length(key) < mlag) mlag <- length(key)
  
  # Define variable mc for shortened versions of len_key
  for (mc in 1:mlag){
    # and reduce length of key used in comparison with M
    sub_key <- key[(len_key-mc+1):len_key]
    
    # Append the new matches found with each sub-key to ii
    ii <- colSums(!(t(M[,(mlag-mc+1):mlag,drop=FALSE])==sub_key))
    # If ii[j] is 0 and is finite, then  contains a match
    matching_rows = which(ii == 0 & is.finite(ii))
    # Add the predictions (rows of M found above) to predicted_tokens
    # This results in a vector containing the predictions of the next word in the phrase
    # Append new_predictions to pre-existing predictions vector
    predictions <- append(predictions, M[matching_rows, mlag+1])
    
    #since sample doesn't need normalised probabilities
    #just use the counts all weighted by the weight for this lag
    predictions_weights <- append(predictions_weights, rep(w[mc]/length(matching_rows),length(matching_rows)))
  }
  
  # Sampling a token from the observed tokens and weighted frequencies
  # Taking resample function from ?sample
  resample <- function(x, ...) x[sample.int(length(x), ...)]

  # If length of the predictions vector is greater than zero (there is at least one prediction)
  # Then resample next_token from the predictions and predictions_weights vector
  if ( length(predictions) > 0 ){
    next_token <- resample(x=predictions, 1, prob=predictions_weights)
  # There are no predictions, so sample next_token randomly from M1
  } else {
    next_token <- resample(x=M1, 1)
  }
  
  # Create a modified M1 vector excluding punctuation tokens for sampling
  # specific_word_retrieval is not coded into the script yet, so punctuation token vector is hard-coded
  punctuation_tokens <- c(1, 2, 11, 23, 46, 14)
  M1_no_punctuation <- M1[! M1 %in% punctuation_tokens]
  # If next_token is NA sample randomly from M1_no_punctuation
  next_token <- if ( is.na(next_token)) sample(M1_no_punctuation, 1) else next_token
  # If next_token and previous token are BOTH punctuation sample randomly from M1_no_punctuation
  next_token <- if ( tail(key, n=1) %in% punctuation_tokens && next_token %in% punctuation_tokens ) sample(M1_no_punctuation, 1) else next_token

  # Return the next token in the phrase
  return(next_token)
}

############ Question 8 ############

create_starter_token <- function(starter_word = NULL, vocab = b){
  "
  This function gives back the token for the word supplied, if it's in the
  vocab. Otherwise, it chooses a token randomly.
  
  Description of Parameters:
  1. If USED, starter_word is the word being looked up
  2. vocab is the vector matching word tokens to words
  "
  
  if (!is.null(starter_word)){ 
    #find the location of the word in the vocab, that will be the token
    token <- which(vocab == starter_word)
    #check that the word was actually in the vocab
    if (length(token) > 0) return(token)
  }
  
  #either no starter word was given or it was not in the vocab
  #so randomly sample one from the vocab
  #but don't sample the words that are punctuations
  punct_vec = unlist(strsplit("!,.?;:¬",""))
  valid_locs <- which( !(vocab %in% punct_vec) )
  # Taking resample function from ?sample
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  # Return a randomly sampled function (ignoring punctuation)
  return( resample(valid_locs, 1) )
}

############ Question 9 ############

print_shakespeare <- function(key, token_compare=b){
  "
  This function converts the final key (vector of tokens) into written text.
  There are no spaces between words and associated punctuation.
  
  Description of Parameters:
  1. key is the vector of generated word-tokens
  2. token_compare is the vector matching word tokens to words
  "
  # Initialize sentence vector (to correspond with key)
  final_sentence <- token_compare[key]
  
  #this function turns a vector of words into a single sentence
  #it ensures there isn't punctuation surround by spaces
  #e.g. "the cat , whose..."
  punct_vec = unlist(strsplit("!,.?;:¬",""))
  #find where the punctuation marks are
  punct_locs <- which(final_sentence %in% punct_vec)
  if(length(punct_locs) >0){
    #join the punctuation marks onto the words proceeding them
    #i found out that paste works this way by experimenting
    final_sentence[punct_locs-1] <- paste(final_sentence[punct_locs-1], final_sentence[punct_locs], sep = "")
    #then remove the entries for those marks
    final_sentence <- final_sentence[-punct_locs]
  }
  # Assign the sentence to a string name
  string_final_sentence <- paste(final_sentence, collapse = " ")
  # Replace first instance (first letter) with uppercase letter using sub()
  # Extract uppercase letter
  string_final_sentence <- sub(substr(string_final_sentence, 1, 1),
                               toupper(substr(string_final_sentence, 1, 1)),
                               string_final_sentence)
  string_final_sentence <- gsub(" i ", " I ", string_final_sentence)
  return(string_final_sentence)
}

# Initialise vector with full stop/question/exclamation,
# and the stop_tokens vector
full_stop_question_exclamation <- unlist(strsplit("!.?",""))
stop_tokens <- c()
for (i in 1:length(full_stop_question_exclamation)){
  stop_tokens <- append(stop_tokens,
                        create_starter_token(full_stop_question_exclamation[i], b))
}

# Choose create_starter_token to initialize the key vector

#key <- create_starter_token()
key <- create_starter_token('romeo', b)
# Initialise final_token_key
final_token_key <- -1
# Enter a loop of the next.word function until a full stop, question, or exclamation is reached
while (! final_token_key %in% stop_tokens){
  # Use the next.word function from Question 7 to predict the next word
  key <- append(key, next.word(key,M,M1))
  # Identify final value in the vector
  final_token_key <- tail(key, n=1)
}
# Print the generated Shakespeare sentence
print_shakespeare(key)

