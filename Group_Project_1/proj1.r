#Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199
#Naoise built the functionality to cover #5-6. Todd built the data cleaning and filtering to cover #4. Cordelia built the functionality to implement 7-9.
# setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_1\\stats_programming_autumn2025\\Group_Project_1") ## comment out of submitted
rm(list=ls())
setwd(r"(C:\Users\Naoise Daly\OneDrive - University of Edinburgh\stat prog\stats_programming_autumn2025\Group_Project_1)")

a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")


#find the positions in a of all the open and close brackets and store them for use
open.bracket <- grep("[", a, fixed=TRUE) 
close.bracket <- grep("]", a, fixed=TRUE) 

#store the locations of stage directions and create counters to process through all
#the open and close brackets
stage.directions <-list()
open.counter <- 1
close.counter <- 1 

#this checks while the length of the open counter is less than or equal to the total 
#number of open brackets and while close counter is less than or equal to the total
#number of close brackets, we move along the positions and takes all the positions
#between two brackets if they are within 100 positions of each other, else it skips
#this should assess only the first close bracket reached after the open bracket.
while (open.counter <= length(open.bracket) && close.counter <= length(close.bracket)) { 
  
  open.position <- open.bracket[open.counter]
  close.position <- close.bracket[close.counter]
  
  if (close.position < open.position) {
    close.counter <- close.counter + 1
  } else {
    # Check if the close bracket within 100 positions of the open
    stage.gap <- close.position - open.position
    if (stage.gap <= 100) {
      #Add the position to the end of the stage direction list then increase both counters
      stage.directions <- c(stage.directions, open.position:close.position)
      open.counter <- open.counter + 1
      close.counter <- close.counter + 1
    } else {
      #No corresponding close bracket, skip it.
      open.counter <- open.counter + 1
    }
  }
}
#remove duplicates, unlist the stage direction positions to then remove from a
#there are still 3 open brackets left, unsure what we want to do with them.
#we could simply remove them during the punctuation section, or ignore.
stage.directions <- unique(stage.directions[stage.directions <= length(a)])
stage.directions.vector <- unlist(stage.directions)
a.no.stage <- a[-stage.directions.vector] 

#Create a vector to store stage names, a counter to loop through the current text,
#and a list of words to avoid removing as they should be considered words when compared
#to their all uppercase versions
stage.names <-c()
name.counter <- 1 
avoid.words <- c("a","i","A","I","I,","I.","I;", "I!", "I:", "I?") 

#Step through the words in a.no.stage to check for stage names
while(name.counter <=length(a.no.stage)) {  
  # this checks if the word is "i" or "a" and skips it
  if (a.no.stage[name.counter] %in% avoid.words){ 
    name.counter <- name.counter +1
    next
  } 
  #this checks if the word (not "i" or "a" is equal to its fully uppercase value)
  if (a.no.stage[name.counter] == toupper(a.no.stage[name.counter])) {        
    # it then adds the position of this word to our stage.names list 
    #(which is a list for some reason and not a vector)
    stage.names[[length(stage.names) + 1]] <- name.counter 
  } 
  #increase our index to continue looking
  name.counter <- name.counter+1 
}

#Unlist the stage.names into a vector of positions (still not sure why it 
#starts as a list not vector). Remove the all caps words from our list of words.
#Remove all underscores and hypens

stage.names <- unlist(stage.names) 
a.no.names <- a.no.stage[-stage.names] 
a.no.underscore <- gsub("_", "", a.no.names, fixed=TRUE)
a.no.underscore <- gsub("-", "", a.no.underscore, fixed=TRUE)

#this is a vector of all punctuation to check for in a.no.underscore
punctuation.vec <- c(",", ".", ";", "!", ":", "?") 

#create function which will take a word vector and a punctuation vector
#as inputs, and takes any punctuation off the ends of words
#and outputs a new vector with the punctuation as standalone words
#in the position immediately after the word they were attached to.
#maybe "wordlist" could be better named "wordvector" or something
split_punct <- function(wordlist, punctuations) {
  #collapse the punctuation vector into a single string 
  collapsed.punct <- paste0("[", paste(punctuations, collapse = ""), "]$")
  
  #Find positions where word has end punctuation
  ends.punct <- grepl(collapsed.punct, wordlist)
  
  #Take the words cutting off the end punctuation if true, else, just the word
  #that didnt have any end punctuation
  no.punct <- ifelse(ends.punct, substr(wordlist, 1, nchar(wordlist) - 1), wordlist)
  #print(no.punct) remove when working correctly
  #print(length(no.punct)) remove when working correctly
  
  #Take the punctuation off of the words that had them at the end only if true, 
  #else it an empty char in the position. This makes sure it is the same length
  #as no.punct (which is every word plus every word without their end punct)
  punct.only <- ifelse(ends.punct,substr(wordlist, nchar(wordlist), nchar(wordlist)),"")
  #print(length(punct.only)) remove when working correctly
  
  #rbind: Take a sequence of vectors, matrix or data-frame arguments and combine by rows. 
  #This takes below:
  #vector1: "word1", "word2", "word3", "word4"   (assume word 4 had an end punct before removal)
  #vector2:  ""    ,  ""    ,   ""   , "1st punct"
  #and makes it look like "word1, "", "word2", "", "word3", "", "word4", "1st punct" 
  output <- c(rbind(no.punct, punct.only))
  #print(output) remove when working correctly
  
  #Remove all the empty strings from the output
  output[output != ""]
}

#Create new word vector with end punctuation split into their own words, using the
#most recently modified text (a.no.underscore), and the vector of all desired 
#punctuation to split
#Then make all words lowercase
a.punct <- split_punct(a.no.underscore, punctuation.vec) 
a.clean.lower <- tolower(a.punct)


############ Q5 ############
#taking this straight from ?sample
resample <- function(x, ...) x[sample.int(length(x), ...)]


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
######### Q6 #############

create_M_from_text <- function(txt, b, mlag){
  #so we are tokenising the text based on the vector b
  #if the word is not in b, not one of the K most common words,
  # it is represented as NA
  #if the word is in b, it is represented by its location in b
  a_tokenised = match(a.clean.lower, b)
  
  #----------- example data - delete later
  # a_tokenised = strsplit(
  #   "A cat sat on the mat last night", " "
  # )[[1]]
  
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

mlag = 10
M <- create_M_from_text(a.clean.lower, b, mlag)
M1 <- na.omit(match(a.clean.lower, b))

######################### Q7

next_word <- function(key, M, M1, w = rep(1,ncol(M)-1) ){
  
  #initially define the maximum order lag from M
  mlag <- ncol(M)-1
  #shorten the key
  if (length(key) > mlag) key <- key[(length(key)-mlag+1):length(key)]
  #if the key is not of length mlag then simply shortening mlag allows
  #everything here to work the same
  if (length(key) < mlag) mlag <- length(key)

  
  combined_u <- numeric()
  combined_u_weights <- numeric()
  for (mc in 1:length(key)){
    #get the rows where the entries from column mc to mlag ALL matched the key
    ii = colSums(!(t(M[,mc:mlag,drop=F]) == key[mc:mlag]) )
    #this will be unaffected by NAs or NaNS as they wont equal one here
    matched_rows = which( ii == 0 & is.finite(ii))
    
    #get the tokens that came next in each of those rows (with duplicates)
    u <-M[matched_rows,mlag+1]
    #since sample doesnt need normalised probabilities
    #just use the counts all weighted by the weight for this lag
    u_weights <- rep(w[mc],length(u))
    
    #just combine these since sample works as expected with duplicates
    combined_u <- c(combined_u, u)
    combined_u_weights <- c(combined_u_weights, u_weights)
  }
  #sample a token from the observed tokens and their weighted frequencies
  
  next_token <- resample(x= combined_u, 1, prob = combined_u_weights)
  #if an NA token was sampled just pick a random token from M1
  next_token<- if (is.na(next_token)) resample(M1,size =1) else next_token
  return(next_token)
}

######################### Q8
create_starter_token <- function(starter_word = NULL, vocab = b){
  #gives back the token for the word supplied, if its in the vocab
  #otherwise it chooses a token randomly
  #
  if (!is.null(starter_word)){ 
    #find the location of the word in the vocab, that will be the token
    token <- which(vocab == starter_word)
    #check that the word was actually in the vocab
    if (length(token) > 0) return(token)
  }
  
  #either no starter word was given or it was not in the vocab
  #so randomly sample one from the vocab 
  #but dont sample the words that are punctuations
  punct_vec = unlist(strsplit("!,.?;:¬",""))
  valid_locs <- which( !(vocab %in% punct_vec) )
  return( 
    resample(valid_locs, 1)
  )
  
}


######################### Q9

merge_words <- function(words){
  #this function turns a vector of words into a single sentence
  #it ensures there isn't punctuation surround by spaces
  #e.g. "the cat , whose..."
  
  punct_vec = unlist(strsplit("!,.?;:¬",""))
  #find where the punctuation marks are
  punct_locs <- which(words %in% punct_vec)
  if(length(punct_locs) >0){
    #join the punctuation marks onto the words proceeding them
    #i found out that paste works this way by experimenting
    words[punct_locs-1] <- paste(words[punct_locs-1], words[punct_locs], sep = "")
    #then remove the entries for those marks
    words <- words[-punct_locs]
  }
  return( paste(words, collapse = " "))
}

print_shakespeare <- function(starter_word=NULL, ...){
  
  #get the sequence started
  token = create_starter_token(starter_word)
  gen_tokens <- c(token)
  cat(token, " ")
  #define when to stop
  full_stop_token = which(b==".")
  while(token != full_stop_token){
    #get the next token and add it in
    token <- next_word(key = gen_tokens, ...)
    cat(token," ")
    gen_tokens <- c(gen_tokens, token)
    #if a . was chosen the loop will stop after this
  }
  cat("\n")
  
  #convert tokens to words and merge them
  sentence = merge_words(b[gen_tokens])
  return(sentence)
  
}
dim(M)
# library("debug");mtrace(next_word)
print_shakespeare("romeo",M,M1)
# mtrace.off()
which(b %in% "romeo")









