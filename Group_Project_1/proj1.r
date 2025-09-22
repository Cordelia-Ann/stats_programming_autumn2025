
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

