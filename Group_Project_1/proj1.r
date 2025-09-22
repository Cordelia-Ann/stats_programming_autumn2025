
setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_1\\stats_programming_autumn2025\\Group_Project_1") ## comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")

open.bracket <- grep("[", a, fixed=TRUE) #this finds all the locations in a where there is an open bracket
close.bracket <- grep("]", a, fixed=TRUE) #this finds all the locations in a where there is an close bracket
data.length <- length(a) #this finds the total length of the shakespeare text

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
  } else {
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
  } else {
    #do nothing
  }
  name.counter <- name.counter+1 #increase our index to step through all of our remaining words
}

stage.names <- unlist(stage.names) #unlist the stage.names into a vector of positions (still not sure why it starts as a list not vector)
a.no.names <- a.no.stage[-stage.names] #remove the all caps words from our list of words
a.no.underscore <- gsub("_", "", a.no.names, fixed=TRUE)
