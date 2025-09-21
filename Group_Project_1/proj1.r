
setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_1\\stats_programming_autumn2025\\Group_Project_1") ## comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")
#test <- c("Be broken, [test my willingess] to [bring about] an end. Turn thy neighbor [james] into [nothing.")
#test.split <- strsplit(test," ")[[1]] ##vector of shakespeare words
#test.split
open.bracket <- grep("[", a, fixed=TRUE) #this finds all the locations in a where there is an open bracket
close.bracket <- grep("]", a, fixed=TRUE) #this finds all the locations in a where there is an close bracket
data.length <- length(a) #this finds the total length of the shakespeare text

stage.directions <- c() #creating an empty vector to put the stage directions into
stage.index <-list() #i need to store the actual stage direction position values somewhere
index <- 1 #this creates an index to reference below in the while loop to cycle through all values of open.bracket
length(open.bracket)
length(close.bracket)
while(index <=length(open.bracket)) {     #i believe i have an error when the bracket indexes don't match 
                                          #(i.e. I pass over) a hanging bracket without a pair. I need to get the two to match always or pass over the hanging bracket. There are 3 open brackets than close.
  if (close.bracket[index]!=open.bracket[index]){
      
  then
  
  if (open.bracket[index]+100 >= close.bracket[index]) {        #this checks if the location of the close bracket is within 100 words of the open bracket
                                                                #if it is, it adds the words to a vector (enclosed.set) then adds those words into the vector stage.directions
    enclosed.set <- a[open.bracket[index]:close.bracket[index]]
    stage.directions <- c(stage.directions, paste(enclosed.set))
    stage.index[[length(stage.index) + 1]] <-open.bracket[index]:close.bracket[index]}
  else {
    
  }
  index <- index+1
}

