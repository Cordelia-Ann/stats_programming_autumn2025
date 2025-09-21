
setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_1\\stats_programming_autumn2025\\Group_Project_1") ## comment out of submitted
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83, fileEncoding="UTF-8")
#test <- c("Be broken, [test my willingess] to [bring about] an end. Turn thy neighbor [james] into [nothing.")
#test.split <- strsplit(test," ")[[1]] ##vector of shakespeare words
#test.split
open.bracket <- grep("[", a, fixed=TRUE)
close.bracket <- grep("]", a, fixed=TRUE)
data.length <- length(a)

stage.directions <- c()
index <- 1
while(index <=length(open.bracket)) {
  
  
  if ("]" %in% a[open.bracket:close.bracket]) {
    close.index <- grepl("]", a, fixed=TRUE)}}

