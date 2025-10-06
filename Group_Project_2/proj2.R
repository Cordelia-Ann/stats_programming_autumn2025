### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd 
# Naoise 
# Cordelia 
# We all collectively debugged and reviewed each others sections 



setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_2\\stats_programming_autumn2025\\Group_Project_2")


n <- 1000
hmax = 5

#h is the household number with the quantity of people in it, max 5
h <- rep(1:n, sample(1:hmax, n, replace=TRUE)) |> head(n)
table(h) #remove later
social_score <- rgamma(n, shape= ((5e-5)/(1e-5)), scale=1e-5) # i used the number from 6.2 for the gamma dist, idk if this is the correct number set


#create a function that returns a list, the ith element of which is a vector 
#the indices of the regular (non-household) contacts of person i

get.net <- function(beta,nc=15) {
  n <- length(beta)
  net <- vector("list", n) # this cool command makes a vector of length n, has quite a few different options
  links <- list()
  
  # while we don't have enough links
  while (length(links) < (n * nc) / 2) {  # divide by 2 because each link is shared
    
    i <- sample(1:n, 1)
    j <- sample(setdiff(1:n, i), 1) # sample two different people
    
    # unordered pair because we dont want say 5<->62 and also 62<->5
    pair <- sort(c(i, j))
    pair_key <- paste(pair, collapse = "<->")
    print(pair_key) #remove later
    # check if link already exists, i.e. if pair key is not in the list
    if (!(pair_key %in% names(links))) {
     
      links[[pair_key]] <- TRUE
      
      #so net looks like a list of all 1000 people, 
      #and then each entry gets filled with a corresponding connection j for i and i for j
      net[[i]] <- c(net[[i]], j)
      net[[j]] <- c(net[[j]], i)
    }
  }
  return(net)
}

nseir <- function(beta, h, alink, alpha = c(.1, .01, .01), 
                  delta = .2, gamma = .4, nc = 15, nt = 100, pinf = .005) {
  
}