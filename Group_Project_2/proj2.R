### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd 
# Naoise 
# Cordelia 
# We all collectively debugged and reviewed each others sections 

setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_2\\stats_programming_autumn2025\\Group_Project_2")


n <- 1000 #increase to 10000 when testing is over
hmax = 5 #max number of people per household

#h is the household number with the quantity of people in it, max 5
h <- rep(1:n, sample(1:hmax, n, replace=TRUE)) |> head(n) |> sample()
table(h) #remove later
which(h == 111)#remove later
social_score <- runif(n)


#create a function that returns a list, the ith element of which is a vector 
#the indices of the regular (non-household) contacts of person i

get.net <- function(beta,households,nc=15) {
  house <- length(households)
  net <- vector("list", house) # this cool command makes a vector of length n, has quite a few different options
  links <- list()
  
  # while we don't have enough links
  while (length(links) < (house * nc) / 2) {  # divide by 2 because each link is shared
    
    i <- sample(1:house, 1)
    j <- sample(setdiff(1:house, i), 1) # sample two different people
    
    # unordered pair because we dont want say 5<->62 and also 62<->5
    pair <- sort(c(i, j))
    pair_key <- paste(pair, collapse = "<->")
    #print(pair_key) #remove later
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
reg_contact <- get.net(social_score, h)

nseir <- function(beta, h, alink, alpha = c(.1, .01, .01), 
                  delta = .2, gamma = .4, nc = 15, nt = 100, pinf = .005) {
    ## SEIR stochastic simulation model.
    ## beta = sociability score; pinf = initially infective; nt = number of days
    ## gamma = daily prob E -> I; delta = daily prob I -> R;
    x <- rep(0,n) ## initialize to susceptible state
    x[1:ni] <- 2 ## create some infectives
    S <- E <- I <- R <- rep(0,nt) ## set up storage for pop in each state
    S[1] <- n-ni;I[1] <- ni ## initialize
    for (i in 2:nt) { ## loop over days
      u <- runif(n) ## uniform random deviates
      x[x==2&u<delta] <- 3 ## I -> R with prob delta
      x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
      x[x==0&u<beta*I[i-1]] <- 1 ## S -> E with prob beta*I[i-1]
      S[i] <- sum(x==0); E[i] <- sum(x==1)
      I[i] <- sum(x==2); R[i] <- sum(x==3)
    }
    list(S=S,E=E,I=I,R=R,beta=beta)
  } ## seir
model_days <- nseir(social_score, h, reg_contact)