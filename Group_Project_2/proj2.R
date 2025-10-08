rm(list=ls())
prob_of_link <- function(i, j, b, n_c ){
  ## the probability of person i and person j being regular contacts
  # b is  the vector beta, the sociability parameters for each person in t
  # the population
  # i, j is the indices of person i and person j in beta
  # n_c mean number of contacts per person
  # it is assumed that i, j are not in the same household
  # this code assumes i holds a single value, whereas j can be of arbitrary
  # length
  n = length(b)#size of the population
  top = n_c*b[i]*b[j]
  bottom =  (n-1)*(mean(b)^2)
  top/bottom
}

get_net <- function(beta, nc=15){
  # Randomly create a regular contact network based off of the sociability
  # of each person,
  # beta stores the sociability values for each person
  # nc is the average number of contacts per person
  #
  # returns a list, network, where network[i] contains the indices
  # of the people i is a regular contact with
  #
  # go through each person and randomly create contacts with other people
  # but we do not consider creating a contact between j and i if we already
  # considered creating a contact between i and j - this would double the 
  # chance of a contact
  
  n <- length(beta) #size of the population
  network <- list();network[1:n] = NA #we will remove the NA at the end
  
  #i only considers the indices greater than i
  #this avoids both i and j considering i<->j 
  u_vec <- runif(n-1) #precompute some coin flips
  for (i in 1:(n-1)){
    #omit people that already considered creating a link with i
    potential_contacts = (i+1):n
    #omit members of the same household, taking h from outside environment
    same_household = which(h == h[i])
    potential_contacts = potential_contacts[
      !(potential_contacts %in% same_household) ]
    #get the chance of contact between i and the potentials
    # using their sociabilities
    chance_of_contact = prob_of_link(i, potential_contacts, beta, nc)
    #then flip a coin with that probability to decide a contact
    contacts = potential_contacts[u_vec[i] < chance_of_contact ]
    network[[i]] <- c( network[[i]], contacts  )
    # add person i into all of its contacts' contacts
    network[contacts] <- lapply(network[contacts], function(x) append(x, i) )
  }
  #take out the placeholder NA, without leaving attributes from na.omit
  lapply(network, function(x) as.numeric(na.omit(x)) )
}

# set.seed(2025)
# this shows that the average contacts will on average be nc
#it takes about a minute to run
num_reps <- 1000
sampled_avg_contacts = numeric(num_reps)
for (i in 1:num_reps){
  n =100; h_max = 5;beta <- runif(n)
  
  h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
  
  system.time( network <- get_net(beta)  )
  length(network)
  counts <- sapply(network, function(x) length(x) )
  sampled_avg_contacts[i] <- mean(counts)
}
hist(sampled_avg_contacts);mean(sampled_avg_contacts)

set.seed(2025)
n =100; h_max = 5;beta <- runif(n)

h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]

# system.time( network <- get_net(beta)  )
library("debug")
mtrace(get_net)
network <- get_net(beta)
mtrace.off()
length(network)
counts <- sapply(network, function(x) length(x) )
counts
mean(counts)
hist(counts)

#checking if prob_of_link is right
bb <- c(.1,.2, .3,.4);n_c <-2
mean(bb)
(n_c*0.1*.2)/((1/16)*3)
prob_of_link(1,2,bb,n_c)
