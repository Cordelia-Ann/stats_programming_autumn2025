### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd 
# Naoise 
# Cordelia 
# We all collectively debugged and reviewed each others sections 

setwd("C:\\Users\\toddh\\Stat Programming\\Statistical-Programming\\stats_programming_autumn2025\\stats_programming_autumn2025\\Group_Project_2\\stats_programming_autumn2025\\Group_Project_2")
prob_of_link <- function(i, j, b, n_c ){
  ## the probability of person i and person j being regular contacts
  # b is  the vector beta, the sociability parameters for each person in t
  # the population
  # i, j is the indices of person i and person j in beta
  # n_c mean number of contacts per person
  # it is assumed that i, j are not in the same household
  # this code assumes i holds a single value, whereas j can be of arbitrary
  # length
  n = length(beta)#size of the population
  top = n_c*b[i]*b[j]
  bottom =  (n-1)*(mean(beta)^2)
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
  network <- vector("list", n)
  
  #i only considers the indices greater than i
  #this avoids both i and j considering i<->j 
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
    u <- runif(length(potential_contacts))
    contacts = potential_contacts[ u < chance_of_contact ]
    network[[i]] <- c( network[[i]], contacts  )
    # add person i into all of its contacts' contacts
    network[contacts] <- lapply(network[contacts], function(x) append(x, i) )
  }
  #take out the placeholder NA, without leaving attributes from na.omit
  lapply(network, function(x) as.numeric(na.omit(x)) )
}

set.seed(2025)
n =1000; h_max = 5;beta <- runif(n)

h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]

system.time( network <- get_net(beta)  )

nseir <- function(beta, h, alink, alpha = c(.1, .01, .01), 
                  delta = .2, gamma = .4, nc = 15, nt = 100, pinf = .005) {
    ## SEIR stochastic simulation model.
    ## beta = sociability score; pinf = initially infective; nt = number of days
    ## gamma = daily prob E -> I; delta = daily prob I -> R;
    x <- rep(0,n) ## initialize to susceptible state
    infect <- length(h)*pinf
    x[1:infect] <- 2 ## create some infectives
    S <- E <- I <- R <- rep(0,nt) ## set up storage for pop in each state
    S[1] <- n-infect;I[1] <- infect ## initialize
    for (i in 2:nt) { ## loop over days
      u <- runif(n) ## uniform random deviates
      x[x==2&u<delta] <- 3 ## I -> R with prob delta
      x[x==1&u<gamma] <- 2 ## E -> I with prob gamma
      
      if household member infected
      x[x==0&u<alpha[1]] <- 1 ## S -> E with probabilities dependent on
      else
      if contact network infected
      x[x==0&u<alpha[2]] <- 1 ## S -> E with probabilities dependent on
      else random interaction
      x[x==0&u<((alpha[3]*nc*beta[i]*beta[j])/(mean(beta)*(n-1))] <- 1 ## S -> E with probabilities dependent on
      
      
      x[x==0&u<beta*I[i-1]] <- 1 ## S -> E with probabilities dependent on
      #their relationship to the other people. so need to determine if they are
      #in the household of the compared person, or in network, or random contact.
      S[i] <- sum(x==0); E[i] <- sum(x==1)
      I[i] <- sum(x==2); R[i] <- sum(x==3)
    }
    list(S=S,E=E,I=I,R=R,beta=beta)
  } ## seir
model_days <- nseir(social_score, h, reg_contact)
