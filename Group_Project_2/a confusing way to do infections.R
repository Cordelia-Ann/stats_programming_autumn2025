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
  network <- vector("list",n)
  
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
    network[contacts] <- lapply(network[contacts], function(x) c(x, i) )
  }
  return( network )
}

nseir_a <- function(beta,h,alink,alpha=c(.1,.01,.01),delta=.2
                  ,gamma=.4,nc=15, nt = 100,pinf = .005){
  n = length(beta) #size of population
  #record the number of people in each state each day
  S <- E <- I <- R <- rep(0, nt)
  pop <- rep(0,n)
  # a random pinf proportion of the population start as infected,
  # the remainder are susceptible
  # if no-one starts out infected, this is a worthless simulation
  #so we hardcode at least one infection
  pop[sample(n, size = max(1, floor(n*pinf))) ] <- 2
  #record the first day
  S[1] <- sum(pop==0); I[1] <- sum(pop==2)
  # 
  a_h <- alpha[1]; a_c <- alpha[2]; a_r <- alpha[3] 
  for (day in 2:nt){
    u <- runif(n)
    
    pop[ pop==2&u<delta ] <- 3 # I -> R
    pop[ pop==1&u<gamma ] <- 2 # E -> I
    currently_S <- which(pop==0); n_S <- length(currently_S)
    for (infected in which(pop == 2)){
      u<- runif(n_S)
      #infects a susceptible person through random mixing
      pop[currently_S][ u<a_r*prob_of_link(infected, currently_S, beta, nc) ] <- 1
      #infects a susceptible member of their household
      pop[currently_S][ h[currently_S]==h[infected] & u<a_h ] <- 1
      #infects a susceptible regular contact
      pop[currently_S][ currently_S %in% alink[[infected]] & u<a_c ] <- 1
    }
    #record today's counts
    S[day] <- sum(pop==0); E[day] <- sum(pop==1)
    I[day] <- sum(pop==2); R[day] <- sum(pop==3)
  }
  return(list(S=S,E=E,I=I,R=R, t=1:nt))
}

nseir_b <- function(beta,h,alink,alpha=c(.1,.01,.01),delta=.2
                    ,gamma=.4,nc=15, nt = 100,pinf = .005){
  n = length(beta) #size of population
  #record the number of people in each state each day
  S <- E <- I <- R <- rep(0, nt)
  pop <- rep(0,n)
  # a random pinf proportion of the population start as infected,
  # the remainder are susceptible
  # if no-one starts out infected, this is a worthless simulation
  #so we hardcode at least one infection
  pop[sample(n, size = max(1, floor(n*pinf))) ] <- 2
  #record the first day
  S[1] <- sum(pop==0); I[1] <- sum(pop==2)
  # 
  a_h <- alpha[1]; a_c <- alpha[2]; a_r <- alpha[3] 
  for (day in 2:nt){
    u <- runif(n)
    
    pop[ pop==2&u<delta ] <- 3 # I -> R
    pop[ pop==1&u<gamma ] <- 2 # E -> I
    currently_S <- which(pop==0); n_S <- length(currently_S)
    currently_I <- which(pop==2)
    #you'll want your wheatabix before this
    if( length(currently_I) >0 ){
      currently_I <- matrix(currently_I)
      #for each element, called infected, in currently_I
      # we work out who they infect as before by working out probabilities
      # and flipping coins
      #then return that as a list, where entry i is the people who have been
      # infected by i
      # we unlist that as we only need a vector
      # and then we set those susceptable people in the population to exposed
      newly_I <- unlist(apply(
        currently_I,1, 
        function(infected){
          u <- runif(n_S);
          c(
            which(u<a_r*prob_of_link(infected, currently_S, beta, nc))
            ,which(h[currently_S]==h[infected] & u<a_h )
            ,which(currently_S %in% alink[[infected]] & u<a_c)
          )
        }              
      ))
      
      pop[currently_S][newly_I] <- 1
    }

    #record today's counts
    S[day] <- sum(pop==0); E[day] <- sum(pop==1)
    I[day] <- sum(pop==2); R[day] <- sum(pop==3)
  }
  return(list(S=S,E=E,I=I,R=R, t=1:nt))
}


set.seed(2025)
n =10000; h_max = 5;beta <- runif(n)
h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
system.time( network <- get_net(beta)  )

#vary n_reps as you want
# on my laptop either version of nseir takes ~20s
# so it would be 5 mins total
n_reps = 15; times = matrix(NA,n_reps,2)
colnames(times) <- c("orig", "apply")
set.seed(2025)
for (i in 1:n_reps){
  times[i,1] <- system.time(
    nseir_a(beta, h, network)
  )[3]
  times[i,2] <- system.time(
    nseir_b(beta, h, network)
  )[3]
  
}
apply(times,2,mean);apply(times,2,sd)

hist_a <- hist(times[,"orig"],  plot = F)
hist_b <- hist(times[,"apply"], plot= F )
plot(hist_a,  col = rgb(1,0,1,.4),
     xlim = range(times),
     ylim = range(hist_a$counts,hist_b$counts),
     main = "Comparing run times", xlab = "seconds/call"
)
plot(hist_b, add = TRUE, col = rgb(1,0,0,.4))
legend(
  "top", legend = c("original", "apply"),
  fill = c( rgb(1,0,1,.4), rgb(1,0,0,.4)),
  bty="n"
)
