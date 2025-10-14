### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd worked primarily on get.link and nseir, improving logic
# Naoise built much of the main code structure
# Cordelia built the core plotting functionality
# We all collectively debugged, reviewed each others sections, and made improvements

prob_of_link <- function(i, j, b, n_c ){
  ## the probability of person i and person j being regular contacts
  # b is  the vector beta, the sociability parameters for each person in
  # the population
  # i, j is the indices of person i and person j in beta
  # n_c mean number of contacts per person
  # it is assumed that i, j are not in the same household
  # this code assumes i holds a single value, whereas j can be of arbitrary length
  n = length(b)#size of the population
  top = n_c*b[i]*b[j]
  bottom =  (n-1)*(mean(b)^2)
  top/bottom #this is the probability calculated
}

get_net <- function(beta, nc=15){
  # Randomly create a regular contact network based off of the sociability
  # of each person.
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
    #using their sociability rating
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

nseir <- function(beta,h,alink,alpha=c(.1,.01,.01),delta=.2
                  ,gamma=.4,nc=15, nt = 100,pinf = .005){
  
  #This function simulates the progression of a disease through a population.
  #People can be in either the Susceptible, Exposed, Infected, or Recovered states
  #Each day of the simulation, the model assesses the probability of an infectious
  #person infecting susceptible people and moves them into the exposed state. Every day
  #there is a chance for an exposed person to become infectious and for infectious
  #to become recovered. The model stores the total number of people in the population
  #every day in each state for analysis. The model begins with a specified probability
  #of being infected for each person at the very beginning and can be modified.
  
  #----inputs----#
  #beta stores the sociability values for each person
  #h is the vector of every person by household
  #alink is the social contact network produced by get.net
  #alpha is a vector of 3 infectiousness modifying constants.
  #in order they modify household infection, social network, and random mixing.
  #delta is the daily probability of moving from the infectious state to recovered
  #gamma is the daily probability of moving from the exposed state to infectious
  #nc is the mean number of contacts per person
  #nt is the number of days of the simulation
  #pinf is the probability of being infectious at the beginning of the model
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

#set the base parameters needed to generate the social networks and future full models
n =10000; h_max = 5;beta <- runif(n); beta2 <- runif(n); beta3 <- rep(mean(beta2), n)
#create households for everyone
h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
#create social networks for everyone
system.time( network <- get_net(beta)  )

#create results from four separate simulations with differing parameters.
#the first is the full model as intended. The second removes the impact of households
#and social networks on the model but increases random mixing probability.
#The third model includes the household and network impacts but instead assigns
#everyone an equal sociability score which is equal to the mean of the scores from
#the second model. The fourth model combines these two variations to show the outcome
#for only random mixing and with constant sociability scores for the entire population.
system.time(full_model <- nseir(beta, h, network ) ) 
system.time(ran_mix <- nseir(beta2, h,alpha=c(0,0,.04), network ) ) 
system.time(con_beta <- nseir(beta3, h, network ) ) 
system.time(con_beta_ran_mix <- nseir(beta3, h,alpha=c(0,0,.04), network ) ) 


plot_pop_change <- function(simu_upper, simu_lower, pop_size, title){
  #This function plots the number of people in each state every day of the model.
  
  #----inputs----#
  #simu_upper is
  #simu_lower is
  #pop_size is the total population size
  #title is the model title name
  
  days = simu_upper$t
  plot(NA,NA,
       xlim=range(days),
       ylim = c(0,pop_size),
       type = "n", xlab = "Day", ylab = "Population",
       main = title
       #must put in a title
  )
  axis(2, tck = 1, lty = 2, col = "gray")
  colours <-  c(rgb(0, 0, 0, 0.5), rgb(0, 0, 1, 0.5),
                 rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5))
  #need to lower intensity/hue/strength of colours to see overlaps
  states <- c("S", "E", "I", "R")
  #doing them in different order as i want E and R on top
  for (state in states[c(4,1,3,2)] ){
    polygon(
      x = c(days, rev(days)),
      y = c(simu_lower[[state]], rev(simu_upper[[state]]) ),
      col = colours[state==states]
    )
  }
  legend("left", x=max(days), y = n/2,
         legend = c("Susceptible", "Exposed", "Infectious", "Recovered"),
         cex=.9, xjust = .5, yjust = .5,
         fill = colours,
         bty = "n"
  )
}

#set up a 2x2 grid and plot the 4 models next to each other for comparison
par(mfcol=c(2,2),mar=c(4.1,4.1,1.1,1.1))
plot_pop_change(full_model, lapply(full_model, function(x) x - 200), n, "Full Model")
plot_pop_change(ran_mix, lapply(ran_mix, function(x) x - 200), n, "Random Mixing")
plot_pop_change(con_beta, lapply(con_beta, function(x) x - 200), n, "Constant Beta")
plot_pop_change(con_beta_ran_mix, lapply(con_beta_ran_mix, function(x) x - 200), n, "Constant Beta + Random Mixing")


#The apparent effect of the household and network structure seems to be to smooth out
#the speed and intensity in which the disease propagates through the population.
#However, this could reasonably be attributed to the higher value for random mixing
#The relative size of people's households (at hmax=5) and networks (at nc=15)
#is a much smaller subset of the population that possible in random mixing.
#As a result, these social groups, despite higher infectiousness don't play as much
#of a role as the random mixing. Therefore when random mixing is made more infectious
#the disease is more aggressive, spikes higher and earlier than those that have
#these social networks considered and infectiousness broken up more across the three.

