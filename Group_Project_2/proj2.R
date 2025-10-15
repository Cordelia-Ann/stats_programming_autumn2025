### Naoise Daly s2848034, Todd House s2809867, Cordelia Bryant s2798199 ###

# Todd and Naoise worked simultaneously on nseir, get.net and general code structure
# Cordelia built the core plotting functionality
# We all collectively debugged, reviewed each others sections, and made improvements

# https://github.com/Cordelia-Ann/stats_programming_autumn2025/tree/Naoise-branch/Group_Project_2

############################
# This scripts simulates the spread of an epidemic using a modification of the SEIR model.

# In the SEIR model each individual in a fixed closed population is one of 4 states:
# Susceptible - has not contracted the disease
# Exposed     - has contracted the disease but cannot spread it to others
# Infectious  - has contracted the disease and may spread it to others
# Recovered   - has contracted the disease previously but has now either recovered or passed away
# The dynamics of the epidemic are modelled by how each individual transitions between these states,
# typically on a day by day basis
# An Infectious person will move to Recovered with a set probability delta
# An Exposed person will move to Infectious with a set probability gamma
# In the basic SEIR model a Susceptible person will move to Exposed with a probability
# defined as a set beta times the number of Infectious people
# This script allows for extensions of that last feature

# This scripts allows beta to vary from person to person, capturing a notion of a persons "sociability"
# The prob_of_link function defines the chance of two people randomly meeting each other,
# based on the sociability of each person.
# This is an attempt to better capture chance encounters in the real world.
# The function nseir incorporates into the SEIR framework the fact a person will be part of a household
# and will have a network of regular contact as well as randomly bumping into other people in the population.
# These are different avenues of infections and nseir allows for specifying different chances of infection in each case.
# The script assigns individuals to households (sizes varying uniformly between 1 and 5) 
# and then randomly creates a network of regular contacts for each person using get.net.

# With this in place, we then examine the dynamics seen in the simulations produced in:
# the full, more sophisticated, model with multiple infection avenues and varing beta values
# the full model with multiple infection avenues and with a common beta value for the population
# the basic model where infection spreads only through people randomly mixing but varying beta values
# the basic model where infection spreads only through people randomly mixing and with a common beta



prob_of_link <- function(i, j, b, n_c ){
  ## the probability of person i and person j meeting 
  # b   - the sociability parameters for the whole population
  # i,j - locations of person i and person j in b
  #       (j can be a vector of people)
  # n_c - mean number of contacts per person
  n = length(b)# population size
  top = n_c*b[i]*b[j]
  bottom =  (n-1)*(mean(b)^2)
  top/bottom
}

get.net <- function(beta, nc=15){
  # Randomly creates a regular contact network from the sociabilitiy parameters
  # of the population
  # beta - sociability values for the whole population
  # nc   - average number of contacts per person
  #
  # returns: a list, network, where network[i] are the regular contacts of person i
  
  n <- length(beta) #size of the population
  network <- vector("list",n)
  
  #person i only considers the indices greater than i
  #this avoids both i and j considering i<->j  which would double the chance of 
  # a link being created
  for (i in 1:(n-1)){
    #omit people that already considered creating a link with i
    potential_contacts = (i+1):n
    #omit members of the same household, taking h from the outside environment
    same_household = which(h == h[i])
    potential_contacts = potential_contacts[!(potential_contacts %in% same_household) ]
    #get the chance of contact between i and the potential contacts
    # using their sociabilities
    chance_of_contact = prob_of_link(i, potential_contacts, beta, nc)
    # with that chance, i and j become regular contacts
    u <- runif(length(potential_contacts))
    contacts = potential_contacts[ u < chance_of_contact ]
    network[[i]] <- c( network[[i]], contacts  )
    # add person i into all of their contacts' contacts
    network[contacts] <- lapply(network[contacts], function(x) c(x, i) )
  }
  return( network )
}

nseir <- function(beta,h,alink,alpha=c(.1,.01,.01),delta=.2
                  ,gamma=.4,nc=15, nt = 100,pinf = .005){
  # Simulate the spread of an epidemic in a population under the SEIR model
  # beta  - a vector of sociability parameters for the whole population
  # h     - a vector of households where h[i] is the household of person i
  # alink - a list containing the regular contacts for each individual
  # alpha - the chance of an infectious person infecting a susceptible
  #          household member, regular contact, and person they randomly bump into,
  #          respectively
  # delta - the chance a person moves from Infectious to Recovered
  # gamma - the chance a person moves from Exposed to Infectious 
  # nc    - average number of regular contacts per person, used in random mixing
  # nt    - number of days to run the simulation for
  # pinf  - the proportion of the population that is Infectious initially
  #
  # Returns: a list with vectors S,E,I,R containing the containing the counts
  #         of people in each state each day, and a vector t storing the days
  
  n = length(beta) # population size
  S <- E <- I <- R <- rep(0, nt)
  # the state of each individual is recorded as 0/1/2/3 
  # encoding Susceptible/Exposed/Infectious/Recovered
  pop <- rep(0,n)
  # a random pinf proportion of the population start as infected,
  # the remainder are susceptible
  # if no-one starts out infected, this is a worthless simulation
  #so we hardcode at least one infection
  pop[sample(n, size = max(1, floor(n*pinf))) ] <- 2
  #record the first day
  S[1] <- sum(pop==0); I[1] <- sum(pop==2)
  # extract elements of alpha as explained above
  a_h <- alpha[1]; a_c <- alpha[2]; a_r <- alpha[3] 
  for (day in 2:nt){
    u <- runif(n)
    pop[ pop==2&u<delta ] <- 3 # Infectious -> Recovered
    pop[ pop==1&u<gamma ] <- 2 # Exposed -> Infectious
    
    currently_S <- which(pop==0); n_S <- length(currently_S)
    # each infectious person may infect the currently Susceptible
    for (infected in which(pop == 2)){
      u<- runif(n_S)
      #infects a susceptible person through random mixing
      # this chance of infection is the chance of meeting randomly times a_r
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

plot_pop_change <- function(sim, pop_size, title="", show_legend = T){
  # Plots the number of people in each state S,E,I,R
  # over the course of the simulation
  # sim         - simulated output of nseir. a list with S,E,I,R,t each of length nt 
  #               storing the counts of people in each state for each day in t
  # pop_size    - the total population size
  # title       - a title for the plot
  # show_legend - include a legend or not
  # ...         - all other arguments are passed to plot
  
  #initialise an empty plot
  plot(NA,NA, type = "n", #show nothing yet
       xlim = range(sim$t), #days on X axis
       ylim = c(0,n), #number of people on the Y axis
       main = title,
       cex.main = .8, #reduce title size to 80%
       cex.axis = .7, #reduce axis label size to 70%
       xlab = "Days", 
       ylab = "Number of People",
       yaxt = "n" #don't add Y-ticks here - done below
  )
  #add dotted horizontal gridlines
  axis(2, tck = 1, lty = 2, col ="gray", gap.axis=1/4)
  # a colour for each state
  cols <-  c("black", "blue", "red", "green")
  states <- c("Susceptible", "Exposed", "Infectious", "Recovered")
  #draw a line of each state over time
  lines(sim$t, sim$S, lwd = 2, col = cols[1])
  lines(sim$t, sim$E, lwd = 2, col = cols[2])
  lines(sim$t, sim$I, lwd = 2, col = cols[3])
  lines(sim$t, sim$R, lwd = 2, col = cols[4])
  #add a legend
  legend(
    x = max(sim$t), y = n/2, # centre right
    xjust = .5, yjust = .5, # legend sits at the edge of the rhs
    legend = states, fill = cols,
    bty="n", #no box around the legend
    plot = show_legend, #show the legend on the plot or not
    cex = .8 #make the text smaller
  )
}


n =10000; h_max = 5 # maximum household size
#assign n people to households of sizes between 1-h_max
h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
#randomly sample population sociability parameters
beta <- runif(n)
#create a regualar contacts network for this population
network <- get.net(beta)

#run 4 situations
#the full model has household, regular contacts and random mixing and there are
#different sociablility parameters for each person
realistic_mixing_variable_beta <- nseir(beta, h, network)
# the model with only random mixing but with a higher infection rate for random 
# mixing, again there are different sociablility parameters for each person
random_mixing_variable_beta <- nseir(beta, h, network, alpha=c(0,0,.04))
#now we assume the sociability parameters are a constant average value
beta_c <- rep(mean(beta), n)
# the model has household, regular contacts and random mixing but constant
# sociability
realistic_mixing_common_beta <- nseir(beta_c, h, network)
# the most simple model where there is only random mixing and constant sociability
random_mixing_common_beta <- nseir(beta_c, h, network, alpha=c(0,0,.04))

# save graphical state to reset it later
old_par <- par(no.readonly = TRUE) 
par(mfrow=c(2,2), #2x2 grid
    mar=c(3,3,2,2) # decrease the margins
    ,mgp=c(2,1,0) # bring axis labels closer to axes
)
#plot the 4 models for comparison
plot_pop_change(realistic_mixing_variable_beta, n,
                "Full Model")
#dont show the legends on the remaining plots
plot_pop_change(random_mixing_variable_beta, n, 
                "Random Mixing", F)
plot_pop_change(realistic_mixing_common_beta, n, 
                "Constant Sociability", F)
plot_pop_change(random_mixing_common_beta, n, 
                "Random Mixing and \n Constant Sociability", F)

par(old_par) # reset graphical state  

# Looking top to bottom:
# Models with constant sociability predict more people will eventually contract the disease.
# Looking left to right: 
# Models omitting household and contact networks predict an earlier and sharper spike 
# in the number of people Exposed or Infectious.

# The last, most basic, model with constant sociability and random mixing predicts 90% 
# of the population contracting the disease.
# The first two models with the more realistic assumption that the chance of contracting
# the disease is not the same for every individual predict 80% of the population 
# contracts the disease.
# The basic model predicts significantly worse outcomes for the spread of the epidemic.



#The apparent effect of the household and network structure seems to be to smooth out
#the speed and intensity in which the disease propagates through the population.
#However, this could reasonably be attributed to the higher value for random mixing
#The relative size of people's households (at hmax=5) and networks (at nc=15)
#is a much smaller subset of the population that possible in random mixing.
#As a result, these social groups, despite higher infectiousness don't play as much
#of a role as the random mixing. Therefore when random mixing is made more infectious
#the disease is more aggressive, spikes higher and earlier than those that have
#these social networks considered and infectiousness broken up more across the three.
#The constant beta to have more of a noticeable effect when combined with the random
#mixing as it leads to many more people getting infected and pushes the peak infection
#date slightly to the right.




############ Extra ############

# If we run multiple simulation with the same parameters repeatedly we can 
# create confidence intervals on the counts per state each day.
# Simply get n simulations and go through each day and state, calculating upper 
# and lower quantiles for that state on that day from the n samples observed.
# the below function allows for that, and the function below it caters for 
# plotting such confidence intervals
# get_confidence_band_nseir <- function(n_reps, n_days, ...){
#   big_l <- list()
#   big_l$R <-  matrix(NA, nrow=n_reps, ncol=n_days)
#   big_l$S <- big_l$E <- big_l$I <- big_l$R
#   
#   for( i in 1:n_reps){
#     simu <- nseir(...)
#     big_l$S[i,] <- simu$S
#     big_l$E[i,] <- simu$E
#     big_l$I[i,] <- simu$I
#     big_l$R[i,] <- simu$R
#   }
#   
#   
#   simu_lower <- lapply(
#     big_l, 
#     function(state) {
#       apply(state, 2,
#             function(day_reps) quantile(day_reps,probs=c(.05))
#       )
#     }
#   )
#   simu_lower$t <- 1:n_days
#   
#   simu_upper <- lapply(
#     big_l, 
#     function(state) {
#       apply(state, 2,
#             function(day_reps) quantile(day_reps,probs=c(.95))
#       )
#     }
#   )
#   simu_upper$t<- 1:n_days
#   return( list(upper=simu_upper, lower = simu_lower) )
# }
# 
# plot_pop_change_bands <- function(simu_upper, simu_lower, pop_size){
#   
#   days = simu_upper$t
#   plot(NA,NA,
#        xlim=range(days),
#        ylim = c(0,pop_size),
#        type = "n", xlab = "Day", ylab = "N",
#        main = "Typical epidemic development"
#        #must put in a title
#   )
#   colours <-  c("black", "red", "blue", "purple")
#   #need to lower intensity/hue/strength of colours to see overlaps
#   states <- c("S", "E", "I", "R")
#   #doing them in different order as i want E and R on top
#   for (state in states[c(4,1,3,2)] ){
#     polygon(
#       x = c(days, rev(days)),
#       y = c(simu_lower[[state]], rev(simu_upper[[state]]) ),
#       col = colours[state==states]
#     )
#   }
#   legend(x=max(days), y = n/2,
#          legend = c("Susceptible", "Exposed", "Infectious", "Recovered"),
#          xjust = 1, yjust = .5,
#          fill = colours,
#          bty = "n"
#   )
# }