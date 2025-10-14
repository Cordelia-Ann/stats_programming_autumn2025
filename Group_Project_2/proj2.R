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

nseir <- function(beta,h,alink,alpha=c(.1,.01,.01),delta=.2
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

# plot_pop_change <- function(sim, pop_size, title="", ...){
#   # Plots the number of people in each state (S,E,I,R)
#   # over the course of the simulation
#   # sim  - output of nseir. a list with S,E,I,R,t of length nt with the counts 
#   #        people in that state each day
#   # pop_size - the total population size
#   # title - a title for the plot
#   # ... - all other arguements are passed to plot
#   
#   #initialise an empty plot
#   plot(NA,NA, type = "n", #show nothing yet
#        xlim = range(sim$t), #days on X axis
#        ylim = c(0,n), #number of people on the Y axis
#        main = title,
#        cex = .8,
#        xlab = "Days", ylab = "Size", ...
#   )
#   # a colour for each state
#   cols <-  c("black", "red", "blue", "purple")
#   states <- c("Susceptible", "Exposed", "Infectious", "Recovered")
#   #draw a line of each state over time
#   lines(sim$t, sim$S, lwd = 2, col = cols[1])
#   lines(sim$t, sim$E, lwd = 2, col = cols[2])
#   lines(sim$t, sim$I, lwd = 2, col = cols[3])
#   lines(sim$t, sim$R, lwd = 2, col = cols[4])
# 
#   legend(
#     x = max(sim$t), y = n/2, # centre right
#     xjust = 1, yjust = .5, # legend sits at the edge of the rhs
#     legend = states, fill = cols,
#     bty="n" #no box around the legend
#   )
# }


set.seed(2025)
n =10000; h_max = 5
h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
beta <- runif(n); 
system.time( network <- get_net(beta)  )

realistic_mixing_variable_beta <- nseir(beta, h, network)
random_mixing_variable_beta <- nseir(beta, h, network, alpha=c(0,0,.04))
beta_c <- rep(mean(beta), n)
realistic_mixing_common_beta <- nseir(beta_c, h, network)
random_mixing_common_beta <- nseir(beta_c, h, network, alpha=c(0,0,.04))


# save graphical state to reset it after
old_par <- par(no.readonly = TRUE) 


plot_one_sim<- function(one_sim) {
  cols <-  c("black", "red", "blue", "purple")
  states <- c("Susceptible", "Exposed", "Infectious", "Recovered")
  #draw a line of each state over time
  lines(one_sim$t, one_sim$S, lwd = 2, col = cols[1])
  lines(one_sim$t, one_sim$E, lwd = 2, col = cols[2])
  lines(one_sim$t, one_sim$I, lwd = 2, col = cols[3])
  lines(one_sim$t, one_sim$R, lwd = 2, col = cols[4])
} 
old_par<- par(no.readonly = T)
par(
  mfrow=c(2,2),
  mgp=c(2,1,0)
)

 days = realistic_mixing_common_beta$t
#top left so no right or bottom border
par(mar=c(0,4,4,0))
plot(NA,NA, type = "n", #show nothing yet
     xlim = range(days), #days on X axis
     ylim = c(0,n), #number of people on the Y axis
     main = "Realistic mixing \n varying sociability",
     ylab = "Size",
     xaxt = "n", xlab = "" #nothing on bottom
)
plot_one_sim(realistic_mixing_variable_beta)
#top right so no left or bottom border
par(mar=c(0,0,4,2))
plot(NA,NA, type = "n", #show nothing yet
     xlim = range(days), #days on X axis
     ylim = c(1,n), #number of people on the Y axis
     main = "Random mixing \n varying sociability",
     ylab = "", yaxt = "n",
     xaxt = "n", xlab = "" #nothing on bottom
);plot_one_sim(random_mixing_variable_beta)
#bottom left so no right or top border
par(mar=c(6,4,0,0))
plot(NA,NA, type = "n", #show nothing yet
     xlim = range(days), #days on X axis
     ylim = c(0,n), #number of people on the Y axis
     ylab = "Size",
     xlab = "Days" #nothing on bottom
);plot_one_sim(realistic_mixing_common_beta)
title("Realistic mixing \n constant sociability"
      , line = -11)
#bottom right so no left or top border
par(mar=c(6,0,0,2))
plot(NA,NA, type = "n", #show nothing yet
     xlim = range(days), #days on X axis
     ylim = c(0,n), #number of people on the Y axis
     ylab = "", yaxt = "n",
     xlab = "Days" #nothing on bottom
);plot_one_sim(random_mixing_common_beta)
title("Random mixing \n constant sociability"
      , line = -11)



# # a colour for each state
# cols <-  c("black", "red", "blue", "purple")
# states <- c("Susceptible", "Exposed", "Infectious", "Recovered")
# #draw a line of each state over time
# lines(sim$t, sim$S, lwd = 2, col = cols[1])
# lines(sim$t, sim$E, lwd = 2, col = cols[2])
# lines(sim$t, sim$I, lwd = 2, col = cols[3])
# lines(sim$t, sim$R, lwd = 2, col = cols[4])
# 
# legend(
#   x = max(sim$t), y = n/2, # centre right
#   xjust = 1, yjust = .5, # legend sits at the edge of the rhs
#   legend = states, fill = cols,
#   bty="n" #no box around the legend
# )



par(old_par) # reset graphical state  

get_confidence_band_nseir <- function(n_reps, n_days, ...){
  big_l <- list()
  big_l$R <-  matrix(NA, nrow=n_reps, ncol=n_days)
  big_l$S <- big_l$E <- big_l$I <- big_l$R
  
  for( i in 1:n_reps){
    simu <- nseir(...)
    big_l$S[i,] <- simu$S
    big_l$E[i,] <- simu$E
    big_l$I[i,] <- simu$I
    big_l$R[i,] <- simu$R
  }
  
  
  simu_lower <- lapply(
    big_l, 
    function(state) {
      apply(state, 2,
            function(day_reps) quantile(day_reps,probs=c(.05))
      )
    }
  )
  simu_lower$t <- 1:n_days
  
  simu_upper <- lapply(
    big_l, 
    function(state) {
      apply(state, 2,
            function(day_reps) quantile(day_reps,probs=c(.95))
      )
    }
  )
  simu_upper$t<- 1:n_days
  return( list(upper=simu_upper, lower = simu_lower) )
}

plot_pop_change_bands <- function(simu_upper, simu_lower, pop_size){
  
  days = simu_upper$t
  plot(NA,NA,
       xlim=range(days),
       ylim = c(0,pop_size),
       type = "n", xlab = "Day", ylab = "N",
       main = "Typical epidemic development"
       #must put in a title
  )
  colours <-  c("black", "red", "blue", "purple")
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
  legend(x=max(days), y = n/2,
         legend = c("Susceptible", "Exposed", "Infectious", "Recovered"),
         xjust = 1, yjust = .5,
         fill = colours,
         bty = "n"
  )
}


