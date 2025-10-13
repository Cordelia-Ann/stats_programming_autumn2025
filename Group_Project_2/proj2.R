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
  # a pinf proportion of the population start as infected,
  # the remainder are susceptible
  initial_I <- NULL
  # if no-one starts out infected, this is a worthless simulation
  while (length(initial_I) == 0 ){ initial_I <- which(runif(n) < pinf) }
  #record the initial day
  pop <- rep(0,n); pop[initial_I] <- 2
  S[1] <- n-length(initial_I); I[1] <- length(initial_I)
  # 
  a_h <- alpha[1]; a_c <- alpha[2]; a_r <- alpha[3] 
  cat(table(pop), "\n")
  for (day in 2:nt){
    u <- runif(n)
    pop[ pop==2&u<delta ] <- 3 # I -> R
    pop[ pop==1&u<gamma ] <- 2 # E -> I
    for (infected in which(pop == 2)){
      #infects a susceptible member of their household
      pop[ pop==0&h[pop]==h[infected]&runif(n) <a_h] <- 1
      #infects a susceptible person through random mixing
      v <-  a_r*prob_of_link(infected, pop, beta, nc)
      cat(length(v), "\n")
      pop[pop==0 & runif(n) < a_r*prob_of_link(infected, pop, beta, nc) ] <- 1
      #infects a susceptible regular contact
      pop[ alink[[infected]] ][ pop[alink[[infected]]]==0 & runif(length(alink[[infected]])) < a_c ] <- 1
    }
    #record today's counts
    S[day] <- sum(pop==0); E[day] <- sum(pop==1)
    I[day] <- sum(pop==2); R[day] <- sum(pop==3)
  }
  return(list(S=S,E=E,I=I,R=R, t=1:nt))
}




# this shows that the average contacts will on average be nc
# it takes about a minute to run
# set.seed(2025); num_reps <- 1000
# sampled_avg_contacts <- sampled_sd_contacts <- numeric(num_reps)
# times <- numeric(num_reps)
# for (i in 1:num_reps){
#   n =100; h_max = 5;beta <- runif(n)
#   h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
#   t <- system.time(network <- get_net(beta))
#   times[i] <- t[3] #elapsed times
#   counts <- sapply(network, function(x) length(x) )
#   sampled_avg_contacts[i] <- mean(counts)
#   sampled_sd_contacts[i] <- sd(counts)
# }
# hist(sampled_avg_contacts);mean(sampled_avg_contacts);mean(sampled_sd_contacts)
# hist(times);mean(times)


set.seed(2025)
n =1000; h_max = 5;beta <- runif(n)
h = rep(1:n, times = sample(1:h_max, n, replace =TRUE))[1:n]
x = system.time( network <- get_net(beta)  );x
# length(network)
# counts <- sapply(network, function(x) length(x) )
# mean(counts);sd(counts)
# hist(counts)
simu <- nseir(beta, h, network)

# table(simu)
# sapply(simu, function(x){plot(1:length(x),x)})
simu$I
plot(1:length(simu$I), simu$I)

plot_epidemic1 <- function(Epidemic, Total_Pop, Beta_Vector){
  "Description of function..."
  # Define S, E, I, R variables
  Susceptible <- Epidemic$S; Exposed <- Epidemic$E
  Infected <- Epidemic$I; Recovered <- Epidemic$R
  
  # Set plot window up for histogram
  par(mfcol=c(1,1),mar = c(5, 4, 4, 1))
  # Create a histogram of the beta distribution
  #histogram_beta_values <- hist(Beta_Vector,xlab="Values for Beta",
  #                              main="Uniform Distribution Hist of Beta",
  #                              col="skyblue1")
  # Plot rate of susceptible individuals over time
  plot(Susceptible,ylim=c(0,max(Total_Pop)),xlab="Day",ylab="Number of People",
       main="Tracking Epidemic Infections over Time", type = "l", lwd = 3)
  points(Exposed, col="green", type = "l", lwd = 3) # Add line for Exposed
  points(Infected, col="red", type = "l", lwd = 3) # Add line for Infected
  points(Recovered, col="blue", type = "l", lwd = 3) # Add line for Recovered
  legend("left",
         c("Susceptible", "Exposed", "Infected", "Recovered"),
         bty = "n", # Removes the legend box
         col=c("black", "green", "red", "blue"),
         lwd=10, xpd = TRUE) # Needed to put the legend outside the plot
  plot_popvstime = recordPlot() # Save plot
  
  # Print plots
  #histogram_beta_values
  plot_popvstime
}                          
                          
plot_epidemic1(simu, 1000, beta)
                          
plot_epidemic <- function(Epidemic, Total_Pop, Beta_Vector){
  "Description of function..."
  # Define S, E, I, R variables
  Susceptible <- Epidemic$S; Exposed <- Epidemic$E
  Infected <- Epidemic$I; Recovered <- Epidemic$R
  
  # Set plot window up for histogram
  par(mfcol=c(1,1),mar = c(5, 4, 4, 1))
  # Create a histogram of the beta distribution
  histogram_beta_values <- hist(Beta_Vector,xlab="Values for Beta",
                                main="Uniform Distribution Hist of Beta",
                                col="skyblue1")
  # set plot window up for r plot
  par(mfcol=c(1,1),mar = c(5, 4, 4, 8))
  # Plot rate of susceptible individuals over time
  plot(Susceptible,ylim=c(0,max(Total_Pop)),xlab="Day",ylab="Number of People",
       main="Tracking Epidemic Infections over Time", type = "l", lwd = 3)
  points(Exposed, col="green", type = "l", lwd = 3) # Add line for Exposed
  points(Infected, col="red", type = "l", lwd = 3) # Add line for Infected
  points(Recovered, col="blue", type = "l", lwd = 3) # Add line for Recovered
  legend("topright", inset = c(-0.325, 0),
         c("Susceptible", "Exposed", "Infected", "Recovered"),
         bty = "n", # Removes the legend box
         col=c("black", "green", "red", "blue"),
         lwd=10, xpd = TRUE) # Needed to put the legend outside the plot
  plot_popvstime = recordPlot() # Save plot
  
  # Print plots
  histogram_beta_values
  plot_popvstime
}                          
                          
plot_epidemic(simu, 1000, beta)
                          

