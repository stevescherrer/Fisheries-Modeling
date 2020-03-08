#### Density Dependent Growth Model.
#### Haddon 2.4
  ##   Investigating a model's dynamic behavior

#### Written 17 September 2014 by Steve Scherrer

# The density dependent growth model predicts the poulation size for a given year 
# accounting for the growth rate (birth rate - death rate), carrying capacity of the system,
# and fishing catch
# 
# 
# Formula:
# n1 = n0 + r * n0 (1-n0/K) -Ct
# 
# n0 = initial_population 
# r = growth_rate  
# K = carrying_capacity
# Ct = catch
# 
# 
#  Outputs:

  # population_array = data array
    # column 1 = Time
    # column 2 = Poppulation at time
    # column 3 = Population at next subsequent timestep

  # Line graph of population by time

  # Dot Plot of Nt and Nt+1

  # n20 = Population at timestep 20



## defining model variables

r = 2.456 # instantaneous growth rate (birth - death)
k = 1000 # carrying capacity
n0 = 50 # initial population
ct = 0 # catch rate
time_steps = 100 #number of timesteps 

## defining parameters
timesteps = c(0:time_steps)


# Path to resources folder
lib.loc = "/Library/Frameworks/R.framework/Versions/3.1/Resources/library"

# Installing principle dependencies
install.packages('matlab') # matlab utilitie functions
library("matlab", lib.loc)

install.packages('wesanderson') # color palett for plotting
library("wesanderson", lib.loc)

install.packages('ggplot2')
library("ggplot2", lib.loc)

## Specialty Functions


Ntime = function (time_steps = timesteps, growth_rate = r, initial_population = n0, carrying_capacity = k, catch = ct){
#   '''
#   Returns a vector of population size at time t
#   ex: t = 1, 2, 3, 4 -> Nt = 219, 408, 831
#   '''
  Nt = zeros(1, length(time_steps))
  Nt[1] = initial_population
  for (i in 2:length(Nt)){
    Nt[i] = max( c( (Nt[i-1] + growth_rate * Nt[i-1]* (1 - Nt[i-1]/carrying_capacity) - catch), 0) )
    }
  return (Nt)
}
  

Ntime_1 = function(Nt) {
#   '''
#   Gives population size one subsequent timestep forward from Nt
#   ex: N1 = 408 ----> N2 = 813
#   '''
  if (length(Nt) < 2) { # if argument is not an array
    print('Ntime_1 function requires an array of size greater than or equal to 2') 
    }
    else  
    {
    Nt1 = zeros(1,(length(Nt)-1)) # creates a vector for Nt1 
    for (i in 1:length(Nt1)){ #indexes vector Nt1
      Nt1[i] = Nt[i+1] #Populates vector Nt1 with elements from vector Nt 
    }  
  }
  return (Nt1)
}

tf = function(timesteps, growth_rate, carry_cap){
  Nt = zeros(1, length(timesteps))
  Nt[1] = growth_rate*carry_cap
}

## Usage
  Nt = Ntime(timesteps, r, n0, k, ct) # creates Nt populated vector

  Nt1 = Ntime_1(Nt) # returns Nt1 population at subsequent timestep vector
  
  population_array = t(rbind(t, Nt, c(0,Nt1))) # creates population array 
  
  N20 = Nt[21] # population at time 20
  
  par(mfrow = c(1,2)) #creates layout with two graphs shown on same page

  plot(timesteps, Nt, type = 'l', main = 'Population Size with Time', xlab = 'Time', ylab = 'Population Size', ylim = c(0,1400), 
       col = wes.palette(5, "Zissou")[2]) # creates line graph
  
  plot(Nt[1:50], Nt1[1:50], main = 'Population Changes by Year', xlab = 'Population', ylab = 'Population at subsequent year', 
       pch = 'x', col = wes.palette(5, "Zissou")[5])


