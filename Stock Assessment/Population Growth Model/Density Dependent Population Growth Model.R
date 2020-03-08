#### Density Dependent Growth Modeling
#### Haddon 2.1
##   Investigating a model's dynamic behavior

#### Written 18 September 2014 by Steve Scherrer

# Population growth curves under different birth and death rate conditions
# 
# 
# Formula:
# nt = n0*e^(b-d)
# 
# nt = size of population at current timestep
# n0 = Initial size of population
# b = birth rate  
# d = death rate
# 
# 
#  Outputs:

# population_array = data array
# column 1 = Time
# column 2 = Poppulation under first set of conditions
# column 3 = Population under second set of conditions
# column 4 = population under third set of conditions

# Line graph of three populations by time

## User defined variables
time_steps = 8 #range of timesteps
timestep_interval = 0.5 # interval between subsequent timesteps
n0 = 500 #Starting population

## defining parameters
timesteps = seq(0,time_steps, timestep_interval)


## Path to resources folder
lib.loc = "/Library/Frameworks/R.framework/Versions/3.1/Resources/library"

## Installing principle dependencies

install.packages('wesanderson') # color palett for plotting
library("wesanderson", lib.loc)

## Specialty Functions
population_size = function(birth_rate = b, death_rate = d, time_steps = timesteps, initial_population = n0){
  #   '''
  #   Returns a vector of population size at time t
  #   ex: t = 1, 2, 3, 4 -> Nt = 219, 408, 831
  #   '''
  pop_size = rep(0, length(timesteps))
  pop_size[1] = initial_population
  r = birth_rate-death_rate #calcluates growth rate from births and deaths
  for (i in 2:length(pop_size)){
    pop_size[i] = max( c(n0*exp(r*time_steps[i]), 0))
      }
  return(pop_size)
}

## Usage 
pop1 = population_size(birth_rate = 0.1, death_rate = 0.04, time_steps = timesteps, initial_pop = n0)
pop2 = population_size(birth_rate = 0.1, death_rate = 0.1, time_steps = timesteps, initial_pop = n0)
pop3 = population_size(birth_rate = 0.1, death_rate = 0.125, time_steps = timesteps, initial_pop = n0)

population_array = cbind(timesteps, pop1, pop2, pop3) #outputs population array

plot(timesteps, pop1, type = 'l', main = 'Population change with time', xlab = 'Time', ylab = 'population', ylim = c(0, 900), col = wes.palette(5, "Zissou")[3])
lines(timesteps, pop2, type = 'l', col = wes.palette(5, "Zissou")[2])
lines(timesteps, pop3, type = 'l', col = wes.palette(5, "Zissou")[5])
