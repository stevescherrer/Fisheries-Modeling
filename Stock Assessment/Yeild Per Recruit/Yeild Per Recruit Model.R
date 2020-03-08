#### Yeild Per Recruit Analysis
#### Haddon 2.5 - Simplified Yield-per-recruit
##   Investigating a model's dynamic behavior

#### Written 18 September 2014 by Steve Scherrer

# Calculates yield from natural and fishing mortality 
# 
# Formulas:
# nt = n(t-1) - n(t-1) * (annual_m + annual_h)
#
# nt = population at time t
# n(t-1) = population at time t-1
# annual_m = annual mortality
# annual_h = annual harvest rate

# catch_n = n(t-1) * annual_h
#
# catch_n = catch for that time
# n(t-1) = population at previous time step
# annual_h = annual harvest rate

# catch_kg = catch_n * weight_kg
#
# catch_kg = weight of catch
# catch_n = number of individuals caught
# weight_kg = average weight of an individual



#  Outputs:

# catch_array = data array
  # column 1 = weight at different time steps
  # column 2 = time steps (age classes)
  # column 3 = Poppulation under first set of conditions
  # column 4 = Individuals caught under first set of conditions
  # column 5 = biomass caught under first set of conditions (kg)
  # column 6 = Population under second set of conditions
  # column 7 = Individuals caught under second set of conditions
  # column 8 = biomass caught under second set of conditions (kg)

# totals = vector
  # column 1 = Total number of fish for all age classes under condition 1
  # column 2 = total catch for all age classes under conditon 1
  # column 3 = total biomass for all age classes under condition 1
  # column 4 = total number of fish for all age classes under condition 2
  # column 5 = total catch for all age classes under conditon 2
  # column 6 = total biomass for all age classes under condition 2

## User defined variables
n0 = 1000 #Starting population
tc = 1 # total catch

time_steps = 10 #range of timesteps
timestep_interval = 1 # interval between subsequent timesteps


## defining parameters
timesteps = seq(0,time_steps, timestep_interval)
weight_kg = c(0.042, 0.082, 0.175, 0.283, 0.400, 0.523, 0.700, 0.850, 0.925, 0.990, 1.00)
age = timesteps
total_a = annual_h + annual_m # total annual mortality


## Path to resources folder
lib.loc = "/Library/Frameworks/R.framework/Versions/3.1/Resources/library"

## Installing principle dependencies

install.packages('wesanderson') # color palett for plotting
library("wesanderson", lib.loc)

## Specialty Functions
stock_size = function(timesteps = timesteps, initial_population = n0, annual_mortality = annual_m, annual_harvest = annual_h){
  #   '''
  #   Returns a vector of stock size at age
  #   ex: t = 1, 2, 3, 4 -> Nt = 219, 408, 831
  #   '''
  stock_n = rep(0, length(timesteps))
  stock_n[1] = initial_population
  for (i in 2:length(stock_n)){
    stock_n[i] = stock_n[i-1] - stock_n[i-1] * (annual_mortality+annual_harvest)
  }
  return(stock_n)
}

catch_n = function (timesteps = timesteps, initial_population = n0, annual_harvest = annual_h, annual_mortality = annual_m){
  # '''
  # returns a vector of total number of individuals caught
  # '''
  n=stock_size(timesteps, initial_population, annual_mortality, annual_harvest)
  catch = rep(0, length(timesteps))
  catch[1] = 0
  for (i in 2:length(catch)){
    catch[i] = n[i-1] * (annual_harvest)
  }
  return (catch) 
}

catch_wt = function(timesteps = timesteps, initial_population = n0, annual_harvest = annual_h, annual_mortality = annual_m, weight = weight_kg){
  # '''
  # Returns a vector of total weight of catch using total harvest and mean weight of harvest
  # '''
  c_n = catch_n(timesteps, initial_population, annual_harvest, annual_mortality)
  c_wt = weight * c_n
  return (c_wt)
}

  ## Usage 

pop1 = cbind(stock_size(timesteps = timesteps, initial_population = 1000,
                        annual_harvest = 0.8, annual_mortality = 0.1), 
             catch_n(timesteps = timesteps, initial_population = 1000, 
                     annual_harvest = 0.8, annual_mortality = 0.1),
             catch_wt(timesteps = timesteps, initial_population = n0, 
                      annual_harvest = 0.8, annual_mortality = 0.1, weight = weight_kg))
            
             
pop2 = cbind(stock_size(timesteps = timesteps, initial_population = 1000,
                        annual_harvest = 0.5, annual_mortality = 0.1), 
             catch_n(timesteps = timesteps, initial_population = 1000, 
                      annual_harvest = 0.5, annual_mortality = 0.1),
             catch_wt(timesteps = timesteps, initial_population = n0, 
                      annual_harvest = 0.5, annual_mortality = 0.1, weight = weight_kg))

output = cbind(age, pop1, pop2)
totals = colSums(output)
output = rbind(output, totals)