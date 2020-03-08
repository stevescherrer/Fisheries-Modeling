##### Non-Equilibrium Surplus Production Modeling
##### Assignment 5
## Written By: Stephen Scherrer 24 Nov 2014 

## R script to recreate  the following excercise boxes by Haddon 
## 11.4
## 11.5
## 11.6


library('bbmle')

setwd('/Users/stephenscherrer/Dropbox/School Stuff/14-15/Fall 2014/MBio 611-Fisheries/Hadden Excercises/Excel')

### Example Box 11.4 - Nonequilibrium surplus production model of the northern Australian tiger prawn fishery

## Loading data
year = read.csv(file = 'EB11.csv', header = TRUE, skip = 7)$Year
cpue.obs = read.csv(file = 'EB11.csv', header = TRUE, skip = 7)$CE_Obs
catch = read.csv(file = 'EB11.csv', header = TRUE, skip = 7)$Catch

## Functions

biomass = function(r, k, b.0, p, catch){
  b.pred = rep(0,length(catch))
  b.pred[1] = b.0
  for (i in 2:length(catch)){
    b.pred[i] = max(b.pred[i-1] + (r/p) * b.pred[i-1] * (1-(b.pred[i-1]/k)^p) - catch[i-1], 100)
  }
  return (b.pred)
}

q.inc_fun = function(cpue.obs, b.pred){
  q.pred = cpue.obs/b.pred
  q_lm = lm(log(q.pred) ~ c(1:length(cpue.obs)))
  q_lm.coeff = q_lm$coef 
  q.inc = exp(q_lm.coeff[1])*exp(q_lm.coeff[2])^(0:(length(cpue.obs)-1))
  return (q.inc)
}

surplus = Vectorize(function(r, k, b.0, p){
  k1 = k*100
  r1 = r/1000
  b.01 = b.0*100
  p1 = p * 0.00000000001
  n = length(catch)
  
  # Calculate predicted biomass at each timestep
  b.pred = biomass(r = r1, k = k1, b.0 = b.01, p = p1, catch = catch)
  
  # Calculate predicted catchability
  q.inc = q.inc_fun(cpue.obs = cpue.obs, b.pred = b.pred)
  
  # Calculating expected CPUE for each timestep
  cpue.pred = q.inc * b.pred
  
  # Calculating Negative Log Likelihoods
  variance = sqrt(sum((log(cpue.pred) - log(cpue.obs))^2)/n)
  neg_log_likelihood = (n/2) * (1+2*log(variance) + log(2*pi))
  return (neg_log_likelihood)
})

n = length(year)

###########################
## Set Objective Function
###########################
surplus = function(r, k, b.0, p){
  
  #Rescale parameters
  k = k * 100
  b.0 = b.0 * 10
  r = r/1000
  p = p * 0.00000000001
  
  #Populate the Biomass Vector 
  b.pred = biomass(r = r, k = k, b.0 = b.0, p = p, catch = catch)
  
  #Calculation of q
  q.pred = cpue.obs/b.pred 

  #Constnat Proportional Increase
  q_lm_coefs = lm(log(q.pred) ~ c(0:(n-1)))$coef
  q.inc = exp(q_lm_coefs[1])*exp(q_lm_coefs[2])^(0:(n-1))
  
  #Populate the expect_CPUE vector
  cpue.pred = q.inc * b.pred 
  
  #Calculate NLL
  variance = sqrt(sum((log(cpue.pred) - log(cpue.obs))^2)/n) #lognormal error structure
  neg_log_likelihood = (length(catch)/2) * (1 + 2*log(variance) + log(2*pi))
  return(neg_log_likelihood)
}


############################
## Parameter Optimization
############################
surplus.optimal = mle2(surplus, start = list(r = 300, k = 300, b.0 = 150, p = 100), method = 'L-BFGS-B', lower = c(r = 0, k = 0, b.0 = 0, p = 0))

surplus.optimal = mle2(surplus, start = list(r = 300, k = 300, b.0 = 150, p = 100), method = 'L-BFGS-B', lower = c(r = 0, k = 0, b.0 = 0, p = 0))
r = surplus.optimal@coef[1]
k = surplus.optimal@coef[2]
b.0 = surplus.optimal@coef[3]
p = surplus.optimal@coef[4]

## Calculating q.inc from optimal params
b.pred = biomass(r = r, k = k, b.0 = b.0, p = p, catch = catch)
q.inc = q.inc_fun(cpue.obs = cpue.obs, b.pred = b.pred)

cpue.pred = q.inc * b.pred


## Pretty plots
# CPUE by time
plot(year, cpue.obs, type = 'l', main = 'CPUE over Time', xlab = 'Year', ylab = 'CPUE t/day', ylim = c(0, .30))
lines(year, cpue.pred, type = 'l', lwd = 4)
legend('topright', cex = 0.5, c('Observed Data', 'Predicted Data'), col = c('black', 'black'),  lty = c(1,1), lwd = c(1,4))

### Example Box 11.5 - Confidence intervals using likelihood profiles
r = 17.5
k = 22

#### PSEUDO CODE

# 11.4 

'11.5 use expand.grid to create a matrix of r, K LL values and plot using persp() for fancy 3d plot. dont need to use color

11.3
surplus production model
b[t+1] = b[t] +rb[t] * (1-e^.....)'

#first estimate starting parameters B0, K, r
B0 = 1.5e6
K = B0*1.4 # K exceeds B0
r = 0.2
q = mean(cpue.obs)/B0

## Parameter start values 

spm = function(params){
  # function takes inputs for Carrying Capacity (K),
  # initial biomass (B0), and growth constant (r) and returns 
  # the sum of residual squares for a cpue model 
  K = params[1]
  B0 = params[2]
  r = params[3]
  b = B0
  q = q
  catch.pred = b.pred = cpue.pred = NULL
  yrs = 1:length(catch)
  for (t in yrs){
    B0 - r*b*(1-b/K)
    b.pred = c(b.pred, b)
    cpue.pred = c(cpue.pred, q*b)
    b = b+B0-catch[t]
    b = max(b, 0)
  }
  error = sum((cpue.obs-cpue.pred)^2)
  return(error)
}

spm2 = function(params2){
  # Determining SSQ for r, q, and B0 inputs. 
  r = params2[1]
  q = params2[2]
  B=B0
  catch.pred = b.pred = cpue.pred = NULL
  yrs = 1:length(catch)
  for (t in yrs){
    B0 - r*b*(1-b/K)
    b.pred = c(b.pred, b)
    cpue.pred = c(cpue.pred, q*b)
    b = b+B0-catch[t]
    b = max(b, 0)
  }
  error = sum((cpue.obs-cpue.pred)^2)
  return(error)
}



#non-linear minimization to estimate K, B0, r
params.init = c(K, B0, r)
estA = nlm(spm, params.init, typsize = params.init, iterlim = 1000)

#now try another estimation for q (and an R?)

K  = estA$est[1]
B0 = estA$est[2]
r = estA$est[3]
q = mean(I)/B0

params2 = c(r,q)

estB = nlm(spm2, params2, typesize = params2, iterlim = 1000)


## running model for range of r and k values
r = 17.5
k = 22
r_incriment = 0.48
k_incriment = 0.9

r_values = seq(r, 50.8, r_incriment)
k_values = seq(k, 42.46 ,k_incriment )

# Modify function to produce LL values for r and k's called LL_r_k

spmll = function(K, B0, r){
  # function takes inputs for Carrying Capacity (K),
  # initial biomass (B0), and growth constant (r) and returns 
  # the sum of residual squares for a cpue model 
  b = B0
  q = q
  catch.pred = b.pred = cpue.pred = NULL
  yrs = 1:length(catch)
  for (t in yrs){
    B0 - r*b*(1-b/K)
    b.pred = c(b.pred, b)
    cpue.pred = c(cpue.pred, q*b)
    b = b+B0-catch[t]
    b = max(b, 0)
  }
  var = sum((cpue.obs-cpue.pred)^2)/(length(cpue.pred))
  return (c(var, cpue.pred))
}

ll_r_k = function(r, k){
  ## Function that outputs a log likelihood for r and k values using the following formual:
  params = spmll(K = k, b0 = (42.1005*1000), r = r)  ## If problems in output, remove *1000
  cpue.pred = params[2]
  var = params[1]
  likelihood_per_obs = rep(0, length(year))
  for (i in 1:length(year)){
    ll[i] = 1/(cpue.obs[i]*sqrt(2*pi*var))*
      exp(-((log(cpue.obs[i])-log(cpue.pred[i]))^2)/
            (2*(var)^2))
    }
  ll = prod(likelihood_per_obs)
}

LL_table = c()

for (r in r_values){
  for (k in k_values){
    rbind(LL_table, c(r/100, k*1000, ll_r_k(r, k)))
  }
}


## Assign plot colors
plot_col = rep(0, length(LL_table[,1]))
for (i in 1:length(LL_table[,1])){
  if (LL_table[,3]>= 17.023){
    plot_col[i] = 1
  }
  else if (LL_table[,3] >= 16.773 && LL_table[,3] < 17.023){
    plot_col[i] = 2
  }
  else if (LL_table[,3] >= 16.523 && LL_table[,3] < 16.773){
    plot_col[i] = 3
  }
  else if (LL_table[,3] >= 16.273 && LL_table[,3] < 16.523){
    plot_col[i] = 4
  }
  else if (LL_table[,3] >= 16.023 && LL_table[,3] < 16.237){
    plot_col[i] = 5
  }
  else if (LL_table[,3] >= 15.773 && LL_table[,3] < 16.023){
    plot_col[i] = 6
  }
  else if (LL_table[,3] >= 15.523 && LL_table[,3] < 15.773){
    plot_col[i] = 7
  }
  else if (LL_table[,3] >= 15.273 && LL_table[,3] < 15.523){
    plot_col[i] = 8
  }
  else if (LL_table[,3] >= 15.023 && LL_table[,3] < 15.273){
    plot_col[i] = 9
  }
  else if (LL_table[,3] >= 14.773 && LL_table[,3] < 15.023){
    plot_col[i] = 10
  }
  else if (LL_table[,3] >= 14.523 && LL_table[,3] < 14.773){
    plot_col[i] = 11
  }
  else if (LL_table[,3] >= 14.273 && LL_table[,3] < 14.523){
    plot_col[i] = 12
  }
  else if (LL_table[,3] >= 14.023 && LL_table[,3] < 14.273){
    plot_col[i] = 13
  }
}

plot_colors = 153:166

## plotting LLhoods
plot(LL_table[plot_col == 1,1], LL_table[plot_col == 1,2], col = plot_colors[1], main = 'Log Likelihood intervals for different values of r and k', xlab = 'r', ylab = 'k', pch = 15)
for (i in 2:length(unique(plot_col))){
  points(LL_table[plot_col = i,1], LL_table[plot_col == i, 2], col = plot_colors[i], pch = 15)
}

