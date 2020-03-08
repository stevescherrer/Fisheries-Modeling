#### Virtual Population Analysis
## Script for generating plots for population, cohort size and yeild per recruit for dissertation slides 

### Defining Population parameters
popi = 1000
b = .3
m = .10
f1 = .10
f2 = .40

### Functions to perform VPA
estimate_population = function(n_init = 1000, b, m, f, iter = 10){
  n = c(n_init, rep(0, iter))
  for(t in 2:(iter+1)){
    n[t] = max(round(n[t-1]*(b - (m + f)) + n[t-1]), 0)
  }
  return(n)
}

age_struct = function(n_init, m, f, iter = 10){
  n = c(n_init, rep(0, iter))
  for(t in 2:(iter+1)){
    n[t] = max(round(n[t-1] - n[t-1]*(m + f)), 0)
  }
  return(n)
}

t = 1:15
nc = c(n_init, rep(0, 15))
for(t in 1:15){
  nc[t+1] = (f/(f+m))*n[t]*(1-exp(-(m + f)))
}

ypr = function(m, f = .1, n_init = 1000, iter = 15){
  nz = rep(0, iter)
  nc = rep(0, iter)
  n = c(n_init, rep(0, iter))
  for(t in 1:iter){
    nz[t] = (n[t]*(1-exp(-(m+f))))
    nc[t] = ((f/(f+m))*n[t]*(1-exp(-(m + f))))
    n[t+1] = n[t] - nz[t]
  }
  
  von_b_eq = function(t, t0, linf, k){
    ## Get estimated length at time t using von bertalanffy function
    return(linf*(1-exp(-k*(t-t0))))
  }
  paka_size = von_b_eq(1:iter, t0 = .219, linf = 67.6, k = .37)
  paka_weight = 0.0000381465*paka_size^2.79567
  
  return(sum(nc * paka_weight) / n_init)
}



### Calculate YPR for different fishing scenarios
yeilds = data.frame('f' = seq(0, .5, .01), 'ypr' = NA, stringsAsFactors = F)

for(i in 1:length(yeilds$f)){
  yeilds$ypr[i] = ypr(m, yeilds$f[i])
}


pdf(file.path(fig_dir, 'YPR.pdf'), height = 3.5, width = 4.4)
par(mfrow = c(1, 1), bg = black, col.axis = grey, col.main = white, col = grey, col.lab = grey, fg = grey)
plot(ypr ~ f, data = yeilds, type = 'l', ylab = 'Yeild Per Recruit (lbs)', xlab = 'Fishing Mortality (F)', col = blue, lwd = 4)
dev.off()

### Estimating population and cohort structure under different mortality parameters
pop = data.frame('years' = 0:15)
pop$n_1 = estimate_population(n_init = 1000, b = b, m = m, f = f1, iter = 15)
pop$n_2 = estimate_population(n_init = 1000, b = b, m = m, f = f2, iter = 15)
pop$c_1 = age_struct(n_init = 1000, m = m, f = f1, iter = 15)
pop$c_2 = age_struct(n_init = 1000, m = m, f = f2, iter = 15)

pdf(file.path(fig_dir, 'Population Dynamics Under F.pdf'), height = 3.5, width = 4.4)
par(mfrow = c(1, 1), bg = black, col.axis = grey, col.main = white, col = grey, col.lab = grey, fg = grey)
plot(n_1 ~ years, data = pop, type = 'l', col = blue, lwd = 4, ylim = c(0, max(pop$n_1)), ylab = 'Population Size', xlab = 'Years')
lines(n_2 ~ years, data = pop, type = 'l', col = blue, lwd = 4)
dev.off()

pdf(file.path(fig_dir, 'Cohort Dynamics Under F.pdf'), height = 3.5, width = 4.4)
par(mfrow = c(1, 1), bg = black, col.axis = grey, col.main = white, col = grey, col.lab = grey, fg = grey)
plot(c_1 ~ years, data = pop, type = 'l', col = blue, lwd = 4, ylim = c(0, max(pop$c_1)), ylab = 'Cohort Size', xlab = 'Years Following Recruitment')
lines(c_2 ~ years, data = pop, type = 'l', col = blue, lwd = 4)
dev.off()