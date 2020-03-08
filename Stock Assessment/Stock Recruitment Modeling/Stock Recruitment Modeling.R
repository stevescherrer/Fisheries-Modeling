##### Stock Recruitment Modeling
##### Assignment 4
## Written By: Stephen Scherrer 24 Nov 2014 

## R script to recreate  the following excercise boxes by Haddon 
## 10.3
## 10.4
## 10.6

install.packages('wesanderson')
library('wesanderson')
install.packages('minpack.lm')
library('minpack.lm')
install.packages('fishmethods')
library('fishmethods')

#### Appendix 10.3
### Example Box 10.3
## Comparing different model outputs for the Deriso-Schnute generalized stock recruitment model
## Formula for Deriso-Schnute:
  ## R[t+1] = alpha * S[1]*(1-beta*gama*spawn[1])^(1/gama)

## Initial parameter values
alpha = 10
beta = 0.0004
gamma = 0.25
beta_bh = 0.001

## Inital explanitory variable values
spawn = c(1, 10, seq(100, 400, 100), seq(500, 10000, 500))

## Response variable values
recruit.ds = alpha*spawn*(1-beta*gamma*spawn)^(1/gamma)
recruit.ricker = alpha*spawn*exp(-beta*spawn)
recruit.bh = (alpha*spawn)/(1+beta_bh*spawn)
diff = recruit.ricker-recruit.ds

## Pretty plots and gorgeous graphs
plot(spawn, recruit.ds, type = 'l', main = 'Comparing recruitment models', xlab = 'Subsequent Recruitment', ylab = 'Spawning Biomass', xlim = c(0, 12000), ylim = c(0, 10000), col = 'black')
lines(spawn, recruit.ricker, col = wes.palette(5, "Zissou")[2])
lines(spawn, recruit.bh, col = wes.palette(5, "Zissou")[5])
legend('topright', cex = 0.5, c('Deriso-Schnute', 'Ricker', 'Bev-Holt'), col = c('black', wes.palette(5, "Zissou")[2], wes.palette(5, "Zissou")[5]),  lty = c(1,1,1), lwd = 1)


#### Appendix 10.4
### Example Box 10.4
## Fitting Ricker and Beverton-Holt stock recruitment models to tiger prawn data. 

## Initial Parameter Values
a1 = 4.0
b1 = 0.1
a2 = 25.
b2 = 3.

year = seq(75, 82, 1)
stock_index = c(24, 18, 12.8, 11.3, 10.4, 10.1, 10, 8.2, 7.4, 6, 5.7, 3.9, 3.2, 2.4)
recruit_index = c(26.8, 9.9, 21, 19.2, 26.9, 22.1, 18.5, 37.5, 19.7, 12.4, 19.1, 14.3, 7.1, 11.6)

## Modeling relationships
ricker = log(a1)-(b1*stock_index)
oe.sq.r = (log(recruit_index/stock_index)-ricker)^2
e.r = a1*stock_index*exp(-b1*stock_index)
bh = log(a2)-log(b2+stock_index)
oe.sq.bh = (log(recruit_index/stock_index)-bh)^2
e.bh = a2*stock_index/(b2+stock_index)
ssq.ricker = sum(oe.sq.r)
ssq.bh = sum(oe.sq.bh)

ssq.ricker
ssq.bh

# Making models
ricker.ml = nlsLM(log(recruit_index/spawn) ~ log(a1)-(b1*stock_index), start = list(a1 = a1, b1 = b1))
bh.ml = nlsLM(log(recruit_index/spawn) ~ log(a2)-log(b2+stock_index), start = list(a2 = a2, b2 = b2))


## Pretty Plots
#Spawning and Recruitment
plot(stock_index, recruit_index, main = 'Recruitment and Spawning Stock Relationship', xlab = 'Spawning Biomass', ylab = 'Consequent Recruitment', xlim = c(0, 25), ylim = c(0, 40), pch = 16)
lines(stock_index, e.bh, col = wes.palette(5, "Zissou")[5])
lines(stock_index, e.r, col = wes.palette(5, "Zissou")[2])
legend('topright',cex = 0.5, c('Ricker', 'Bev-Holt'), col = c( wes.palette(5, "Zissou")[2], wes.palette(5, "Zissou")[5]),  lty = c(1,1), lwd = 1)


#### Appendix 10.6
### Example Box 10.6
## Modeling the influence of environmental factors on stock recruitment in Australian tiger prawn


## Initial Parameters
a = 4
b = 0.07
c.jan = 0
d.feb = 0

spawn_index = c(2.4, 3.2, 3.9, 5.7, 6, 7.4, 8.2, 10, 10.1, 10.4, 11.3, 12.8, 18, 24)
recruit_index = c(11.6, 7.1, 14.3, 19.1, 12.4, 19.7, 37.5, 18.5, 22.1, 26.9, 19.2, 21, 9.9, 26.8)
ci.jan = c(0, 85, 0, 0, 18, 14, 0, 102, 2, 4, 0, 1, 353, 23)
ci.feb = c(0, 28, 54, 1, 19, 41, 213, 22, 1, 10, 0, 5, 19, 38)

## model fitting
ricker = log(a)+log(spawn_index)-(b*spawn_index)+(c.jan*ci.jan)+(d.feb*ci.feb)
obs.exp.sq = (log(recruit_index)-ricker)^2
ricker.exp = exp(ricker)
ricker.model.log = nlsLM(log(recruit_index) ~ log(a)+log(spawn_index)-(b*spawn_index)+(c.jan*ci.jan)+(d.feb*ci.feb), 
                     start = list(a = a, b= b, c.jan = c.jan, d.feb = d.feb))
summary(ricker.model.log)

ricker.model = nlsLM(recruit_index ~ log(a)+log(spawn_index)-(b*spawn_index)+(c.jan*ci.jan)+(d.feb*ci.feb), 
                     start = list(a = a, b= b, c.jan = c.jan, d.feb = d.feb))

a = summary(ricker.model)$parameters["a", "Estimate"]
b = summary(ricker.model)$parameters["b", "Estimate"]
c = summary(ricker.model)$parameters["c.jan", "Estimate"]
d = summary(ricker.model)$parameters["d.feb", "Estimate"]

## Pretty Plots
# recruitment vs spawning
plot(spawn_index, recruit_index, xlab = 'Spawning Stock Index (Year x)', ylab = 'Recruitment Index (Year x + 1)', main = 'Spawning Stock and Recruitment', pch = 0)
lines(spawn_index, ricker.exp)
lines(spawn_index, predict(ricker.model), lty = 2)
legend('topright', cex = 0.5, legend = c('Observed Data', 'Ricker Model', 'Ricker + Cyclone Index'),
       pch = c(0, NA, NA), lty = c(NA, 1, 2), col = c("Black", "black", "black"), lwd = c(NA, 1, 1))
