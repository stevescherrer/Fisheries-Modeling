##### Modeling Length and Age
##### Assignment 3 - Appendix
## Written By: Stephen Scherrer 23 Nov 2014 - 24 Nov

## R script to recreate  the following excercise boxes by Haddon 
## 9.1
## 9.2
## 9.3
## 9.5

install.packages('minpack.lm')
library('minpack.lm')
install.packages('fishmethods')
library('fishmethods')

### Appendix 9.1
### Example Box 9.1
## Fitting length and weight at age von Bertalanffy growth curves to data for female Pacific hake. 
# Von B model
  # L[t] = L[inf] * (1-e^-k[ t-t[0] ]) + error
  # where L[t] is length at time t, L[inf] is the average maximum size, k is a growth factor and t[0] is the hypothetical time at which the animal is length 0
# Linearized
  # log(L[t]) = log(L[inf]) * -k [t-t[0]] + log(error)

# Importing data
age = c(c(1,2), seq(3.3, 13.3, 1))
l.obs = c(15.4, 28.03, 41.18, 46.2, 48.23, 50.26, 51.82, 54.27, 56.98, 58.93, 59.00, 60.91, 61.83)
weight.obs = c(0.2, 0.35, 0.72, 1.70, 2.40, 3.12, 3.82, 3.93, 4.22, 3.88, 4.42, 4.13, 4.42)

# Parameters
l.inf = 61.2
K = 0.3
t.0 = -0.06
w.inf = 4.85
b = 3.078

l.exp = sapply(age, function(t) l.inf * (1-exp(-K*(t - t.0))))
w.exp = sapply(age, function(t) w.inf * (1-exp(-K*(t - t.0)))^b)

## Fitting better parameter estimates through non linear least squares
fit.vonB.length = nls(l.obs ~ l.inf * (1-exp(-K*(age - t.0))), 
               start=list(l.inf = l.inf, K = K, t.0 = t.0))
summary(fit.vonB.length)

fit.vonB.weight = nlsLM(weight.obs ~ w.inf*(1-exp(-K*(age - t.0)))^b, start = list(w.inf = w.inf, K = K, t.0 = t.0, b = b))
summary(fit.vonB.weight)

K = summary(fit.vonB)$parameters["K","Estimate"]
l.inf = summary(fit.vonB)$parameters["l.inf","Estimate"]
t.0 = summary(fit.vonB)$parameters["t.0","Estimate"]
b = summary(fit.vonB.weight)$parameters["b", 'Estimate']

## Pretty plots
# weight and Age
plot(age, weight.obs, ylab = 'Weight kg', xlab = 'Age (years)', main = 'Weight and Age')
lines (age, w.exp)

# Length and age
plot(age, l.obs, ylab = 'Length cm', xlab = 'Age (years)', main = 'Optimal von Bertalanffy curve for Pacific hake', pch = 4)
lines(age, l.exp)

### Appendix 9.2
### Example Box 9.2
## Fitting as seasonal growth curve to data for minnows.
  ## The curve fitted is a modified von Bertalanffy model.
# Model: 

## Importing data
t = c(1, 4, 8, 10, 14, 24, 28, 30, 38, 48, 52, 60, 64, 70, 72, 82, 90, 94, 104, 112, 114, 122, 132, 134, 138, 146, 152, 158, 172, 176)
l.obs = c(3, 9, 15, 12, 19, 24, 24, 21, 21, 24, 30, 36, 38, 48, 45, 49, 49, 52, 59, 61, 65, 67, 68, 64, 65, 67, 68, 71, 75, 73)
data = data.frame(t,l.obs)

#Starting parameter values
l.inf = 106.9
K = 0.0068
t.0 = -5.2
c1 = 0.06
c2 = 0
s1 = 4.0
s2 = -20
period2 = 70

## Fitting better parameter estimates
fit.svonB = nlsLM(l.obs ~ l.inf * (1 - exp(-(c1 * sin((2 * pi * (t - s1)) / 52) + c2 * sin((2 * pi * (t - s2)) / period2) + K * (t - t.0)))), 
                  start = list(l.inf = l.inf, K = K, t.0 = t.0, c1 = c1, c2 = c2, s1 = s1, s2 = s2, period2 = period2))
summary(fit.svonB)

single_osc.model = nlsLM(l.obs ~ l.inf * (1 - exp(-(c1 * sin((2 * pi * (t - s1)) / 52)  + K * (t - t.0)))), start = list(t.0 = t.0, K = K, c1 = c1, s1 = s1))


l.inf = summary(fit.svonB)$parameters["l.inf","Estimate"]
K = summary(fit.svonB)$parameters["K","Estimate"]
t.0 = summary(fit.svonB)$parameters["t.0","Estimate"]
c1 = summary(fit.svonB)$parameters["c1","Estimate"]
c2 = summary(fit.svonB)$parameters["c2","Estimate"]
s1 = summary(fit.svonB)$parameters["s1","Estimate"]
s2 = summary(fit.svonB)$parameters["s2","Estimate"]
period2 = summary(fit.svonB)$parameters["period2","Estimate"]

## Predicting values for full model and model without sin functions
l.pred = sapply(t, function(t) l.inf * (1 - exp(-(c1 * sin((2 * pi * (t - s1)) / 52) + c2 * sin((2 * pi * (t - s2)) / period2) + K * (t - t.0)))))
l.pred.ns = sapply(t, function(t) l.inf * (1 - exp(-K*(t-t.0))))

# Other random bits
n = length(l.obs)
ssq = sum((l.pred - l.obs)^2)
ssq.ns = sum((l.pred.ns - l.obs)^2)
## Pretty plots 
# length at time t
plot(t, l.obs, main = 'Length by Age for Minnows', xlab = 'Age (weeks)', ylab = 'Length (mm)')
lines(t, l.pred, lwd = 4)
lines(t, l.pred.ns)
legend('topleft', cex = 0.5, legend = c('Observed Data', 'Sin Model', 'Model without sin term '),
       pch = c(1, NA, NA), lty = c(NA, 1, 1), col = c("Black", "black", "black"), lwd = c(NA, 4, 1))
plot(predict(single_osc.model),(l.obs-predict(single_osc.model)), ylab = 'Residual', xlab = 'Expected Length', pch = 16)
abline(h = 0)
### Appendix 9.3
### Example Box 9.3

## Fitting a growth curve to tagging data using Fabens method
l.inf = 100.39
K = 0.31
t.0 = 0

t.delta = c(rep(170,10), rep(385,10), rep(690,10))
l.t = c(21, 27, 28, 35, 40, 49, 56, 72, 74, 75, 20, 36, 46, 48, 58, 60, 70, 72, 81, 86, 22, 23, 25, 26, 28, 38, 51, 51, 74, 81)
l.delta = c(15.1, 6.4, 11.5, 10.3, 10.8, 9.4, 3.6, 2.1, 5.2, 2.3, 27.2, 14.8, 18.3, 15.2, 12.2, 10.8, 10.2, 7.1, 4.1, 6.2, 28.6, 37.3, 37.2, 26.1, 27.9, 31, 24.7, 19.9, 9.7, 9.3)
t.delta.yrs = t.delta/365
e.l.delta =  (l.inf - l.t) * (1 - exp((-K*t.delta.yrs)))

# modeling change in length by least squares
ls.l.delta = nlsLM(l.delta ~ (l.inf -l.t) * (1-exp((-K*t.delta.yrs))), 
                   start = list(K = K, l.inf = l.inf))
summary(ls.l.delta)
ssq = sum(resid(ls.l.delta)^2)

## Pretty plots
sub1 = cbind(l.t[t.delta == 170], l.delta[t.delta == 170])
sub2 = cbind(l.t[t.delta == 385], l.delta[t.delta == 385])
sub3 = cbind(l.t[t.delta == 690], l.delta[t.delta == 690])
lm.sub1 = lm(sub1[,2] ~ sub1[,1])
lm.sub2 = lm(sub2[,2] ~ sub2[,1])
lm.sub3 = lm(sub3[,2] ~ sub3[,1])

plot(sub1[,1], sub1[,2], xlab = 'Initial Length Lt', ylab = 'Delta L', xlim = c(10, 90), ylim = c(0, 40), main = 'Initial Length and change in length', pch = 0)
points(sub2[,1], sub2[,2], pch = 17)
points(sub3[,1], sub3[,2], pch = 1)
lines(x = c(min(sub1[,1]), max(sub1[,1])), y = lm.sub1$coef[1]+lm.sub1$coef[2]*c(min(sub1[,1]), max(sub1[,1])))
lines(x = c(min(sub2[,1]), max(sub2[,1])), y = lm.sub2$coef[1]+lm.sub2$coef[2]*c(min(sub2[,1]), max(sub2[,1])))
lines(x = c(min(sub3[,1]), max(sub3[,1])), y = lm.sub3$coef[1]+lm.sub3$coef[2]*c(min(sub3[,1]), max(sub3[,1])))
legend('topright', cex = 1, legend = c('170 Days', '385 days', '690 days'),
       pch = c(0, 17, 1), col = c("Black", "black", "black"))

### Appendix 9.5
### Example box 9.5
## Using Kimura's likelihood ratio test to compare von Bertalanffy curves for male and females

## Importing Data
sex = c(rep('F',14), rep('M',10))
t = c(1, 2, 3.3, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3, 10.3, 11.3, 12.3, 13.3, 1, 2, 3.3, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3, 10.3, 11.3)
l = c(15.4, 28.03, 41.18, 46.2, 48.23, 50.26, 51.82, 54.27, 56.98, 58.93, 59, 60.91, 61.83, 15.4, 26.93, 42.23, 44.59, 47.63, 49.67, 50.87, 52.3, 54.77, 56.43, 55.88)

## Starting parameter values
l_f.inf = 61.23
k_f = 0.296
t.0_f = -0.057
l_m.inf = 61.23
k_m = 0.296
t.0_m = -0.057

l.obs_f = l[sex != 'M']
l.obs_m = l[sex == 'M']
t_f = t[sex == 'F']
t_m = t[sex == 'M']

## Makin Models
f.vonB.length = nls(l.obs_f ~ l_f.inf * (1-exp(-k_f*(t_f - t.0_f))), 
                    start=list(l_f.inf = l_f.inf, k_f = k_f, t.0_f = t.0_f))
summary(f.vonB.length)

plot(t_f, l.obs_f, xlab = 'Age in Years', ylab = 'Total Length cm')
lines(t_f[-length(t_f)], unique(predict(f.vonB.length)))
lines(t_m, unique(predict(m.vonB.length)))
points(t_m, l.obs_m, pch = 17 )
legend('bottomright', cex = 0.5, legend = c('Females', 'Males'),
       pch = c(1, 17),  col = c("Black", "black", "black"))

m.vonB.length = nls(l.obs_m ~ l_m.inf * (1-exp(-k_m*(t_m - t.0_m))), 
                      start=list(l_m.inf = l_m.inf, k_m = k_m, t.0_m = t.0_m))
summary(m.vonB.length)

## Hypothesis testing
Kimuras = vblrt(len = l, age = t, group = sex, select = 2, Linf = l_f.inf, K = k_f,  t0 = t.0_f)
Kimuras$results
ssq = sum(Kimuras$residuals[5]^2)

