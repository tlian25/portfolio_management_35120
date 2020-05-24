# This program generates solutions for the eighth homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

# Note that before running this file, you must run the command
# install.packages('R.matlab')

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)
library(R.matlab)

rets.data = R.matlab::readMat('rets_hwk8.mat')
flows.data = R.matlab::readMat('flows_hwk8.mat')
rets.data = as.data.frame(rets.data$rets)
rets.data = rets.data[, 2:ncol(rets.data)]
yrs = rets.data[1,]
yr.names = paste0('y', yrs)
names(rets.data) = yr.names 
rets.data = rets.data[2:nrow(rets.data),]
for(varname in names(rets.data)) rets.data[varname][rets.data[varname] == -99] = NA
flows.data = as.data.frame(flows.data$flows)
flows.data = flows.data[, 2:ncol(flows.data)]
names(flows.data) = paste0('y', flows.data[1,])
flows.data = flows.data[2:nrow(flows.data),]
for(varname in names(flows.data)) flows.data[varname][flows.data[varname] == -99] = NA

nyears = length(yr.names)
n = 10
dec.names = paste('d', 1:n)
avgrets = as.data.frame(matrix(NA, n, nyears-1, 
                               dimnames = list(dec.names, yr.names[2:length(yr.names)])))
avgflows = as.data.frame(matrix(NA, n, nyears-1, 
                                dimnames = list(dec.names, yr.names[2:length(yr.names)])))
coefs = as.data.frame(matrix(NA, 3, nyears-1, 
                             dimnames = list(c('a', 'b', 'c'), yr.names[2:length(yr.names)])))
tcoefs = as.data.frame(matrix(NA, 3, nyears-1, 
                              dimnames = list(c('a', 'b', 'c'), yr.names[2:length(yr.names)])))
fitvals = as.data.frame(matrix(NA, n, nyears-1, 
                               dimnames = list(dec.names, yr.names[2:length(yr.names)])))

for(i in 2:length(yr.names)) {
  rf.data = as.data.frame(cbind(rets.data[,i-1], flows.data[,i]))
  names(rf.data) = c('ret', 'flow')
  rf.data = rf.data[is.finite(rf.data$ret) & is.finite(rf.data$flow) & rf.data$flow > -1,]
  rf.data$decile = cut(rf.data$ret, breaks = quantile(rf.data$ret, probs = seq(0, 1, 0.1), type = 5), 
                       labels = 1:10, include.lowest = TRUE)
  agg.data = aggregate(rf.data[,1:2], by = list(rf.data$decile), mean)
  agg.data$decile = as.numeric(as.character(agg.data$Group.1))
  
  agg.data$ret2 = agg.data$ret^2
  rf.mod = lm(flow ~ ret + ret2, data = agg.data)
  summary(rf.mod)
  agg.data$fitted = rf.mod$fitted.values
  
  coefs[, i-1] = rf.mod$coefficients
  tcoefs[, i-1] = summary(rf.mod)$coefficients[,3]
  fitvals[, i-1] = rf.mod$fitted.values
  avgrets[, i-1] = agg.data$ret
  avgflows[, i-1] = agg.data$flow
}

#setEPS()
#postscript("perf_flow_1.eps")
#plot(1:n, avgflows$y2002, lty = 1, type = 'l',
#     main = '2002 Fund Flows As a Function of 2001 Return Deciles',
#     xlab = 'Return Decile', ylab = 'Net Flows')
#lines(1:n, fitvals$y2002, lty = 2, col = 'red')
#legend('bottomright', legend = c('flow','fitted'), lty = 1:2, col = c('black', 'red'))
#dev.off()

y2002flow.est = as.data.frame(rbind(coefs$y2002, tcoefs$y2002))

fm.mean = rowMeans(coefs)
fm.tstat = apply(coefs, 1, function (x) sqrt(length(x))*mean(x)/sd(x))
fama.macbeth = as.data.frame(rbind(fm.mean, fm.tstat))

#setEPS()
#postscript("perf_flow_2.eps")
#plot(1:n, rowMeans(avgflows), lty = 1, type = 'l',
#     main = 'Fund Flows As a Function of Return Deciles - Averages Across Years',
#     xlab = 'Return Decile', ylab = 'Net Flows')
#lines(1:n, rowMeans(fitvals), lty = 2, col = 'red')
#legend('bottomright', legend = c('flow','fitted'), lty = 1:2, col = c('black', 'red'))
#dev.off()

rA = 0.01     
rB1 = -0.35     # is this right?
rB2 = 0.25      # is this right?
p = 0.5         # is this right?      

fm_c = fm.mean
exp_flow_A = fm_c[1] + fm_c[2]*rA + fm_c[3]*rA^2     
exp_flow_B1 = fm_c[1] + fm_c[2]*rB1 + fm_c[3]*rB1^2
exp_flow_B2 = fm_c[1] + fm_c[2]*rB2 + fm_c[3]*rB2^2

exp_fund_size_A = 100*(1+rA)*(1+exp_flow_A) 
exp_fund_size_B = 100*( p*(1+rB1)*(1+exp_flow_B1) + (1-p)*(1+rB2)*(1+exp_flow_B2) )

exp_comp_A = 0.01*exp_fund_size_A
exp_comp_B = 0.01*exp_fund_size_B



