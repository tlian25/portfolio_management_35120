# This program generates solutions for the eighth homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)

f.hold = read.table('fund_holding.txt', comment.char = '%')
names(f.hold) = c('date', 'mweight')
ff.fac = read.table('ff_factors_192607_201912.txt', comment.char = '%')
names(ff.fac) = c('date', 'mktrf', 'smb', 'hml', 'rf')
data = merge(f.hold, ff.fac, by = 'date')
for(varname in c('mktrf', 'smb', 'hml', 'rf')) data[varname] = data[varname]/100
data$mkt = data$mktrf + data$rf
data$fund.return = data$mweight * data$mkt + (1 - data$mweight)*data$rf
data$eret = data$mweight * data$mktrf # same as data$fund.return - data$rf

jensen.mod = lm(eret ~ mktrf, data = data)
summary(jensen.mod)

data$trey.maz = data$mktrf^2
tm.mod = lm(eret ~ mktrf + trey.maz, data = data)
summary(tm.mod)

data$hen.mert = data$mktrf * (data$mktrf > 0)
hm.mod = lm(eret ~ mktrf + hen.mert, data = data)
summary(hm.mod)




