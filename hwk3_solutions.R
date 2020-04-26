# This program generates solutions for the third homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)

stock.data = read.table('VWMKT_26_19.txt')
names(stock.data) = c('date', 'rm')
bond.data = read.table('TB_26_19.txt')
names(bond.data) = c('date', 'rf')
stock.data$year = floor(stock.data$date / 10000)
bond.data$year = floor(bond.data$date / 10000)
data = merge(stock.data, bond.data, by = 'year')

N=10000
T=length(data$rm)
c=0.02

###########################################################

data$f=data$rm>data$rf
100*mean(data$f)

data$hrm=cumprod(1+data$rm)
data$hrf=cumprod(1+data$rf)
data$hrm[T]
data$hrf[T]

Sm=mean(data$rm-data$rf)/sd(data$rm-data$rf)

###########################################################

data$rperfect=data$rf 
data$rperfect[data$f]=data$rm[data$f] 
data$hrperfect=prod(1+data$rperfect)
data$hrperfect[T]
mean(data$rperfect)
mean(data$rperfect-data$rf)/sd(data$rperfect-data$rf)
mean(data$rm)
Sm

###########################################################

data$rnever=data$rm
data$rnever[data$f]=data$rf[data$f]

out2 = matrix(NA, T, N)
for (j in 1:N) {
  f2 = sample(0:1, T, replace = TRUE, prob = c(0.4, 0.6)) == TRUE
  out2[f2 ,j] = data$rperfect[f2]
  out2[!f2, j] = data$rnever[!f2]
}

ravg2=colMeans(out2)
Sr2=colMeans(out2-data$rf %*% matrix(1,1,N))/apply(out2-data$rf %*% matrix(1,1,N), 2, sd)
#setEPS()
#postscript("hwk3fig1.eps")
#par(mfrow = c(2,1))
#hist(ravg2,20, xlab = 'Avg returns, 60% accuracy', 
#     col = 'gray16', main = '', ylab = '')
#lines(c(mean(data$rm), mean(data$rm)),c(0, 4000), lwd=2, col = 'red')
#hist(Sr2,20, xlab = 'Sharpe ratios, 60% accuracy', 
#     main = '', ylab = '', col = 'gray16')
#lines(c(Sm, Sm),c(0, 4000), lwd=2, col = 'red')
#dev.off()

mean(ravg2)
mean(Sr2)
mean(data$rm)
Sm

###########################################################

acc_lev = seq(0.25, 0.95, 0.01)
acc.len = length(acc_lev)

exp_ret = rep(NA, acc.len)
sha_rat = rep(NA, acc.len)
sha_rat1 = rep(NA, acc.len)

v = matrix(runif(T*N), nrow = T)

r1=data$rperfect %*% matrix(1,1,N)
r0=data$rnever %*% matrix(1,1,N)
rf1=data$rf %*% matrix(1,1,N)

for(j in 1:acc.len) {
  r11=r1
  f = v>=acc_lev[j]
  r11[f] = r0[f]
  exp_ret[j] = mean(r11)
  sha_rat[j] = mean(colMeans(r11-rf1)/apply(r11-rf1, 2, sd))
  sha_rat1[j] = mean(colMeans(r11-rf1-c)/apply(r11-rf1, 2, sd))
}

#setEPS()
#postscript("hwk3fig2.eps")
#par(mfrow = c(2,1))
#plot(acc_lev,exp_ret, type = 'l', main = '', ylab = '', 
#     xlab = 'Accuracy vs. expected returns, no fees')
#lines(acc_lev, rep(mean(data$rm), acc.len), lty = 3, col = 'red')
#plot(acc_lev,sha_rat, type = 'l', main = '', ylab = '', 
#     xlab = 'Accuracy vs. expected Sharpe ratios, no fees')
#lines(acc_lev, rep(Sm, acc.len), lty = 3, col = 'red')
#dev.off()

#setEPS()
#postscript("hwk3fig3.eps")
#par(mfrow = c(2,1))
#plot(acc_lev,exp_ret-c, type = 'l', main = '', ylab = '', 
#     xlab = paste('Accuracy vs. expected returns, fees of', c))
#lines(acc_lev, rep(mean(data$rm), acc.len), lty = 3, col = 'red')
#plot(acc_lev,sha_rat1, type = 'l', main = '', ylab = '', 
#     xlab = paste('Accuracy vs. expected Sharpe ratios, no fees', c))
#lines(acc_lev, rep(Sm, acc.len), lty = 3, col = 'red')
#dev.off()

