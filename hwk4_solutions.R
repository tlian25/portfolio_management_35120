# This program generates solutions for the fourth homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)

begdate = as.Date('19730131', '%Y%m%d')
begdate_oos = as.Date('19780131', '%Y%m%d')
b = c(0.6, 0.7, 1.2, 0.9, 1.2)
ERm = 0.005
p = 0.5
com.names = c('Exxon', 'PG', 'Pfizer', 'Walmart', 'Intel')
N = length(com.names)

stock.data = read.csv('data/STOCK_RETS.csv')
names(stock.data) = c('date', com.names)
tbill.data = read.csv('data/TB_7301_1912.csv')
names(tbill.data) = c('date', 'rf')
data = merge(stock.data, tbill.data, by = 'date')
data$date = as.Date(as.character(data$date), '%Y%m%d')

r = data[com.names]
re = r - data$rf %*% matrix(1, 1, N)
ER1 = colMeans(re)
V1 = cov(r)
w1 = (solve(V1) %*% ER1)/sum(solve(V1) %*% ER1)

w0 = (solve(V1) %*% matrix(1,N,1))/(matrix(1,1,N) %*% solve(V1) %*% matrix(1,N,1))[1]

E_1 = t(w1) %*% ER1
E_0 = t(w0) %*% ER1
V_1 = t(w1) %*% V1 %*% w1
V_0 = t(w0) %*% V1 %*% w0

ER11 = round(ER1*100)/100
w11 = (solve(V1) %*% ER11)/sum(solve(V1) %*% ER11)

w3 = ER1/sum(ER1)
w31 = ER11/sum(ER11)

ER2 = b * ERm
w2 = (solve(V1) %*% ER2)/sum(solve(V1) %*% ER2)

ER3 = p*ER1+(1-p)*ER2
avgsig2 = mean(diag(V1))
V3 = p*V1+(1-p)*avgsig2*diag(rep(1, nrow(V1)))
w4 = solve(V3) %*% ER3/sum(solve(V3) %*% ER3)

f1 = which(data$date >= begdate_oos & as.numeric(format(data$date, '%m')) == 1)
d = data$date[f1[1]:nrow(data)]

colnm = c('ret', 'exret')
rownm = format(d, '%Y-%m-%d')
out1 = as.data.frame(matrix(NA, length(d), 2, dimnames = list(rownm, colnm)))
out2 = as.data.frame(matrix(NA, length(d), 2, dimnames = list(rownm, colnm)))
out3 = as.data.frame(matrix(NA, length(d), 2, dimnames = list(rownm, colnm)))
out4 = as.data.frame(matrix(NA, length(d), 2, dimnames = list(rownm, colnm)))

for(tt in 1:length(f1)) {           
    
  r1 = r[1:(f1[tt]-1), ]   
  re1 = re[1:(f1[tt]-1), ] 
  
  ER1o = colMeans(re1)
  ER2o = ER2
  ER3o = p*ER1o+(1-p)*ER2o
  V1o = cov(r1)
  avgsig2 = mean(diag(V1o))
  V3o = p*V1o+(1-p)*avgsig2*diag(rep(1, nrow(V1o)))
  
  w1o = solve(V1o) %*% ER1o/sum(solve(V1o) %*% ER1o)
  w2o = solve(V1o) %*% ER2o/sum(solve(V1o) %*% ER2o)
  w3o = as.matrix(ER1o/sum(ER1o))
  w4o = solve(V3o) %*% ER3o/sum(solve(V3o) %*% ER3o)
  
  r2 = as.matrix(r[f1[tt]:(f1[tt]+11),])
  re2 = as.matrix(re[f1[tt]:(f1[tt]+11),])
  out1[(12*tt-11):(12*tt), ] = cbind(r2 %*% w1o, re2 %*% w1o)
  out2[(12*tt-11):(12*tt), ] = cbind(r2 %*% w2o, re2 %*% w2o)
  out3[(12*tt-11):(12*tt), ] = cbind(r2 %*% w3o, re2 %*% w3o)
  out4[(12*tt-11):(12*tt), ] = cbind(r2 %*% w4o, re2 %*% w4o)
}

we1 = cumprod(out1$ret+1)-1
we2 = cumprod(out2$ret+1)-1
we3 = cumprod(out3$ret+1)-1
we4 = cumprod(out4$ret+1)-1
#maxval = max(c(we1, we2, we3, we4))
#setEPS()
#postscript("fig_hw4.eps")
#plot(d, we1, type = 'l',
#     xlab = 'Year', ylab = 'Cumulative Return',
#     ylim = c(0, maxval), lty = 3)
#lines(d, we2, lty = 4)
#lines(d, we3, lty = 2)
#lines(d, we4, lty = 1)
#legend(x = 'topleft', legend = c('Ehat+Vhat','E(CAPM)+Vhat','Ehat+Identity','Bayesian'),
#       lty = c(3, 4, 2, 1))
#dev.off()

avgrets = c(mean(out1$ret), mean(out2$ret), mean(out3$ret), mean(out4$ret))
stdrets = c(sd(out1$ret), sd(out2$ret), sd(out3$ret), sd(out4$ret))
sharpes = c(mean(out1$exret)/sd(out1$ret), mean(out2$exret)/sd(out2$ret), 
            mean(out3$exret)/sd(out3$ret), mean(out4$exret)/sd(out4$ret))
