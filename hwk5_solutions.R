# This program generates solutions for the fifth homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)

raus = read.table('data/australia.txt', skip = 2, header = TRUE)
rfra = read.table('data/france.txt', skip = 2, header = TRUE)
rger = read.table('data/germany.txt', skip = 2, header = TRUE)
rita = read.table('data/italy.txt', skip = 2, header = TRUE)
rjap = read.table('data/japan.txt', skip = 2, header = TRUE)
rbri = read.table('data/uk.txt', skip = 2, header = TRUE)
years = as.numeric(substr(as.character(rbri$X.), 1, 4))
months = as.numeric(substr(as.character(rbri$X.), 5, 6))
dates = as.Date(paste0(as.character(rbri$X.), '28'), '%Y%m%d')
t.len = length(months)
int.hml = as.data.frame(cbind(raus$High - raus$Low, rfra$High - rfra$Low, rger$High - rger$Low,
                rita$High - rita$Low, rjap$High - rjap$Low, rbri$High - rbri$Low))/100
countries = c('AUS', 'FRA', 'GER', 'ITA', 'JAP', 'UK')
names(int.hml) = countries
m.int.hml = colMeans(int.hml)*100
s.int.hml = apply(int.hml, 2, sd)*100
t.int.hml = sqrt(t.len)*m.int.hml / s.int.hml
W = 60
mov.avg = as.data.frame(matrix(NA, nrow(int.hml), ncol(int.hml), 
                               dimnames = list(1:t.len, countries)))
for (t in (1+W):t.len) mov.avg[t,] = colMeans(int.hml[(t-W):t,])
mov.avg$dates = dates
mov.avg = mov.avg[is.finite(mov.avg$AUS),]




setEPS()
postscript("int_movavg.eps")
par(mfrow = c(3,2))
for (j in 1:6) {
  plot(mov.avg$dates, mov.avg[,j], type = 'l',  
       xlab = '', ylab = '', main = countries[j],
       ylim = c(-0.015, 0.025))
  lines(mov.avg$dates, rep(0, nrow(mov.avg)))
}
dev.off()




int.hml$date = dates
ff.factors = read.table('data/ff_factors_192607_201912.txt', header = TRUE)
names(ff.factors) = c('date', 'mktrf', 'smb', 'hml', 'rf')
ff.factors$date = as.Date(paste0(as.character(ff.factors$date), '28'), '%Y%m%d')
for (varname in c('mktrf', 'smb', 'hml', 'rf')) ff.factors[varname] = ff.factors[varname]/100
usa.hml = ff.factors[c('date', 'hml')]
names(usa.hml) = c('date', 'USA')
assets1 = merge(usa.hml, int.hml, by = 'date')
all.assets = merge(ff.factors, int.hml, by = 'date')
cor(assets1[,2:8])
V1 = cov(assets1[,2:8])
w1 = solve(V1) %*% matrix(1,7,1)/(matrix(1,1,7) %*% solve(V1) %*% matrix(1,7,1))[1]
SP1 = sqrt(t(w1) %*% V1 %*% w1)[1]
std_hml = sd(all.assets$hml)
gamma = mean(all.assets$mkt)/var(all.assets$mkt)
assets2 = all.assets[c('mktrf', 'hml')]
E2 = colMeans(assets2)
V2 = cov(assets2)
w2 = solve(V2) %*% E2/gamma
c(as.numeric(w2), 1 - sum(w2))
SR_mkt = mean(all.assets$mkt)/sd(all.assets$mkt)
SR_mkt_hml = t(w2) %*% E2 / sqrt(t(w2) %*% V2 %*% w2)
inf.lm = lm(hml ~ mktrf, data = all.assets)
alpha = inf.lm$coefficients[1]
se = sd(inf.lm$residuals)
IR = alpha / se
assets3 = all.assets[c('mktrf', 'hml', countries)]
E3 = colMeans(assets3)
V3 = cov(assets3)
w3 = solve(V3) %*% E3 / gamma
SR_all = t(w3) %*% E3 / sqrt(t(w3) %*% V3 %*% w3)[1]
c(as.numeric(w3), 1 - sum(w3))


