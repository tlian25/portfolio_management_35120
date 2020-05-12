# This program generates solutions for the sixth homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)
maxyear = 2018

gen.dates = function(yb, mb, ye, me) {
  years = c(rep(yb, (12-mb+1)), rep((yb+1):(ye-1), each = 12), rep(ye, me))
  months = c(mb:12, rep(1:12, (ye-yb-1)), 1:me)
  months = as.character(months)
  months[nchar(months) == 1] = paste0('0', months[nchar(months) == 1])
  return(as.numeric(paste0(years, months)))
}

liq.names = c(paste0('dec', 1:10), 'dec10min1')
liq = read.table('liq.txt', comment.char = '%')
names(liq) = c('date', 'liq')
vix = read.table('vix.txt', comment.char = '%')
names(vix) = 'vix'
vix$date = gen.dates(1990, 1, maxyear, 12)
liq.fac = read.table('liq_vw_hist_deciles_1968_2018.txt', comment.char = '%')
names(liq.fac) = liq.names
liq.fac$date = gen.dates(1968, 1, maxyear, 12)
ff.fac = read.table('ff_factors_192607_201912.txt', comment.char = '%')
names(ff.fac) = c('date', 'mktrf', 'smb', 'hml', 'rf')
data = merge(liq, vix, by = 'date', all = TRUE)
data = merge(data, liq.fac, by = 'date', all = TRUE)
data = merge(data, ff.fac, by = 'date', all = TRUE)
data = data[data$date < (maxyear+1)*100,]

cor(data$liq, data$vix, use = 'complete.obs')
print(cor(data$liq, data$vix, use = 'complete.obs'))

cor(data$liq, data$mktrf, use = 'complete.obs')
cor(data$liq[data$mktrf < 0], data$mktrf[data$mktrf < 0], use = 'complete.obs')
cor(data$liq[data$mktrf >= 0], data$mktrf[data$mktrf >= 0], use = 'complete.obs')
print(cor(data$liq, data$mktrf, use = 'complete.obs'))
print(cor(data$liq[data$mktrf < 0], data$mktrf[data$mktrf < 0], use = 'complete.obs'))
print(cor(data$liq[data$mktrf >= 0], data$mktrf[data$mktrf >= 0], use = 'complete.obs'))

liq.ports = c(paste0('exdec', 1:10), 'dec10min1')
for(i in 1:10) data[liq.ports[i]] = 100*data[liq.names[i]] - data$rf
data$dec10min1 = 100*data$dec10min1

data = data[data$date > 196200,]
all.sample = as.data.frame((matrix(NA, 2, 11, 
                                dimnames = list(c('alpha', 't'), liq.ports))))
for(i in 1:length(liq.ports)) {
  formula = eval(parse(text = paste(liq.ports[i], '~ mktrf + smb + hml')))
  reg = lm(formula, data = data)
  all.sample[1, i] = reg$coefficients[1]*12
  all.sample[2, i] = summary(reg)$coefficients[1, 3]
}

oos.sample = as.data.frame((matrix(NA, 2, 11, 
                                   dimnames = list(c('alpha', 't'), liq.ports))))
for(i in 1:length(liq.ports)) {
  formula = eval(parse(text = paste(liq.ports[i], '~ mktrf + smb + hml')))
  reg = lm(formula, data = data, subset = data$date > 200000)
  oos.sample[1, i] = reg$coefficients[1]*12
  oos.sample[2, i] = summary(reg)$coefficients[1, 3]
}

data$liq = data$liq*100
all.liq = as.data.frame((matrix(NA, 2, 11, 
                                   dimnames = list(c('liq.beta', 't'), liq.ports))))
for(i in 1:length(liq.ports)) {
  formula = eval(parse(text = paste(liq.ports[i], '~ liq + mktrf + smb + hml')))
  reg = lm(formula, data = data)
  all.liq[1, i] = reg$coefficients[2]
  all.liq[2, i] = summary(reg)$coefficients[2, 3]
}

oos.liq = as.data.frame((matrix(NA, 2, 11, 
                                   dimnames = list(c('liq.beta', 't'), liq.ports))))
for(i in 1:length(liq.ports)) {
  formula = eval(parse(text = paste(liq.ports[i], '~ liq + mktrf + smb + hml')))
  reg = lm(formula, data = data, subset = data$date > 200000)
  oos.liq[1, i] = reg$coefficients[2]
  oos.liq[2, i] = summary(reg)$coefficients[2, 3]
}

setEPS()
postscript("fig_liq_alp_bet.eps")
par(mfrow = c(2,1))
plot(1:10, all.sample[1,1:10], lty = 1, type = 'l',
     main = 'Upward slope => Liquidity risk is priced',
     ylab = 'Fama-French alpha (% per year)',
     xlab = 'Portfolios sorted by historical liquidity beta (low to high)',
     ylim = c(-4, 4))
lines(1:10, oos.sample[1,1:10], lty = 2)
legend('bottomright', legend = c('196801-201812', '200001-201812'), lty = 1:2)
plot(1:10, all.liq[1,1:10], lty = 1, type = 'l',
     main = 'Upward slope => Historical liquidity betas predict future liquidity betas',
     ylab = 'Future liquidity beta',
     xlab = 'Portfolios sorted by historical liquidity beta (low to high)',
     ylim = c(-0.1, 0.1))
lines(1:10, oos.liq[1,1:10], lty = 2)
legend('bottomright', legend = c('196801-201812', '200001-201812'), lty = 1:2)
dev.off()
