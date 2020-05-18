# This program generates solutions for the seventh homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)

### function used to adjust y limits on plot
adjust_ylim = function(y1, y2, lower.mult, upper.mult) {
  lower = min(c(y1, y2))
  upper = max(c(y1, y2))
  return(c(lower.mult * lower, upper.mult * upper))
}

tickers = c('SPY', 'SH', 'SSO', 'SDS', 'IYF', 'SEF', 'UYG', 'SKF', 'IYR', 'REK', 'URE',
            'SRS', 'IWM', 'RWM', 'UWM', 'TWM')
data = read.table('LETF.txt', comment.char = '%')
names(data) = c('date', tickers)
spret = read.table('SP500.txt', comment.char = '%')
names(spret) = c('date', 'SP500')
data = merge(data, spret, by = 'date', all = TRUE)
data$date = as.Date(as.character(data$date), '%Y%m%d')
for (var in names(data)) data[var][data[var] == -99] = NA

#setEPS()
#postscript("h7_Fig_1.eps")
#plot(data$date, cumprod(data$SP500 + 1), lty = 1, type = 'l', col = 'blue',
#     main = 'Cumulative Returns from S&P500 index and 1xETF (SPY)',
#     ylab = '', xlab = '')
#lines(data$date, cumprod(data$SPY + 1), lty = 2, col = 'red')
#legend('bottomright', legend = c('S&P500 Index','S&P500 1x ETF'), lty = 1:2, col = c('blue', 'red'))
#dev.off()

index.names = c('S&P 500', 'DJUSFN', 'DJUSRE', 'Russell 2000')
names.1x = tickers[1:length(tickers) %% 4 == 1]
names.n1x = tickers[1:length(tickers) %% 4 == 2]
names.2x = tickers[1:length(tickers) %% 4 == 3]
names.n2x = tickers[1:length(tickers) %% 4 == 0]
#setEPS()
#postscript("h7_Fig_2a.eps")
#par(mfrow = c(2,2), oma = c(0, 0, 2, 0))
for(i in 1:4) {
  subsample = is.finite(unlist(data[names.1x[i]])) & is.finite(unlist(data[names.2x[i]]))
  subdata = data[subsample, ]
#  plot(subdata$date, cumprod(2*unlist(subdata[names.1x[i]])+1), 
#       lty = 1, type = 'l',
#       main = index.names[i],
#       ylab = '', xlab = '', col = 'blue')
#  lines(subdata$date, cumprod(unlist(subdata[names.2x[i]])+1), lty = 2, col = 'red')
#  legend('top', legend = c('2 times 1xETF','2xLETF'), lty = 1:2, col = c('blue', 'red'))
}
#mtext('2 times 1xETF vs. 2xLETF cum. return', outer = TRUE, cex = 1.5)
#dev.off()

#setEPS()
#postscript("h7_Fig_2b_i.eps")
#par(mfrow = c(2,2), oma = c(0, 0, 2, 0))
for(i in 1:4) {
  subsample = is.finite(unlist(data[names.1x[i]])) & is.finite(unlist(data[names.n1x[i]]))
  subdata = data[subsample, ]
#  plot(subdata$date, cumprod(unlist(subdata[names.1x[i]])+1), 
#       lty = 1, type = 'l',
#       main = index.names[i],
#       ylab = '', xlab = '', col = 'blue',
#       ylim = adjust_ylim(cumprod(unlist(subdata[names.1x[i]])+1), cumprod(-1*unlist(subdata[names.n1x[i]])+1), 0.99, 1.1))
#  lines(subdata$date, cumprod(-1*unlist(subdata[names.n1x[i]])+1), lty = 2, col = 'red')
#  legend('topleft', legend = c('1xETF','negative of -1xETF'), lty = 1:2, col = c('blue', 'red'))
}
#mtext('1xETF vs. neg. of -1xETF cum. return', outer = TRUE, cex = 1.5)
#dev.off()

#postscript("h7_Fig_2b_ii.eps")
#par(mfrow = c(2,2), oma = c(0, 0, 2, 0))
#legend.place = c('topleft', 'top', 'top', 'topleft')
for(i in 1:4) {
  subsample = is.finite(unlist(data[names.2x[i]])) & is.finite(unlist(data[names.n2x[i]]))
  subdata = data[subsample, ]
#  plot(subdata$date, cumprod(unlist(subdata[names.2x[i]])+1), 
#       lty = 1, type = 'l',
#       main = index.names[i],
#       ylab = '', xlab = '', col = 'blue',
#       ylim = adjust_ylim(cumprod(unlist(subdata[names.2x[i]])+1), cumprod(-1*unlist(subdata[names.n2x[i]])+1), 0.99, 1.15))
#  lines(subdata$date, cumprod(-1*unlist(subdata[names.n2x[i]])+1), lty = 2, col = 'red')
#  legend(legend.place[i], legend = c('2xLETF','negative of -2xLETF'), lty = 1:2, col = c('blue', 'red'))
}
#mtext('2xLETF vs. neg. of -2xLETF cum. return', outer = TRUE, cex = 1.5)
#dev.off()

#postscript("h7_Fig_2c.eps")
subsample = data$date >= as.Date('May31, 2007', '%b%d, %Y') & data$date <= as.Date('May31, 2010', '%b%d, %Y')
subdata = data[subsample, ]
#plot(subdata$date, cumprod(unlist(subdata[names.1x[4]])+1), 
#     lty = 1, type = 'l',
#     main = '1xETF vs. 2xLETF vs. -2xLETF cumulative return ~ Russell 2000',
#     ylab = '', xlab = '', ylim = c(0, 3), col = 'blue')
#lines(subdata$date, cumprod(unlist(subdata[names.2x[4]])+1), lty = 2, col = 'red')
#lines(subdata$date, cumprod(unlist(subdata[names.n2x[4]])+1), lty = 4, col = 'darkorange')
#legend('topright', legend = c('1xETF','2xLETF','-2xLETF'), lty = c(1:2, 4), col = c('blue', 'red', 'darkorange'))
#dev.off()

#postscript("h7_Fig_3.eps")
#par(mfrow = c(2,2), oma = c(0, 0, 2, 0))
for(i in 1:4) {
  subsample = is.finite(unlist(data[names.2x[i]])) & is.finite(unlist(data[names.n2x[i]]))
  subdata = data[subsample, ]
#  plot(subdata$date, 0.5*cumprod(unlist(subdata[names.2x[i]])+1) + 
#         0.5*cumprod(unlist(subdata[names.n2x[i]])+1), 
#       lty = 1, type = 'l',
#       main = index.names[i],
#       ylab = '', xlab = '', col = 'blue')
}
#mtext('2xLETF Paired with -2xLETF cum. return', outer = TRUE, cex = 1.5)
#dev.off()




