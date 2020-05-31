# Modify this program as necessary to generate solutions for 
# Assignment 10 for Portfolio Management. 

beg_date = as.Date('19940301', '%Y%m%d')
beg_capital = 50
moneyness = 0.8
target_ret = 0.04
tb_ret = 0.0001
#data = read.table('sp500_daily.txt', comment.char = '%')
data = read.csv("data/sp500_daily.csv")
names(data) = c('date', 'sp_ret', 'sp_prices')
data$date = as.Date(as.character(data$date), '%Y%m%d')
sigma = sd(data$sp_ret)
last_date = data$date[nrow(data)]

data$month = as.numeric(format(data$date, '%m'))
data$year = as.numeric(format(data$date, '%Y'))
data$last_day_of_month = FALSE
data$first_day_of_year = FALSE
data$last_day_of_month[nrow(data)] = TRUE
data$first_day_of_year[1] = TRUE
for (i in 2:nrow(data)) {
  if (data$month[i] != data$month[i-1]) 
    data$last_day_of_month[i-1] = TRUE
  if (data$year[i] != data$year[i-1])
    data$first_day_of_year[i] = TRUE
}
##### What happens on the day the hedge fund is started #####
idate = min(which(data$date >= beg_date))
S = data$sp_prices[idate]    
K = S*moneyness
Tm = 60
d1 = ( log(S/K) + (tb_ret + (sigma^2)/2)*Tm ) / (sigma*sqrt(Tm))
d2 = d1 - sigma*sqrt(Tm)    
bs_call = S*pnorm(d1) - K*exp(-Tm*tb_ret)*pnorm(d2)    # Black-Scholes
bs_put = bs_call + K*exp(-Tm*tb_ret) - S                     # Put-call parity
capital = beg_capital
N = target_ret*capital/bs_put
capital = capital*(1+target_ret)
bs_put_old = bs_put
###### What happens on each following day #####
capitals_d = capital
capitals_m = capital
compens_m = 0
sp_m = S
ret_m = NULL
ret_sp_m = NULL
thedate = data$date[idate]
while(capital>0 & thedate<=last_date) {    
    idate = idate + 1
    if(idate > nrow(data)) break
    thedate = data$date[idate]   
    S = data$sp_prices[idate]     
    capital = capital*(1+tb_ret)
    if(data$last_day_of_month[idate]) { 
        # Buy back your one-month-old options to close out your position
        Tm = 30
        d1 = ( log(S/K) + (tb_ret + (sigma^2)/2)*Tm ) / (sigma*sqrt(Tm))
        d2 = d1 - sigma*sqrt(Tm) 
        bs_call = S*pnorm(d1) - K*exp(-Tm*tb_ret)*pnorm(d2) 
        bs_put_new = bs_call + K*exp(-Tm*tb_ret) - S                 
        put_cost = N*bs_put_new
        capital = capital - put_cost       
        # Write new options
        K = S*moneyness
        Tm = 60
        d1 = ( log(S/K) + (tb_ret + (sigma^2)/2)*Tm ) / (sigma*sqrt(Tm))
        d2 = d1 - sigma*sqrt(Tm)    
        bs_call = S*pnorm(d1) - K*exp(-Tm*tb_ret)*pnorm(d2)    
        bs_put = bs_call + K*exp(-Tm*tb_ret) - S                     
        N = target_ret*capital/bs_put
        capital = capital*(1+target_ret)
        if (capital>0) { 
            # Compute monthly returns, compensation
            old_cap = capitals_m[length(capitals_m)]
            new_cap = capital
            ret = (new_cap-old_cap)/old_cap
            compens = (0.02/12 + 0.2*max(ret-21*tb_ret,0))*old_cap
            ret_m = c(ret_m, ret)
            compens_m = c(compens_m, compens)    
            capital = capital - compens
            capitals_m = c(capitals_m, capital)
        
            old_sp = sp_m[length(sp_m)]
            new_sp = S
            ret = (new_sp-old_sp)/old_sp
            ret_sp_m = c(ret_sp_m, ret)
            sp_m = c(sp_m, S)
        }
    }
    capitals_d = c(capitals_d, capital)       
}
pos = sum(ret_m>0)/sum(ret_m>-Inf)
sr_riteput = mean(ret_m-21*tb_ret)/sd(ret_m)
sr_sp = mean(ret_sp_m-21*tb_ret)/sd(ret_sp_m)
tot_comp = sum(compens_m)
thedate = thedate
compens = compens
c(min(ret_m), max(ret_m))
end_date = thedate
alive = data$date[data$date >= beg_date & data$date <= end_date]
#setEPS()
#postscript(paste0('riteput_', format(beg_date, '%Y%m%d'),
#                  '_', 100*moneyness,'.eps'))
#par(mfrow = c(2,1))
#plot(alive, capitals_d, type = 'l', lty = 1,
#     main = 'Riteput\'s capital', ylim = c(-10, max(capitals_d)+20),
#     xlab = '', ylab = '')
#lines(alive, rep(0, length(alive)), lty = 2)
#plot(1:length(compens_m), compens_m, type = 'l',
#     main = 'Your monthly compensation ($ million)',
#     xlab = '', ylab = '')
#dev.off()



