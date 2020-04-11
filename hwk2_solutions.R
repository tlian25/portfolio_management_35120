# This program generates solutions for the second homework problem for
# Portfolio Management. For this program to run properly, the data files
# need to be in the same directory as this program! Alternatively,
# you can change the current working directory to the directory of the
# data files using the setwd() command.

### this bizarre option represses scientific notation
# options(scipen=999) represses it completely
options(scipen=10)
rd = read.table('data/returns_daily.txt', comment.char = '%')
rm = read.table('data/returns_monthly.txt', comment.char = '%')
ra = read.table('data/returns_annual.txt', comment.char = '%')
col.names = c('date', 'mktret', 'bondret')
names(rd) = col.names
names(rm) = col.names
names(ra) = col.names
name.vec = c('Daily', 'Monthly', 'Annual')
asset.names = c('stock', 'bond')
data.list = list(rd, rm, ra)
means = as.data.frame(matrix(NA, 3, 2, dimnames = list(name.vec, asset.names)))
stds = as.data.frame(matrix(NA, 3, 2, dimnames = list(name.vec, asset.names)))
cvds = as.data.frame(matrix(NA, 3, 1, dimnames = list(name.vec)))
for(i in 1:3) {
  data = data.list[[i]]
  means$stock[i] = mean(data$mktret)
  means$bond[i] = mean(data$bondret)
  stds$stock[i] = sd(data$mktret)
  stds$bond[i] = sd(data$bondret)
  cvds[i, ] = cov(data$mktret, data$bondret)
}
nsim = 10000
out = as.data.frame(matrix(NA, nsim, 6))
names(out) = apply(expand.grid(asset.names, tolower(name.vec)), 1, function(x) paste(x, collapse="."))
for(i in 1:3) {
  out[,2*i-1] = rnorm(nsim, means$stock[i], stds$stock[i])
  out[,2*i] = rnorm(nsim, means$bond[i], stds$bond[i])
}
k = c(-0.2, -0.1, 0, 0.1, 0.2)
sprob.col.names = paste0(as.character(100*k), '%')
shortfall.probs = as.data.frame(matrix(NA, 6, length(k), dimnames = list(names(out), sprob.col.names)))
for (j in 1:6) {
  for (jj in 1:length(k)) {
    shortfall.probs[j, jj] = mean(out[,j] < k[jj])
  }
}
setEPS()
postscript("hwk2_fig1.eps")
par(mfrow = c(3,2))
for (j in 1:6) {
  plot(k, shortfall.probs[j,], type = 'o', pch = 8, 
       xlab = gsub('[.]', ' ', rownames(shortfall.probs)[j]),
       ylab = '', main = '')
}
dev.off()
shortfall.probs
for(i in 1:3) {
  data = data.list[[i]]
  out[,2*i-1] = sample(data$mktret, nsim, replace = TRUE)
  out[,2*i] = sample(data$bondret, nsim, replace = TRUE)
}
for (j in 1:6) {
  for (jj in 1:length(k)) {
    shortfall.probs[j, jj] = mean(out[,j] < k[jj])
  }
}
setEPS()
postscript("hwk2_fig2.eps")
par(mfrow = c(3,2))
for (j in 1:6) {
  plot(k, shortfall.probs[j,], type = 'o', pch = 8, 
       xlab = gsub('[.]', ' ', rownames(shortfall.probs)[j]),
       ylab = '', main = '')
}
dev.off()
shortfall.probs
analytical.prob = as.data.frame(matrix(NA, 3, 2, dimnames = list(name.vec, asset.names)))
ndsim.prob = as.data.frame(matrix(NA, 3, 2, dimnames = list(name.vec, asset.names)))
resample.prob = as.data.frame(matrix(NA, 3, 2, dimnames = list(name.vec, asset.names)))
period = 5
cutoff.ret = 0.2
for(i in 1:3) {
  data = data.list[[i]]
  means$stock[i] = mean(log(1 + data$mktret))
  means$bond[i] = mean(log(1 + data$bondret))
  stds$stock[i] = sd(log(1 + data$mktret))
  stds$bond[i] = sd(log(1 + data$bondret))
}
for(i in 1:3) {
  analytical.prob$stock[i] = pnorm(log(1 + cutoff.ret), period*means$stock[i], 
                                   sqrt(period)*stds$stock[i], lower.tail = FALSE)
  analytical.prob$bond[i] = pnorm(log(1 + cutoff.ret), period*means$bond[i], 
                                  sqrt(period)*stds$bond[i], lower.tail = FALSE)
}
for(i in 1:3) {
  data = data.list[[i]]
  stock.sim = matrix(rnorm(nsim*period, means$stock[i], stds$stock[i]), nrow = nsim)
  bond.sim = matrix(rnorm(nsim*period, means$bond[i], stds$bond[i]), nrow = nsim)
  stock.sim = exp(apply(stock.sim, 1, sum))
  bond.sim = exp(apply(bond.sim, 1, sum))
  ndsim.prob$stock[i] = mean(stock.sim > 1 + cutoff.ret)
  ndsim.prob$bond[i] = mean(bond.sim > 1 + cutoff.ret)
}
for(i in 1:3) {
  data = data.list[[i]]
  stock.sim = matrix(sample(data$mktret, nsim*period, replace = TRUE), nrow = nsim)+1
  bond.sim = matrix(sample(data$bondret, nsim*period, replace = TRUE), nrow = nsim)+1
  stock.sim = apply(stock.sim, 1, prod)
  bond.sim = apply(bond.sim, 1, prod)
  resample.prob$stock[i] = mean(stock.sim > 1 + cutoff.ret)
  resample.prob$bond[i] = mean(bond.sim > 1 + cutoff.ret)
}
analytical.prob
ndsim.prob
resample.prob
nms = apply(expand.grid(asset.names, tolower(name.vec)), 1, function(x) paste(x, collapse="."))
results = as.data.frame(matrix(NA, 6, 3, dimnames = list(nms, c('analytical', 'simulation', 'resample'))))
res.list = list(analytical.prob, ndsim.prob, resample.prob)
for(i in 1:3) {
  for(j in 1:3) {
    res = res.list[[j]]
    results[2*i-1, j] = res$stock[i]
    results[2*i, j] = res$bond[i]
  }
}
results
means = as.data.frame(matrix(NA, 3, 1, dimnames = list(name.vec, 'diff')))
stds = as.data.frame(matrix(NA, 3, 1, dimnames = list(name.vec, 'diff')))
analytical.prob = as.data.frame(matrix(NA, 3, 1, dimnames = list(name.vec, 'prob')))
resample.prob = as.data.frame(matrix(NA, 3, 1, dimnames = list(name.vec, 'prob')))
period = 30
for(i in 1:3) {
  data = data.list[[i]]
  means[i,] = mean(log(1 + data$mktret) - log(1 + data$bondret))
  stds[i,] = sd(log(1 + data$mktret) - log(1 + data$bondret))
}
for(i in 1:3) {
  analytical.prob[i,] = pnorm(0, period*means[i,], 
                                   sqrt(period)*stds[i,], lower.tail = TRUE)
}
for(i in 1:3) {
  data = data.list[[i]]
  sample.indices = sample(1:nrow(data), nsim*period, replace = TRUE)
  stock.sim = matrix(data$mktret[sample.indices], nrow = nsim)+1
  bond.sim = matrix(data$bondret[sample.indices], nrow = nsim)+1
  stock.sim = apply(stock.sim, 1, prod)
  bond.sim = apply(bond.sim, 1, prod)
  resample.prob[i,] = mean(stock.sim < bond.sim)
}
analytical.prob
resample.prob

