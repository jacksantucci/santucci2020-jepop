load('../output/vsg_items_recoded.Rdata')

########################################
# ORDINAL 2PL MODELS
########################################

library(MCMCpack)

vsg2012 <- as.matrix(subset(items, select=vars2012))
# keep2012 <- which(rowSums(vsg2012, na.rm=T)>0)
# vsg2012 <- vsg2012[keep2012,]

vsg2016 <- as.matrix(subset(items, select=vars2016))
# keep2016 <- which(rowSums(vsg2016, na.rm=T)>0)
# vsg2016 <- vsg2016[keep2016,]

vsg12.1d <- MCMCordfactanal(vsg2012, factors=1, lambda.constraints=list(univhealthcov_baseline=list(2, "+")), burnin=1e5, mcmc=5e5, thin=100, l0=0, L0=0.001, store.lambda=T, store.scores=T, tune=1.2, seed=1776)

save(vsg12.1d, file='../output/bayes_vsg_2012_1d.Rdata')

rm(vsg12.1d)

vsg16.1d <- MCMCordfactanal(vsg2016, factors=1, lambda.constraints=list(univhealthcov_2016=list(2, "+")), burnin=1e5, mcmc=5e5, thin=100, l0=0, L0=0.001, store.lambda=T, store.scores=T, tune=1.2, seed=1776)

save(vsg16.1d, file='../output/bayes_vsg_2016_1d.Rdata')

rm(vsg16.1d)

q(save="no")