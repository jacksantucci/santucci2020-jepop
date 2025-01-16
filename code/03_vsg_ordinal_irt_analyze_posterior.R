##################################################
# PACKAGES AND DATA
##################################################

library(MCMCpack)

library(coda)

load('../output/respondent_attributes.Rdata')

load("../output/vsg_items_recoded.Rdata")

load("../output/bayes_vsg_2012_1d.Rdata")

load("../output/bayes_vsg_2016_1d.Rdata")

##################################################
# NEW VARIABLE NAMES
##################################################

newnames <- c("Abortion", "Immigrants make contribution", "Immigrant legalization path", "Ease of immigration", "Gay marriage", "Death penalty", "Death penalty frequency", "Universal healthcare", "Health reform bill", "Global warming denial", "Humans cause warming", "Affirmative action", "Higher taxes >$200k", "Regulation of business", "Foreign trade", "Blacks have gotten less", "Other minorities overcame", "Blacks should try harder", "Generations of slavery")

##################################################
# GEWEKE TESTS
##################################################

gw.tests.2012 <- geweke.diag(vsg12.1d)
gw.tests.2016 <- geweke.diag(vsg16.1d)

test12 <- prop.table(table(abs(gw.tests.2012$z)<1.96))
test16 <- prop.table(table(abs(gw.tests.2016$z)<1.96))

save(gw.tests.2012, gw.tests.2016, test12, test16, file='../output/geweke_tests.Rdata')

print(test12)
print(test16)

##################################################
# EXTRACT PARMETERS
##################################################

summary12 <- summary(vsg12.1d)
summary16 <- summary(vsg16.1d)

##### Difficulty

getDifficulty <- function(result.summary){
	lams <- result.summary$statistics[grep('Lambda', rownames(result.summary$statistics)),]
	diff <- lams[grep('.1$', rownames(lams)),]
	diff.mean <- diff[,1]
	return(diff.mean)
}

getDifficultyCI <- function(result.summary){
	lams <- result.summary$quantiles[grep('Lambda', rownames(result.summary$quantiles)),]
	diff <- lams[grep('.1$', rownames(lams)),]
	return(diff)
}

diff12 <- getDifficulty(vsg12.1d)
diff16 <- getDifficulty(vsg16.1d)

diff12cis <- getDifficultyCI(vsg12.1d)
diff16cis <- getDifficultyCI(vsg16.1d)

##### Discrimination

getDiscrim1 <- function(result.summary){
	lams <- result.summary$statistics[grep('Lambda', rownames(result.summary$statistics)),]
	discrim1 <- lams[grep('.2$', rownames(lams)),]
	discrim.mean <- discrim1[,1]
	return(discrim.mean)
}

getDiscrim1CI <- function(result.summary){
	lams <- result.summary$quantiles[grep('Lambda', rownames(result.summary$quantiles)),]
	discrim1 <- lams[grep('.2$', rownames(lams)),]
	return(discrim1)
}

discrim12 <- getDiscrim1(summary12)
discrim16 <- getDiscrim1(summary16)

discrim12cis <- getDiscrim1CI(summary12)
discrim16cis <- getDiscrim1CI(summary16)

discrim1 <- cbind.data.frame(newnames, discrim12, discrim16, "lo12"=discrim12cis[,'2.5%'], "lo16"=discrim16cis[,'2.5%'], "hi12"=discrim12cis[,5], "hi16"=discrim16cis[,'97.5%'], stringsAsFactors=F)

##### Ideal points/factor scores

getXbar1 <- function(result.summary){
	phis <- result.summary$statistics[grep('phi', rownames(result.summary$statistics)),]
	phimat <- phis[grep('.2$', rownames(phis)),]
	phibar <- phimat[,1]
	return(phibar)
}

getXbar1CI <- function(result.summary){
	phicis <- result.summary$quantiles[grep('phi', rownames(result.summary$quantiles)),]
	phicis <- phicis[grep('.2$', rownames(phicis)),]
	return(phicis)
}

x12 <- getXbar1(summary12)

x16 <- getXbar1(summary16)

cor(x12, attribs$ideo5_baseline, "complete.obs")
cor(x16, attribs$ideo5_2016, "complete.obs")

cor(x12, attribs$pid7_baseline, "complete.obs")
cor(x16, attribs$pid7_2016, "complete.obs")

##################################################
# SIGNIFICANCE TESTS AND PLOTTING: DISCRIMINATION PARAMETERS
##################################################

##### Discrimination parameters

increasedp <- discrim1$discrim16>discrim1$discrim12

sigdp <- rep(NA, length(increasedp))

sigdp[which(increasedp==T & discrim1$lo16 > discrim1$hi12)] <- 1
sigdp[which(increasedp==T & discrim1$lo16 < discrim1$hi12)] <- 0
sigdp[which(increasedp==F & discrim1$hi16 < discrim1$lo12)] <- 1
sigdp[which(increasedp==F & discrim1$hi16 > discrim1$lo12)] <- 0

updown <- rep(NA, length(increasedp)) # for tables of t-tests at the end
updown[sigdp==1 & increasedp==1] <- "Up"
updown[sigdp==1 & increasedp==0] <- "Down"
updown[sigdp==0] <- "No change"

discrim1$increasedp <- increasedp

discrim1$sigdp <- sigdp

discrim1 <- discrim1[order(discrim1[,'discrim12']),]

yidx <- 1:nrow(discrim1)

yidx12 <- yidx+0.05
yidx16 <- yidx-0.05

signames <- ifelse(discrim1$sigdp==0, discrim1$newnames, paste0('*', discrim1$newnames))

pdf('../graphics/discrim_params_1d.pdf', width=5, height=8)
par(mar=c(4.1, 11.6, 0.1, 0.5))
plot(1,1, pch=NA, xlim=c(min(discrim1[,4:7]), max(discrim1[,4:7])), ylim=c(1, max(yidx)), axes=F, ylab='', xlab='Discrimination parameter\n(*change is sig. with 95% confidence)')
# points(discrim1$discrim04, yidx04, col="darkgray")
segments(x0=discrim1$lo12, x1=discrim1$hi12, y0=yidx12, col="darkgray", lwd=2)
# points(discrim1$discrim16, yidx16)
segments(x0=discrim1$lo16, x1=discrim1$hi16, y0=yidx16, lwd=2)
mtext(text=signames, side=2, las=2, at=yidx, cex=1, font=discrim1$varfont)
abline(v=median(discrim1$discrim16), lty=3)
axis(1)
legend("bottomright", bty='n', legend=c("2012", "2016"), lty=c(1,1), lwd=c(2,2), col=c("darkgray", "black"))
dev.off()

##################################################
# TABLE OF DISCRIMINATION PARAMETERS
##################################################

discrim.table <- cbind.data.frame("Item"=discrim1$newnames, "Estimate 2012"=round(discrim1$discrim12, 2), "Range 2012"=paste0('[', round(discrim1$lo12, 2), ', ', round(discrim1$hi12, 2), ']'), "Estimate 2016"=round(discrim1$discrim16, 2), "Range 2016"=paste0('[', round(discrim1$lo16, 2), ', ', round(discrim1$hi16, 2), ']'))

library(xtable)

print(xtable(discrim.table), include.rownames=F, type="html", html.table.attributes=c('border=0'), file="../output/discrim_table.html")