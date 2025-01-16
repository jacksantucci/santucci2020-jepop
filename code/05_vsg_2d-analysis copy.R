load('../output/vsg_items_recoded.Rdata')

load('../output/respondent_attributes.Rdata')

vsg2012 <- as.matrix(subset(items, select=vars2012))
# keep2012 <- which(rowSums(vsg2012, na.rm=T)>0)
# vsg2012 <- vsg2012[keep2012,]

vsg2016 <- as.matrix(subset(items, select=vars2016))
# keep2016 <- which(rowSums(vsg2016, na.rm=T)>0)
# vsg2016 <- vsg2016[keep2016,]

### colors for 2012 and 2016 vote choice

attribs$col12 <- ifelse(attribs$post_presvote12_2012==1, "blue", ifelse(attribs$post_presvote12_2012==2, "red", "darkgray"))

attribs$col16 <- ifelse(attribs$presvote16post_2016==1, "blue", ifelse(attribs$presvote16post_2016==2, "red", "darkgray"))

attribs$vote12 <- ifelse(attribs$post_presvote12_2012==1, "D", ifelse(attribs$post_presvote12_2012==2, "R", "O"))

attribs$vote16 <- ifelse(attribs$presvote16post_2016==1, "D", ifelse(attribs$presvote16post_2016==2, "R", "O"))

########################################
# FACTOR ANALYSIS, MULTIPLE IMPUTATION
########################################

library(mice)

set.seed(1776)

mi2012 <- mice(vsg2012)

set.seed(1776)

mi2016 <- mice(vsg2016)

fa12 <- factanal(~., factors=2, data=as.data.frame(complete(mi2012)), scores="Bartlett", na.action=na.omit)

fa16 <- factanal(~., factors=2, data=as.data.frame(complete(mi2016)), scores="Bartlett", na.action=na.omit)

########################################
# FACTOR LOADINGS
########################################

fa12$loadings # Same-sex marriage, healthcare (2 items), climate (2 items), affirmative action, wealth tax, government regulation of business (0.29 of variance) # Immigrants' contribution to society, death penalty (2 items), racial resentment (4 items) (0.49 of variance)

fa16$loadings # Abortion, immigration (2 items), same-sex marriage, 

########################################
# EXTRACT QIs
########################################

### new: from confirmatory factor analysis

allqi <- cbind.data.frame("voteHist"=attribs$switch, "vote12"=substr(attribs$switch, 1, 1), "vote16"=substr(attribs$switch, 2, 2), "dimone12"=fa12$scores[,1], "dimtwo12"=fa12$scores[,2], "dimone16"=fa16$scores[,1], "dimtwo16"=fa16$scores[,2], "r12"=ifelse(attribs$post_presvote12_2012==2, 1, 0), "r16"=ifelse(attribs$presvote16post_2016==2, 1, 0), "d12"=ifelse(attribs$post_presvote12_2012==1, 1, 0), "d16"=ifelse(attribs$presvote16post_2016==1, 1, 0), "switcher"=ifelse(attribs$switch %in% c("DD", "RR", NA), 0, 1))


########################################
# PROBITS
########################################

lm12 <- glm(d12 ~ -1 + dimone12 + dimtwo12, data=allqi[allqi$switcher==0,], family=binomial(link="probit"))

lm12.switch <- glm(d12 ~ -1 + dimone12 + dimtwo12, data=allqi[allqi$switcher==1,], family=binomial(link="probit"))

lm16 <- glm(d16 ~ -1 + dimone16 + dimtwo16, data=allqi[allqi$switcher==0,], family=binomial(link="probit"))

lm16.switch <- glm(d16 ~ -1 + dimone16 + dimtwo16, data=allqi[allqi$switcher==1,], family=binomial(link="probit"))

### generate 'estimated cleavage lines' -- solve for B1 and B2 given fitted=1/2
### coef(lm12)[2]x + coef(lm12)[3]y = 1
### x + y = 0.5

## loyalists

a12 <- matrix(c(coef(lm12)[1], 1, coef(lm12)[2], 1), nrow=2, ncol=2)

b12 <- matrix(c(1, 0.5), nrow=2, ncol=1)

solve12 <- solve(a12, b12)

slope12 <- solve12[2,]/solve12[1,]

a16 <- matrix(c(coef(lm16)[1], 1, coef(lm16)[2], 1), nrow=2, ncol=2)

b16 <- matrix(c(1, 0.5), nrow=2, ncol=1)

solve16 <- solve(a16, b16)

slope16 <- solve16[2,]/solve16[1,]

## switchers

a12.s <- matrix(c(coef(lm12.switch)[1], 1, coef(lm12.switch)[2], 1), nrow=2, ncol=2)

b12.s <- matrix(c(1, 0.5), nrow=2, ncol=1)

solve12.s <- solve(a12.s, b12.s)

slope12.s <- solve12.s[2,]/solve12.s[1,]

a16.s <- matrix(c(coef(lm16.switch)[1], 1, coef(lm16.switch)[2], 1), nrow=2, ncol=2)

b16.s <- matrix(c(1, 0.5), nrow=2, ncol=1)

solve16.s <- solve(a16.s, b16.s)

slope16.s <- solve16.s[2,]/solve16.s[1,]

########################################
# PLOTS
########################################

pdf("../graphics/2d_scatterplots.pdf", width=6, height=6)

par(mfrow=c(2,2), mar=c(4.1, 4.1, 3.1, 1.1))

plot(allqi$dimone12, allqi$dimtwo12, col=attribs$col12, pch=attribs$vote12, cex=1/3, xlim=c(-3,3), ylim=c(-3,3), main="2012 Electorate", xlab="Liberal-conservative", ylab="Ethnoracial resentment", axes=F)
abline(0, slope12)
abline(0, slope12.s, lty=2)
legend("bottomleft", legend=c("Obama", "Romney", "Other"), pch=c('D', 'R', 'O'), pt.cex=c(rep(2/3,3)), col=c("blue", "red", "darkgray"), cex=0.8, title="Vote choice", bty='n')
legend('topright', legend=c("Loyalists", "Switchers"), lty=c(1,2), cex=0.8, title="Est. cleavage line", bty='n')
axis(1, tick=T)
axis(2, tick=T, las=2)

plot(allqi$dimone16, allqi$dimtwo16, col=attribs$col16, pch=attribs$vote16, cex=1/3, xlim=c(-3,3), ylim=c(-3,3), main="2016 Electorate", xlab="Liberal-conservative", ylab="Ethnoracial resentment", axes=F)
abline(0, slope16)
abline(0, slope16.s, lty=2)
legend("bottomleft", legend=c("Clinton", "Trump", "Other"), pch=c('D', 'R', 'O'), pt.cex=c(rep(2/3,3)), col=c("blue", "red", "darkgray"), cex=0.8, title="Vote choice", bty='n')
legend('topright', legend=c("Loyalists", "Switchers"), lty=c(1,2), cex=0.8, title="Est. cleavage line", bty='n')
axis(1, tick=T)
axis(2, tick=T, las=2)

### just switchers

plot(allqi$dimone12[allqi$switcher==1], allqi$dimtwo12[allqi$switcher==1], col=attribs$col12[allqi$switcher==1], pch=attribs$vote12[allqi$switcher==1], cex=1/3, xlim=c(-3,3), ylim=c(-3,3), main="Switchers in 2012", xlab="Liberal-conservative", ylab="Ethnoracial resentment", axes=F)
abline(0, slope12)
abline(0, slope12.s, lty=2)
legend("bottomleft", legend=c("Obama", "Romney", "Other"), pch=c('D', 'R', 'O'), pt.cex=c(rep(2/3,3)), col=c("blue", "red", "darkgray"), cex=0.8, title="Vote choice", bty='n')
legend('topright', legend=c("Loyalists", "Switchers"), lty=c(1,2), cex=0.8, title="Est. cleavage line", bty='n')
axis(1, tick=T)
axis(2, tick=T, las=2)

plot(allqi$dimone16[allqi$switcher==1], allqi$dimtwo16[allqi$switcher==1], col=attribs$col16[allqi$switcher==1], pch=attribs$vote16[allqi$switcher==1], cex=1/3, xlim=c(-3,3), ylim=c(-3,3), main="Switchers in 2016", xlab="Liberal-conservative", ylab="Ethnoracial resentment", axes=F)
abline(0, slope16)
abline(0, slope16.s, lty=2)
legend("bottomleft", legend=c("Clinton", "Trump", "Other"), pch=c('D', 'R', 'O'), pt.cex=c(rep(2/3,3)), col=c("blue", "red", "darkgray"), cex=0.8, title="Vote choice", bty='n')
legend('topright', legend=c("Loyalists", "Switchers"), lty=c(1,2), cex=0.8, title="Est. cleavage line", bty='n')
axis(1, tick=T)
axis(2, tick=T, las=2)

dev.off()

########################################
# DENSITY PLOTS
########################################

# xlim.val <- c(-0.6, 0.6)
# ylim.val <- c(0, 5)

xlim.val <- c(-3, 3)
ylim.val.1d <- c(0, 1.2)
ylim.val.2d <- c(0, 0.6)

pdf('../graphics/2d_density_plots.pdf', width=6, height=6)

par(mfrow=c(2,2), mar=c(3.1, 3.1, 3.1, 1.1))

### 2012 d1

plot(density(allqi$dimone12[allqi$voteHist=="DD"], na.rm=T), col="black", lwd=2, xlim=xlim.val, ylim=ylim.val.1d, ann=F, axes=F)
lines(density(allqi$dimone12[allqi$voteHist=="RR"], na.rm=T), col="darkgray", lwd=2)
lines(density(allqi$dimone12[allqi$voteHist=="DR"], na.rm=T), col="black", lwd=2, lty=2)
lines(density(allqi$dimone12[allqi$voteHist=="RD"], na.rm=T), col="darkgray", lwd=2, lty=2)
legend("topright", bty='n', legend=c("D-D", "D-R", "R-R", "R-D"), lty=c(1,2,1,2), lwd=rep(2,4), col=c("black", "black", "darkgray", "darkgray"), ncol=2, cex=0.8)
title(main="2012 1st Dimension")
axis(1, tick=T)
axis(2, tick=T, las=2)

### 2016 d1

plot(density(allqi$dimone16[allqi$voteHist=="DD"], na.rm=T), col="black", lwd=2, xlim=xlim.val, ylim=ylim.val.1d, ann=F, axes=F)
lines(density(allqi$dimone16[allqi$voteHist=="RR"], na.rm=T), col="darkgray", lwd=2)
lines(density(allqi$dimone16[allqi$voteHist=="DR"], na.rm=T), col="black", lwd=2, lty=2)
lines(density(allqi$dimone16[allqi$voteHist=="RD"], na.rm=T), col="darkgray", lwd=2, lty=2)
legend("topright", bty='n', legend=c("D-D", "D-R", "R-R", "R-D"), lty=c(1,2,1,2), lwd=rep(2,4), col=c("black", "black", "darkgray", "darkgray"), ncol=2, cex=0.8)
title(main="2016 1st Dimension")
axis(1, tick=T)
axis(2, tick=T, las=2)

### 2012 d2

plot(density(allqi$dimtwo12[allqi$voteHist=="DD"], na.rm=T), col="black", lwd=2, xlim=xlim.val, ylim=ylim.val.2d, ann=F, axes=F)
lines(density(allqi$dimtwo12[allqi$voteHist=="RR"], na.rm=T), col="darkgray", lwd=2)
lines(density(allqi$dimtwo12[allqi$voteHist=="DR"], na.rm=T), col="black", lwd=2, lty=2)
lines(density(allqi$dimtwo12[allqi$voteHist=="RD"], na.rm=T), col="darkgray", lwd=2, lty=2)
legend("topright", bty='n', legend=c("D-D", "D-R", "R-R", "R-D"), lty=c(1,2,1,2), lwd=rep(2,4), col=c("black", "black", "darkgray", "darkgray"), ncol=2, cex=0.8)
title(main="2012 2nd Dimension")
axis(1, tick=T)
axis(2, tick=T, las=2)

### 2016 d2

plot(density(allqi$dimtwo16[allqi$voteHist=="DD"], na.rm=T), col="black", lwd=2, xlim=xlim.val, ylim=ylim.val.2d, ann=F, axes=F)
lines(density(allqi$dimtwo16[allqi$voteHist=="RR"], na.rm=T), col="darkgray", lwd=2)
lines(density(allqi$dimtwo16[allqi$voteHist=="DR"], na.rm=T), col="black", lwd=2, lty=2)
lines(density(allqi$dimtwo16[allqi$voteHist=="RD"], na.rm=T), col="darkgray", lwd=2, lty=2)
legend("topright", bty='n', legend=c("D-D", "D-R", "R-R", "R-D"), lty=c(1,2,1,2), lwd=rep(2,4), col=c("black", "black", "darkgray", "darkgray"), ncol=2, cex=0.8)
title(main="2016 2nd Dimension")
axis(1, tick=T)
axis(2, tick=T, las=2)

dev.off()