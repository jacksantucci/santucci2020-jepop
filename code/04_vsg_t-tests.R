##################################################
# PACKAGES AND DATA
##################################################

library(MCMCpack)

library(coda)

library(scales)

library(xtable)

load('../output/respondent_attributes.Rdata')

attribs$switch[is.na(attribs$switch)] <- "Other" # code as "other" those who are not party loyalists, O-T switchers, nor R-C switchers

load("../output/vsg_items_recoded.Rdata")

load("../output/bayes_vsg_2012_1d.Rdata")

load("../output/bayes_vsg_2016_1d.Rdata")

##### Summarize the chains

summary12 <- summary(vsg12.1d)
summary16 <- summary(vsg16.1d)

##################################################
# NEW VARIABLE NAMES
##################################################

newnames <- c("Abortion", "Immigrants make contribution", "Immigrant legalization path", "Ease of immigration", "Gay marriage", "Death penalty", "Death penalty frequency", "Universal healthcare", "Health reform bill", "Global warming denial", "Humans cause warming", "Affirmative action", "Higher taxes >$200k", "Regulation of business", "Foreign trade", "Blacks have gotten less", "Other minorities overcame", "Blacks should try harder", "Generations of slavery")

##################################################
# DISCRIMINATION PARAMETERS AND TESTS FOR SIGNIFICANT CHANGE
##################################################

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

##### Discrimination parameters tests

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

##################################################
# T-TESTS OF WITHIN-SUBGROUP DIFFERENCES ON ITEMS
##################################################

getEstimateAndT <- function(t.stat.out){
	out <- c(t.stat.out$estimate, t.stat.out$statistic, t.stat.out$p.value)
	return(out)
}

calcDiffMeans <- function(tests, minuend.col=1, subtrahend.col=2, colname="colname"){
	outdiff <- tests[,minuend.col]-tests[,subtrahend.col]
	outdiff <- round(outdiff, 2)
	outdiff <- ifelse(abs(tests[,3])>1.96, paste0(outdiff, '*'), outdiff)
	outdiff <- as.data.frame(outdiff, stringsAsFactors=F)
	outdiff[outdiff==0] <- "0.00"
	names(outdiff) <- colname
	return(outdiff)
}

addStar <- function(vec){
	out <- ifelse(abs(vec)>1.96, paste0(vec, '*'), vec)
	return(out)
}

##### subset the panels and rescale

fort12 <- subset(items, select=grep('baseline', names(items)))

fort16 <- subset(items, select=grep('2016', names(items)))

fort12 <- as.data.frame(apply(fort12, 2, function(x) rescale(x, to=c(0, 1))))

fort16 <- as.data.frame(apply(fort16, 2, function(x) rescale(x, to=c(0, 1))))

##### run the t-tests

octests <- list(NA)
rttests <- list(NA)
ottests <- list(NA)
rctests <- list(NA)
otests <- list(NA)
for (i in 1:ncol(fort12)){
	octests[[i]] <- t.test(fort12[which(attribs$switch=="DD"), i], fort16[which(attribs$switch=="DD"), i])
	rttests[[i]] <- t.test(fort12[which(attribs$switch=="RR"), i], fort16[which(attribs$switch=="RR"), i])
	ottests[[i]] <- t.test(fort12[which(attribs$switch=="DR"), i], fort16[which(attribs$switch=="DR"), i])
	rctests[[i]] <- t.test(fort12[which(attribs$switch=="RD"), i], fort16[which(attribs$switch=="RD"), i])
	otests[[i]] <- t.test(fort12[which(attribs$switch=="Other"), i], fort16[which(attribs$switch=="Other"), i])
}

octstat <- lapply(octests, getEstimateAndT)
ocTests <- do.call(rbind, octstat)

rttstat <- lapply(rttests, getEstimateAndT)
rtTests <- do.call(rbind, rttstat)

ottstat <- lapply(ottests, getEstimateAndT)
otTests <- do.call(rbind, ottstat)

rctstat <- lapply(rctests, getEstimateAndT)
rcTests <- do.call(rbind, rctstat)

otstat <- lapply(otests, getEstimateAndT)
oTests <- do.call(rbind, otstat)

discrimdiff <- discrim1$discrim12-discrim1$discrim16 ## order of discrimination change

discrimpct <- 100*(discrim1$discrim16-discrim1$discrim12)/discrim1$discrim12 ## order of discrimination change

subgroup.table <- cbind.data.frame("Item"=newnames, "Salience"=updown, calcDiffMeans(ocTests, 2, 1, "Loyal Ds"), calcDiffMeans(rcTests, 2, 1, "Romney-Clinton"), calcDiffMeans(rtTests, 2, 1, "Loyal Rs"), calcDiffMeans(otTests, 2, 1, "Obama-Trump"), calcDiffMeans(oTests, 2, 1, "Other"), stringsAsFactors=F)

subgroup.table <- subgroup.table[order(discrimdiff, decreasing=T),]

# subgroup.table <- subgroup.table[subgroup.table$Salience!="No change",]

##################################################
# TABLES OF COALITION DIFFERENCES ON ITEMS
##################################################

coalition.table <- cbind.data.frame("Item"=newnames, "Salience"=updown, calcDiffMeans(bothTests2012, 2, 1, "Romney mean - Obama mean"), calcDiffMeans(bothTests2016, 2, 1, "Trump mean - Clinton mean"), stringsAsFactors=F) # "2012 R mean - D mean", "2016 R mean - D mean"

coalition.table$Difference <- as.numeric(gsub('\\*', '', coalition.table[,4]))-as.numeric(gsub('\\*', '', coalition.table[,3]))

coalition.table <- coalition.table[order(discrimdiff, decreasing=T),]

coalition.table <- coalition.table[coalition.table$Salience!="No change",]

##################################################
# TABLES OF INTRA-COALITION DIFFERENCES ON ITEMS
##################################################

intra.table <- cbind.data.frame("Item"=newnames, "Salience"=updown, calcDiffMeans(intradTests2012, 2, 1, "Dems 2012"), calcDiffMeans(intradTests2016, 2, 1, "Dems 2016"), calcDiffMeans(intrarTests2012, 2, 1, "Reps 2012"), calcDiffMeans(intrarTests2016, 2, 1, "Reps 2016"), stringsAsFactors=F) # "2012 R mean - D mean", "2016 R mean - D mean"

intra.table$Difference.D <- as.numeric(gsub('\\*', '', intra.table[,4]))-as.numeric(gsub('\\*', '', intra.table[,3]))

intra.table$Difference.R <- as.numeric(gsub('\\*', '', intra.table[,5]))-as.numeric(gsub('\\*', '', intra.table[,6]))

intra.table <- intra.table[order(discrimdiff, decreasing=T),]

intra.table <- intra.table[intra.table$Salience!="No change",]


##################################################
# FINAL TABLES
##################################################

coalition.table

print(xtable(coalition.table, digits=2), include.rownames=F, type="html", html.table.attributes=c('border=0'), file="../output/coalition_table.html")

subgroup.table

print(xtable(subgroup.table, digits=2), include.rownames=F, type="html", html.table.attributes=c('border=0'), file="../output/subgroup_table.html")

intra.table

print(xtable(intra.table, digits=2), include.rownames=F, type="html", html.table.attributes=c('border=0'), file="../output/intra2012_table.html")