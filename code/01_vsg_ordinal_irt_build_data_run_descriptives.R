########################################
# READ DATA
########################################

d <- read.csv("../data/VOTER_Survey_April18_Release1.csv", stringsAsFactors=F)

### check the "caseid" variable in 2012 CCAP (downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/26721, per instructions in Broockman, Kalla, and Aronow [2014, p. 5], https://stanford.edu/~dbroock/broockman_kalla_aronow_lg_irregularities.pdf). Direct access to data here: https://scholarsportal.github.io/Dataverse-Data-Explorer/?fileId=2481749&siteUrl=https://dataverse.harvard.edu. This variable name corresponds to one in the 2016 and 2018 VSG releases. Neither of those VSG codebooks references the variable.

cap <- read.delim("../data/ccap12short.tab")

## total 2012 CCAP respondents, 2016 VSG respondents

table(is.na(d$caseid))

table(is.na(d$case_identifier))

### subset to the original 4,705 respondents

table(is.na(d$caseid), is.na(d$case_identifier)) # the original 4,705 are is.na(d$caseid)==F & is.na(d$case_identifier)==F

d <- d[is.na(d$caseid)==F & is.na(d$case_identifier)==F,]
nrow(d)

### items asked in both waves

vars2012 <- c("abortview3_baseline", "immi_contribution_baseline", "immi_naturalize_baseline", "immi_makedifficult_baseline", "gaymar2_baseline", "deathpenalty_baseline", "deathpenfreq_baseline", "univhealthcov_baseline", "healthreformbill_baseline", "envwarm_baseline", "envpoll2_baseline", "affirmact_gen_baseline", "taxwealth_baseline", "govt_reg_baseline", "tradepolicy_baseline", "race_deservemore_baseline", "race_overcome_baseline", "race_tryharder_baseline", "race_slave_baseline")

vars2016=c("abortview3_2016", "immi_contribution_2016", "immi_naturalize_2016", "immi_makedifficult_2016", "gaymar_2016", "deathpen_2016", "deathpenfreq_2016", "univhealthcov_2016", "healthreformbill_2016", "envwarm_2016", "envpoll2_2016", "affirmact_gen_2016", "taxdoug_2016", "govt_reg_2016", "tradepolicy_2016", "race_deservemore_2016", "race_overcome_2016", "race_tryharder_2016", "race_slave_2016")

### respondent attributes

attribs <- subset(d, select=c("caseid", "ideo5_baseline", "ideo5_2016", "pid7_baseline", "pid7_2016", "race_baseline", "race_2016", "birthyr_baseline", "educ_baseline", "educ_2016", "gender_baseline", "presvote08_baseline", "post_presvote12_2012", "presvote16post_2016", "pp_primary16_2016",  "pp_demprim16_2016", "pp_repprim16_2016", "prim12_baseline", "L2_primary_2016"))

attribs$case <- paste0('case', attribs$caseid)

### code party switcher variable in respondent attributes

attribs$switch <- ifelse(attribs$post_presvote12_2012==1 & attribs$presvote16post_2016==2, "DR", NA)
attribs$switch[attribs$post_presvote12_2012==2 & attribs$presvote16post_2016==1] <- "RD"
attribs$switch[attribs$post_presvote12_2012==1 & attribs$presvote16post_2016==1] <- "DD"
attribs$switch[attribs$post_presvote12_2012==2 & attribs$presvote16post_2016==2] <- "RR"

### save respondent attributes

save(attribs, file='../output/respondent_attributes.Rdata')

### separate out items and recode missing values

items <- subset(d, select=c(vars2012, vars2016))

items[items==8] <- NA # missing value code

########################################
# RECODE TO CONSERVATIVE POLARITY, HANDLE NON-SYSTEMATIC MISSINGS
########################################

items$immi_naturalize_baseline[items$immi_naturalize_baseline==3] <- NA

items$gaymar2_baseline[items$gaymar2_baseline==3] <- NA

items$deathpenalty_baseline <- 10*items$deathpenalty_baseline
items$deathpenfreq_baseline[items$deathpenfreq_baseline==4] <- NA
items$deathpenalty_baseline[items$deathpenalty_baseline ==10] <- 2
items$deathpenalty_baseline[items$deathpenalty_baseline ==20] <- 1
items$deathpenfreq_baseline[items$deathpenfreq_baseline==4] <- NA

items$deathpen_2016 <- 10*items$deathpen_2016
items$deathpen_2016[items$deathpen_2016==10] <- 2
items$deathpen_2016[items$deathpen_2016==20] <- 1

items$envwarm_baseline[items$envwarm_baseline==5] <- NA

items$envwarm_2016[items$envwarm_2016==5] <- NA

items$envpoll2_baseline[items$envpoll2_baseline==3] <- NA

items$envpoll2_2016[items$envpoll2_2016==3] <- NA

items$taxwealth_baseline[items$taxwealth_baseline==3] <- NA

items$taxdoug_2016[items$taxdoug_2016==3] <- NA

items$govt_reg_baseline <- (-1*items$govt_reg_baseline)+4

items$govt_reg_2016 <- (-1*items$govt_reg_2016)+4

items$race_deservemore_baseline[items$race_deservemore_baseline==3] <- NA
items$race_deservemore_baseline[items$race_deservemore_baseline==4] <- 3
items$race_deservemore_baseline[items$race_deservemore_baseline==5] <- 4

items$race_overcome_baseline[items$race_overcome_baseline==3] <- NA
items$race_overcome_baseline[items$race_overcome_baseline==4] <- 3
items$race_overcome_baseline[items$race_overcome_baseline==5] <- 4
items$race_overcome_baseline <- (-1*items$race_overcome_baseline)+5

items$race_overcome_2016[items$race_overcome_2016==3] <- NA
items$race_overcome_2016[items$race_overcome_2016==4] <- 3
items$race_overcome_2016[items$race_overcome_2016==5] <- 4
items$race_overcome_2016 <- (-1*items$race_overcome_2016)+5

items$race_tryharder_baseline[items$race_tryharder_baseline ==3] <- NA
items$race_tryharder_baseline[items$race_tryharder_baseline ==4] <- 3
items$race_tryharder_baseline[items$race_tryharder_baseline ==5] <- 4
items$race_tryharder_baseline <- (-1*items$race_tryharder_baseline)+5

items$race_tryharder_2016[items$race_tryharder_2016 ==3] <- NA
items$race_tryharder_2016[items$race_tryharder_2016 ==4] <- 3
items$race_tryharder_2016[items$race_tryharder_2016 ==5] <- 4
items$race_tryharder_2016 <- (-1*items$race_tryharder_2016)+5

items$race_slave_baseline[items$race_slave_baseline==3] <- NA
items$race_slave_baseline[items$race_slave_baseline==4] <- 3
items$race_slave_baseline[items$race_slave_baseline==5] <- 4

table(items$immi_naturalize_2016, exclude=F)

rownames(items) <- attribs$caseid

save(vars2012, vars2016, items, file="../output/vsg_items_recoded.Rdata")

########################################
# RUN DESCRIPTIVES
########################################

library(xtable)

newnames <- c("Abortion", "Immigrants make contribution", "Immigrant legalization path", "Ease of immigration", "Gay marriage", "Death penalty", "Death penalty frequency", "Universal healthcare", "Health reform bill", "Global warming denial", "Humans cause warming", "Affirmative action", "Higher taxes >$200k", "Regulation of business", "Foreign trade", "Blacks have gotten less", "Other minorities overcame", "Blacks should try harder", "Generations of slavery")

idx12 <- grep('_baseline', names(items))

idx16 <- grep('_2016', names(items))

nonmissings <- 100*round(apply(items, 2, function(x) length(x[is.na(x)==F]))/nrow(items), 3)

mins <- apply(items, 2, function(x) min(x, na.rm=T))

maxes <- apply(items, 2, function(x) max(x, na.rm=T))
maxes[idx12]==maxes[idx16]

means <- apply(items, 2, function(x) round(mean(x, na.rm=T), 1))

descr.table <- cbind.data.frame("Item"=newnames, stringsAsFactors=F, "Var. 2012"=names(items[,idx12]), "Var. 2016"=names(items[,idx16]), "Max"=maxes[idx12], "Mean 2012"=means[idx12], "Mean 2016"=means[idx16], "% complete 2012"=nonmissings[idx12], "% complete 2016"=nonmissings[idx16])

print(xtable(descr.table, digits=1), include.rownames=F, type="html", html.table.attributes=c('border=0'), file="../output/descriptives.html")