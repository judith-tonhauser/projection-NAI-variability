## JD working directory
setwd('/Users/titlis/cogsci/projects/stanford/projects/projection-NAI-variability/results/exp2a/')

## JT working directory
setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/Git-variability/results/exp2a/')

## code for both starts here
source('rscripts/helpers.R')

# read in the data
d = readRDS(file="data/d.rds")

theme_set(theme_bw())

library(plyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(languageR)
library(lme4)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)
library(ucminf)
library(scales)

############## Pre-analysis data clean-up #################

# look at Turkers' comments
unique(d$comments)

# age info
table(d$age) #20-77
# convert 3' into 30
d$age <- gsub("\`","0",d$age)
# change into numeric
str(d$age)
d$age <- as.numeric(d$age)
median(d$age) #30
mean(d$age) #33.1

# change the response so that what was 1/at-issue is now 0/at-issue
# by subtracting the responses from 1
table(d$short_trigger,d$response)
d$response = 1 - d$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 3 to 17 (2 instruction slides at beginning)
d$trial = d$slide_number_in_experiment - 2
unique(d$trial) # trial numbers from 1 to 15

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# englih    english    eNGLISH    English    ENGLISH 
# 15       1050         15       2490         60 
# english    English     englsih     Enlish   mandarin 
# 15         30         15         15         15 
# Tamil Vietnamese 
# 15         15
d <- subset(d, (d$language != "Tamil" & d$language != "Vietnamese" 
                & d$language != "mandarin"))
d = droplevels(d)
length(unique(d$workerid)) #247 (data from 3 Turkers excluded, 247 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid)) #247
length(which(is.na(d$ame))) #15 (one Turker did not respond)
table(d$ame) 
#No  Yes 
#30 3660
d <- subset(d, d$ame == "Yes") #(also exclude the Turker who did not respond)
d = droplevels(d)
length(unique(d$workerid)) #244 (data from 3 Turkers excluded, 244 remaining Turkers)

################# Excuding Turkers based on main clause controls #################

### look at how Turkers did on the main clauses 
# main clauses
names(d)
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #1464 (244 Turkers x 6 MCs)

# group projection mean (all Turkers, all clauses)
round(mean(d.MC$response),2)

# calculate each Turkers mean response to the not-at-issueness of main clauses
ai.means = aggregate(response~workerid, data=d.MC, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC, FUN="ci.high")$response
ai.means

ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# identify Turkers whose response mean on NAI of main clauses is more than 3
# standard deviations higher than the mean 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai
# 6 Turkers give unusually high responses: 0, 89, 93, 117, 118, 244

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #36 (6 unique outlier Turkers x 6 main clauses)

# check if there's any systematicity to the items that they're outliers on

# for each Turker, plot their response to the 6 main clauses they saw
ggplot(outliers, aes(x=content,y=response)) +
  geom_point() +
  facet_wrap(~workerid)
# 0 gave really high response to 1 of the 6 main clauses, around .2 for the other 5 (mean .25)
# 89 and 118 have mean .25 and .26, and that's where all responses hover around
# 117 and 244 have mean .33 and .30, and that's where all the responses hover around
# 93 gave really high responses to all 6 (mean .63)

# for each main clause content, plot the responses that the outlier Turkers gave to them
# do this in case the "outlier" Turkers all happened to see the same contents
ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point() +
  facet_wrap(~content)
# 14 of 17 contents represented among outliers (so it's not about a subset of items)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% outliers$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #238 remaining Turkers, 6 excluded

# clean data = cd
cd = d
saveRDS(cd, file="data/cd.rds")
head(cd)

################## Exclusion Turkers based on response times ############
################# no Turkers were excluded #######################

# How long did the Turkers take?
table(d$Answer.time_in_minutes)
mean(d$Answer.time_in_minutes) 
sd(d$Answer.time_in_minutes) 
min(d$Answer.time_in_minutes)
max(d$Answer.time_in_minutes)

mean(d$rt/1000)
sd(d$rt/1000)
min(d$rt/1000)
max(d$rt/1000)

# mean response times per Turker
mean.rt = aggregate(rt~workerid, data=d, FUN="mean")
mean.rt$YMin = mean.rt$rt - aggregate(rt~workerid, data=d, FUN="ci.low")$rt
mean.rt$YMax = mean.rt$rt + aggregate(rt~workerid, data=d, FUN="ci.high")$rt
mean.rt

min(mean.rt$rt)
max(mean.rt$rt)
sd(mean.rt$rt)

ggplot(mean.rt, aes(x=workerid,y=rt)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylab("Mean response time")+
  ylim(800,10000)

# Identify participants with response times lower than 4 seconds = 4000 milliseconds
fast_rt <- d[d$rt < 2500,]
unique(fast_rt$workerid) 

fastest <- subset(fast_rt, fast_rt$workerid == "27")
fastest

fastest$rt

ggplot(fast_rt, aes(x=workerid,y=rt)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(500, 5000)+
  xlab("Response times in milliseconds (below 4 seconds")

# did not exclude the fast Turkers because it seemed to arbitrary
#d <- subset(d, !(d$workerid %in% fast$workerid))
#d <- droplevels(d)
#length(unique(d$workerid)) #91  remaining Turkers (3 excluded)

################## Properties of the data #############
# load clean data (Turkers excluded)
cd = readRDS(cd, file="data/cd.rds")

# age info
table(cd$age) #20-77
median(cd$age) #30
mean(cd$age) #33.1

# What's the distribution of contents across the triggers?
table(cd$content,cd$short_trigger)

################## Analyses ############################

# make a data structure tmp that includes only info relevant to the analyses
t = d %>%
  dplyr::select(response, short_trigger, content, trigger_class, workerid)
head(t)

# save data structure t 
saveRDS(t, file="data/t.rds")

############# START OF JD'S ANALYSIS CODE ######################
t <- readRDS(file="data/t.rds")

# aggregate responses by trigger for merging in means from exp 1a to plot
tagr = t %>%
  group_by(short_trigger) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# continue here aggregating only by trigger for both datasets and plotting

# load and aggregate data from exp 1a to merge in projectivity means
t.proj <- readRDS(file="../exp1a/data/t.rds")
tagr.proj = t.proj %>%
  group_by(short_trigger) %>%
  summarise(mean_proj = mean(projective), ci_low_proj = ci.low(projective), ci_high_proj = ci.high(projective))
tagr.proj = as.data.frame(tagr.proj)
tagr.proj$ci_min_proj = tagr.proj$mean_proj - tagr.proj$ci_low_proj
tagr.proj$ci_max_proj = tagr.proj$mean_proj + tagr.proj$ci_high_proj
head(tagr.proj)

means = tagr %>%
  select(short_trigger,mean_ai,ci_min_ai,ci_max_ai) %>%
  inner_join(tagr.proj[,c("short_trigger","mean_proj","ci_min_proj","ci_max_proj")],by=c("short_trigger"))

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc) 

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,color=Trigger,group=1)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  geom_point() +
  # geom_smooth(method="lm") +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.3,1) +
  ylim(0.3,1)
ggsave(file="graphs/ai-proj-bytrigger.pdf",width=4.8,height=3)

ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  geom_point() +
  # geom_smooth(method="lm") +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating ('are you sure')") +
  ylab("Mean projectivity rating") +
  xlim(0.3,1) +
  ylim(0.3,1)
ggsave(file="graphs/ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)

# aggregate responses by trigger and content for merging in means from exp 1a to plot and run regression analysis
tagr = t %>%
  group_by(short_trigger, content) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# load and aggregate data from exp 1a to merge in projectivity means
t.proj <- readRDS(file="../exp1a/data/t.rds")
tagr.proj = t.proj %>%
  group_by(short_trigger, content) %>%
  summarise(mean_proj = mean(projective), ci_low_proj = ci.low(projective), ci_high_proj = ci.high(projective))
tagr.proj = as.data.frame(tagr.proj)
tagr.proj$ci_min_proj = tagr.proj$mean_proj - tagr.proj$ci_low_proj
tagr.proj$ci_max_proj = tagr.proj$mean_proj + tagr.proj$ci_high_proj
head(tagr.proj)

means = tagr %>%
  select(short_trigger,content,mean_ai,ci_min_ai,ci_max_ai) %>%
  inner_join(tagr.proj[,c("short_trigger","content","mean_proj","ci_min_proj","ci_max_proj")],by=c("short_trigger","content"))

nrow(means) # this only leaves 60 data points for analysis (of which 17 are MCs, i.e., only 43)
saveRDS(means, file="data/means.rds")

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc)
means_nomc$cmean_ai = myCenter(means_nomc$mean_ai)

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# plot it all -- this will only run if you don't load plyr
ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,color=Trigger,group=1)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  # geom_smooth(method="lm") +
  geom_point() +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.25,1) +
  ylim(0.25,1)
ggsave(file="graphs/ai-proj-bytrigger-bycontent.pdf",width=5.7,height=4)


m = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m)

m.0 = lmer(mean_proj ~ (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.0)

m.1 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1|content), data = means_nomc, REML=F)
summary(m.1)

m.2 = lmer(mean_proj ~ cmean_ai + (1|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML = F)
summary(m.2)

m.3 = lmer(mean_proj ~ cmean_ai + (0+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m.3)

m.4 = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (0+cmean_ai|content), data = means_nomc, REML=F)
summary(m.4)

anova(m.1,m) # p-value for by-content slope -- ns
anova(m.2,m) # p-value for by-trigger slope -- ns
anova(m.3,m) # p-value for by-trigger intercept -- ns
anova(m.4,m) # p-value for by-content intercept -- .02
anova(m.0,m) # p-value for at-issueness -- .06

# so, only include by-content intercepts. this is the model reported in the paper:
m.report = lmer(mean_proj ~ cmean_ai + (1|content), data = means_nomc, REML=F)
summary(m.report)

m.report.0 = lmer(mean_proj ~ 1 + (1|content), data = means_nomc, REML=F)
summary(m.report.0)

anova(m.report.0,m.report) # p-value for at-issueness -- .0001  (but if you instead report the one with max random effects structure, p-value for at-issueness is only marginally significant)

# power analysis
library(simr)
# effect sizes of at-issueness in exp. 1a and 1b, for comparison: .37, .34. if we wanted to test for power of m to detect effect size of .35, we would say the following before running powerSim:
# fixef(m)["cmean_ai"] = .35
# effect sizes in exp. 2a and 2b: .25/.29 (depending on random effects structure)

# we analyze the power of two different models: 
# m -- has full maximal random effects structure and detected only marginally significant effect of at-issueness (beta = .25)
fixef(m)["cmean_ai"]
powerSim(m) # power: 60.40% (57.29, 63.45)

# Power for predictor 'cmean_ai', (95% confidence interval):
#   60.40% (57.29, 63.45)
# 
# Test: Kenward Roger (package pbkrtest)
# Effect size for cmean_ai is 0.25
# 
# Based on 1000 simulations, (171 warnings, 0 errors)
# alpha = 0.05, nrow = 43
# 
# Time elapsed: 0 h 20 m 53 s

# m.report -- has only by-content random intercepts after model comparison (beta = .29)
fixef(m.report)["cmean_ai"]
powerSim(m.report) # power: 99.80% (99.28, 99.98)
# Power for predictor 'cmean_ai', (95% confidence interval):
#   99.80% (99.28, 99.98)
# 
# Test: Kenward Roger (package pbkrtest)
# Effect size for cmean_ai is 0.29
# 
# Based on 1000 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 43
# 
# Time elapsed: 0 h 9 m 21 s

# we want to know: does m have 80% power to detect .25 effect of at-issueness? does m.report? 
# if m doesn't and m.report does: ok. but that means we don't have a way of estimating the random effects (tough luck)
# if both do: guess it's probably not real then.
# if neither do: UNDERPOWERED

############# END OF JD'S ANALYSIS CODE ######################

############# Plotting ######################
t <- readRDS(file="data/t.rds")
nrow(t) #3570 / 15 = 238 Turkers

### are the 20 contents more NAI when realized with trigger than with main clause?
agr = aggregate(response~short_trigger+content, data=t, FUN="mean")
agr$YMin = agr$response - aggregate(response~short_trigger+content, data=t, FUN="ci.low")$response
agr$YMax = agr$response + aggregate(response~short_trigger+content, data=t, FUN="ci.high")$response
agr

ggplot(agr, aes(x=short_trigger, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  facet_wrap(~content,ncol=1)
ggsave(f="graphs/contents-mean-response.pdf",height=20,width=6)

### plot the not-at-issueness of the different projective content triggers
t.proj <- droplevels(subset(t,t$short_trigger != "MC"))
table(t.proj$short_trigger)

# calculate mean not-at-issueness for each trigger and relevel by mean
mean_nai = aggregate(response~short_trigger, data=t.proj, FUN="mean")
mean_nai$YMin = mean_nai$response - aggregate(response~short_trigger, data=t.proj, FUN="ci.low")$response
mean_nai$YMax = mean_nai$response + aggregate(response~short_trigger, data=t.proj, FUN="ci.high")$response
mean_nai

# save mean not-at-issueness for comparison to other experiments
mean_nai_Exp5 <- mean_nai
saveRDS(mean_nai_Exp5, file="data/mean_nai_Exp5.rds")

t.proj$trigger_ai <- factor(t.proj$short_trigger, levels=mean_nai[order(mean_nai$response), "short_trigger"])

ggplot(t.proj, aes(x=trigger_ai, y=response)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
#geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-not-at-issueness.pdf",height=3,width=6)

ggplot(t.proj, aes(x=trigger_ai, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness ('asking whether')")+
  xlab("Projective content trigger")
ggsave(f="graphs/boxplot-not-at-issueness.pdf",height=3.1,width=6)

# JT inserted JD's code here for calculating individual variability
head(t.proj)

variances = t.proj %>%
  group_by(workerid) %>%
  summarise(AIMean=mean(response),AI.ci.low=ci.low(response),AI.ci.high=ci.high(response))
variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,AIMean),y=AIMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=AIMean-AI.ci.low,ymax=AIMean+AI.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Not-at-issueness ('asking whether')")
ggsave("graphs/ai-subjectmeans.pdf",height=3,width=6)


######## Analysis ################
library(lmerTest)

### are projective contents significantly more NAI than main clauses?
names(t)

# make main clauses the reference level 
t$short_trigger = as.factor(as.character(t$short_trigger))
contrasts(t$short_trigger)
t$short_trigger <- relevel(t$short_trigger, ref = "MC")

# predict not-at-issueness
nai <- lmer(response ~ short_trigger + (1+short_trigger|workerid) + (1|content), data=t)
# nai does not converge
nai.1 <- lmer(response ~ short_trigger + (1|workerid) + (1|content), data=t)
summary(nai.1)
# all projective contents significantly more NAI than main clauses

# load library for multiple comparisons
library(multcomp)

# multiple comparisons wrt not-at-issueness
comp_nai <- glht(nai.1,mcp(short_trigger="Tukey"))
summary(comp_nai)

################## MERGE RESULTS FROM EXPERIMENTS 4 AND 5 ###################

### Plotting and analysis based on means 

# Current means data structure has the new not-at-issue responses for the 9 triggers and MCs
head(mean_nai_Exp5)
table(mean_nai_Exp5$short_trigger)
means.5 <- mean_nai_Exp5

# Current working directory:
# setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/5-NAI/results')
# read in the means data structure from experiment 4
means.4 <- readRDS("../exp1a/data/mean_nai_Exp4.rds")
table(means.4$short_trigger)
# remove main clauses
#means.4 <- droplevels(subset(means.4, short_trigger != "MC"))

head(means.4)
head(means.5)

# change the column names of means.5
colnames(means.5) <- c("short_trigger", "ai_new", "New_YMin", "New_YMax")
head(means.5)

# merge means.4 and means.5 
means.45 <- merge(means.4,means.5,"short_trigger")
head(means.45)

# save merged data for comparison with factive verb experiments
saveRDS(means.45, file="data/means.45.rds")

## Plotting
means.45 <- readRDS("data/means.45.rds")

# plot mean not-at-issueness (2 measures) and projectivity
AP <- lm(projective ~ ai, data = means.45)
summary(AP)
AP2 <- lm(projective ~ ai_new, data = means.45)
summary(AP2)

ggplot(subset(means.45)) + 
  geom_point(aes(x = ai, y = projective, color = "ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 NAI 'asking whether'= ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 NAI 'Are you sure?' = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('Mean not-at-issueness (both diagnostics)') +
  ylab('Mean projectivity')
ggsave(file="graphs/correlation-means-NAI-NAI-Proj.pdf",width = 14, height = 6)

# plot correlation between two measures of not-at-issueness
AP3 <- lm(ai ~ ai_new, data = means.45)
summary(AP3)

ggplot(subset(means.45)) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 'whether' x 'sure'= ",signif(summary(AP3)$adj.r.squared, 5),
                     "Intercept =",signif(AP3$coef[[1]],5 ),
                     " Slope =",signif(AP3$coef[[2]], 5),
                     " P =",signif(summary(AP3)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  theme(text = element_text(size=14)) +
  xlab("Mean not-at-issueness ('asking whether')") +
  ylab("Mean not-at-issueness ('are you sure?')")
ggsave(file="graphs/correlation-means-NAI-NAI.pdf",width = 10, height = 5)
