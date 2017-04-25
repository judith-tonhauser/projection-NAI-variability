## JD working directory
setwd('/Users/titlis/cogsci/projects/stanford/projects/projection-NAI-variability/results/exp1b/')

## JT working directory
setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/Git-variability/results/exp1b/')

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
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(ucminf)
library(scales)

############## Pre-analysis data clean-up #################

# age info
table(d$age) #18-74
median(d$age) #32
mean(d$age) #34.3

# look at Turkers' comments
unique(d$comments)

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 4 to 44 
#(3 instructions at beginning, 24 missing because another instruction)
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) # trial numbers from 1 to 41 (21 missing because instruction)
d[d$trial > 20,]$trial = d[d$trial > 20,]$trial - 1
unique(d$trial) # trials from 1 to 40

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# anglish         Chinese         ebglish         english 
# 40              40              40            2360 
# English         ENGLISH        english         English  
# 6960             160              40             120 
# englush         Englush         Enlgish French, English 
# 40              40              40              40 
# Russian          telugu 
# 40              40 
d <- subset(d, (d$language != "Chinese" & d$language != "Russian" 
                & d$language != "telugu"))                  
d = droplevels(d)
length(unique(d$workerid)) #247 (data from 3 Turker excluded, 247 remaining Turkers)

# exclude non-American English speakers
length(unique(d$workerid))#247
length(which(is.na(d$ame))) #0 (everybody responded)
table(d$ame) 
# Yes 
# 9880
# d <- subset(d, d$ame == "Yes")
# d = droplevels(d)
# length(unique(d$workerid)) #no Turkers excluded

#247 remaining Turkers

################# Excuding Turkers based on main clause controls #################

### look at how Turkers did on the main clauses 
# main clauses
names(d)
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #3952 (247 Turkers x 8 MCs x 2 questions)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- subset(d.MC, d.MC$question_type == "projective")
d.MC.Proj <- droplevels(d.MC.Proj)
nrow(d.MC.Proj) #1976

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2)

# calculate each Turkers mean response to the projection of main clauses
p.means = aggregate(response~workerid, data=d.MC.Proj, FUN="mean")
p.means$YMin = p.means$response - aggregate(response~workerid, data=d.MC.Proj, FUN="ci.low")$response
p.means$YMax = p.means$response + aggregate(response~workerid, data=d.MC.Proj, FUN="ci.high")$response
p.means

ggplot(p.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- subset(d.MC, d.MC$question_type == "ai")
d.MC.AI <- droplevels(d.MC.AI)
nrow(d.MC.AI) #1976

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2)

# calculate each Turkers mean response to the projection of main clauses
ai.means = aggregate(response~workerid, data=d.MC.AI, FUN="mean")
ai.means$YMin = ai.means$response - aggregate(response~workerid, data=d.MC.AI, FUN="ci.low")$response
ai.means$YMax = ai.means$response + aggregate(response~workerid, data=d.MC.AI, FUN="ci.high")$response
ai.means

ggplot(ai.means, aes(x=workerid,y=response)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 3
# standard deviations away from the overall mean

# get the Turkers who are more than 3 standard deviations above the mean on projection 
p <- p.means[p.means$response > (mean(p.means$response) + 3*sd(p.means$response)),]
p
# 7 Turkers:
# workerid response      YMin      YMax
# 46        46  0.61000 0.5387187 0.6775000
# 68        68  0.50625 0.2062500 0.7287813
# 78        78  0.52000 0.4599688 0.5837500
# 96        96  0.55875 0.4350000 0.6750000
# 149      149  0.54000 0.4125000 0.6550625
# 168      168  0.60250 0.5137500 0.6737500
# 188      189  0.82125 0.7862500 0.8762813

# get the Turkers who are more than 3 standard deviations above the mean on ai 
ai <- ai.means[ai.means$response > (mean(ai.means$response) + 3*sd(ai.means$response)),]
ai
# 8 Turkers
# workerid response      YMin      YMax
# 1          0  0.44125 0.2299062 0.6762813
# 32        32  0.45500 0.3100000 0.5875937
# 45        45  0.42875 0.2912500 0.5750000
# 46        46  0.47750 0.3612500 0.5837812
# 59        59  0.98125 0.9737500 0.9887500
# 65        65  0.36250 0.3237187 0.4237500
# 96        96  0.46000 0.3562187 0.5512500
# 168      168  0.39125 0.2624687 0.5487812

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% p$workerid | d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #192 (12 unique outlier Turkers x 16 = 8 main clauses x 2 questions)

# check if there's any systematicity to the items that they're outliers on. maybe there's 
# something about some of the items that makes some people interpret things differently? 
# It's quite telling that the 
# error bars on the outliers are huge, which means that they could have well given mostly very 
# low responses and only on a few items gave a higher response, which may indicate either 
# systematicity to some of the items, or just noise.

# for each Turker, plot their response to the 8 main clauses they saw
ggplot(outliers, aes(x=content,y=response,color=question_type)) +
  geom_point() +
  facet_wrap(~workerid)
# 0: both NAI and projection all over the place (too high in NAI)
# 32: both NAI and projection all over the place (too high in NAI)
# 45: both NAI and projection all over the place (too high in NAI)
# 46: 8 medium-high NAI and projection responses (too high in both)
# 59: 8 high NAI responses
# 65: both NAI and projection all over the place (too high in NAI)
# 68: 8 high projection responses
# 78: 8 high projection responses
# 96: 8 medium-high projection and NAI responses (too high in both)
# 149: 7/8 high projection responses
# 168: high NAI and projection responses
# 189: 8 medium-high projection responses
## excluding all of these for systematically different responses given

# for each main clause content, plot the responses that the outlier Turkers gave to them
# do this in case the "outlier" Turkers all happened to see the same contents
ggplot(outliers, aes(x=workerid,y=response,color=question_type)) +
  geom_point() +
  facet_wrap(~content)
# all 20 represented among outliers (so it's not about a subset of items)

# exclude all outliers identified above
d <- subset(d, !(d$workerid %in% p$workerid | d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) # 235 remaining Turkers (12 Turkers excluded)

# clean data = cd
cd = d
saveRDS(cd, file="data/cd.rds")
head(cd)

################## Exclusion Turkers based on response times ############
################# no Turkers were excluded #######################

# the goal is to remove Turkers who just clicked through the experiment
# there's two numbers we can look at: the overall time it took them to take the experiment, 
# and how long they took to respond to each item

# How long did the Turkers take overall?
table(d$Answer.time_in_minutes)
mean(d$Answer.time_in_minutes)
sd(d$Answer.time_in_minutes)
min(d$Answer.time_in_minutes)
max(d$Answer.time_in_minutes)

# > mean(d$Answer.time_in_minutes)
# [1] 6.708623
# > sd(d$Answer.time_in_minutes)
# [1] 2.805515
# > min(d$Answer.time_in_minutes)
# [1] 2.16395
# > max(d$Answer.time_in_minutes)
# [1] 21.85447

# much lower mean response than before (but also lower max answer)

# Plot the response times below 5 minutes
ggplot(d, aes(x=workerid,y=Answer.time_in_minutes)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(0, 5)

# Identify participants who did the experiment in less than the mean minus 1sd minutes
fast <- d[d$Answer.time_in_minutes < mean(d$Answer.time_in_minutes) - sd(d$Answer.time_in_minutes),]
unique(fast$workerid) #2 5 13 58 80 181 

# these guys did the experiment in 3.8 minutes or less
table(fast$workerid,fast$Answer.time_in_minutes)

ggplot(fast, aes(x=workerid,y=Answer.time_in_minutes)) +
  geom_point() 

# plot the response times (rt) of the fast Turkers
ggplot(fast, aes(x=workerid,y=rt)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(0, 4000)

min(fast$rt) #1263 (1.2 seconds)

# some Turker completed the experiment fast, but they still took at least 1.9 seconds
# for each response

# now look at how long the Turkers took to respond to individual items

# rt in milliseconds (1000 milliseconds = 1 second)
mean(d$rt)
sd(d$rt)
min(d$rt)
max(d$rt)

# > mean(d$rt)
# [1] 7500.575
# > sd(d$rt)
# [1] 9558.314
# > min(d$rt)
# [1] 1263
# > max(d$rt)
# [1] 421078

# rt in seconds (1000 milliseconds = 1 second)

mean(d$rt/1000)
sd(d$rt/1000)
min(d$rt/1000)
max(d$rt/1000)

# > mean(d$rt/1000)
# [1] 7.500575
# > sd(d$rt/1000)
# [1] 9.558314
# > min(d$rt/1000)
# [1] 1.263
# > max(d$rt/1000)
# [1] 421.078

# calculate and plot the mean rt for each Turker
mean.rt = aggregate(rt~workerid, data=d, FUN="mean")
mean.rt$YMin = mean.rt$rt - aggregate(rt~workerid, data=d, FUN="ci.low")$rt
mean.rt$YMax = mean.rt$rt + aggregate(rt~workerid, data=d, FUN="ci.high")$rt
mean.rt

min(mean.rt$rt) #2.6 seconds

ggplot(mean.rt, aes(x=workerid,y=rt)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylab("Mean response time")+
  ylim(1000,10000)

# the fastest response took 1.9 seconds, so there's nothing really to worry about

str(d$rt)
d$rt = as.numeric(d$rt)
# plot the response times below 20 seconds (mean = 9 seconds)
ggplot(d, aes(x=rt))+
  geom_histogram(binwidth = 100) + 
  xlim(0, 20000)+
  xlab("Response times in milliseconds (below 20 seconds")

# Identify participants with response times lower than 4 seconds = 4000 milliseconds
fast_rt <- d[d$rt < 4000,]
unique(fast_rt$workerid) #39 Turkers

# plot the response time for the 39 Turkers with "fast" rts (less than 3 seconds)
ggplot(fast_rt, aes(x=workerid,y=rt)) +
  geom_point() + 
  geom_text(aes(label=workerid), vjust = 1, cex= 5, offset = 10)+
  ylim(1000, 5000)+
  xlab("Response times in milliseconds (below 4 seconds")

# these responses all took two or more seconds

# no Turkers are excluded based on the time it took them to respond!

# exclude the fast Turkers
# d <- subset(d, !(d$workerid %in% fast$workerid))
# d <- droplevels(d)
# length(unique(d$workerid)) #204  remaining Turkers (6 excluded)

################## Properties of the data #############
# load clean data (Turkers excluded)
cd = readRDS(file="data/cd.rds")

# age info
names(cd)
table(cd$age) #18-74
median(cd$age) #33
mean(cd$age) #34.7

# What's the distribution of contents across the triggers?
table(cd$content,cd$short_trigger)

################## test if block order mattered ############################
cd = readRDS(cd, file="data/cd.rds")
length(unique(cd$workerid)) #235

# make a data structure tmp that includes only info relevant to the analyses
# use dplyr::select to make sure that select comes from the dplyr package
tmp = cd %>%
  dplyr::select(response, short_trigger, content, question_type, trigger_class, workerid, block)
nrow(tmp) #9400 (9400 / 235 Turkers = 40 items per Turker)

# "block1" is first, "block2" is second
# the two types of questions were "ai" and "projective"
table(tmp$block,tmp$question_type)
#        ai projective
#block1 2400       2300
#block2 2300       2400
# 2400 / 20 items = 120 Turkers judged ai first
# 2300 / 20 items = 115 Turkers judged projection first

str(tmp$response)
library(lmerTest)

# make a subset of the data that only includes the target data with question about at-issueness
tmp_ai = subset(tmp, (trigger_class != "NonProj" & question_type == "ai"))
tmp_ai = droplevels(tmp_ai)
head(tmp_ai)
table(tmp_ai$block,tmp_ai$question_type)
#        ai
#block1 1440
#block2 1380
# 1440 + 1380 = 2820 / 12 triggers = 235 Turkers

# make a subset of the data that only includes the target data with question about projectivity
tmp_proj = subset(tmp, (trigger_class != "NonProj" & question_type == "projective"))
tmp_proj = droplevels(tmp_proj)
head(tmp_proj)
table(tmp_proj$block,tmp_ai$question_type)

# predict at-issueness response from trigger and block, no interaction
m.ai = lmer(response ~ short_trigger + block + (1|workerid) + (1|content), data=tmp_ai)
summary(m.ai)
# block is not significant

# predict projection response from trigger and block, no interaction
m.proj = lmer(response ~ short_trigger + block + (1|workerid) + (1|content), data=tmp_proj)
summary(m.proj)
# block is not significant

################## make data structure with projectivity and ai wide ############################
################## to be able to predict projectivity from ai #################

length(unique(cd$workerid))

# make a data structure tmp that includes only info relevant to the analyses
# use dplyr::select to make sure that select comes from the dplyr package
tmp = cd %>%
  dplyr::select(response, short_trigger, content, question_type, trigger_class, workerid, block)
tmp$block_ai = tmp$block
tmp[tmp$question_type == "projective" & tmp$block == "block1",]$block_ai = "block2"
tmp[tmp$question_type == "projective" & tmp$block == "block2",]$block_ai = "block1"
nrow(tmp) #9400 (9400/40 items = 235 Turkers)

# make a new column "variable" that codes a trigger-content-class-workerid combination
tmp$variable = paste(tmp$short_trigger, tmp$content, tmp$trigger_class, tmp$workerid, tmp$block_ai)
tmp$variable
str(tmp$variable)
head(tmp)

# make a new data structure t that has rows with the variable info, the ai and the projective responses 
t = tmp %>%
  dcast(variable ~ question_type , value.var="response")

# now add back onto that the info in the variable as separate columns
t$short_trigger = sapply(strsplit(as.character(t$variable)," "), "[", 1)
t$content = sapply(strsplit(as.character(t$variable)," "), "[", 2)
t$trigger_class = sapply(strsplit(as.character(t$variable)," "), "[", 3)
t$workerid = sapply(strsplit(as.character(t$variable)," "), "[", 4)
t$block_ai = sapply(strsplit(as.character(t$variable)," "), "[", 5)
head(t)
nrow(t) #4700 (good: half of 9400)

# save data structure t 
saveRDS(t, file="data/t.rds")
head(t)

# now we have a semi-wide data structure t that codes for each Turker-item pair 
# the ai and projective response by that Turker to that item
t <- readRDS(file="data/t.rds")
nrow(t)

str(t$projective)
str(t$ai)
str(t$workerid)
t$workerid <- as.factor(t$workerid)
str(t$content)
t$content <- as.factor(t$content)
head(t)
table(t$short_trigger)



### START OF JUDITH D'S PRELIMINARY ANALYSIS CODE

# make main clauses the reference level 
t$short_trigger = as.factor(as.character(t$short_trigger))
contrasts(t$short_trigger)
t$short_trigger <- relevel(t$short_trigger, ref = "MC")
t$block_ai = as.factor(t$block_ai)
t$cblock_ai = myCenter(t$block_ai)
t$cai = myCenter(t$ai)

m = lmer(projective ~ ai*short_trigger + cblock_ai + (1+ai|workerid) + (1+ai|content), data=t)
summary(m)

# do the same without main clauses
t_nomc = droplevels(subset(t, short_trigger != "MC"))
t_nomc$cblock_ai = myCenter(t_nomc$block_ai)
t_nomc$cai = myCenter(t_nomc$ai)
contrasts(t_nomc$short_trigger)

t_nomc$Trigger = factor(x=as.character(t_nomc$short_trigger),levels=c("established","confessed","revealed","discovered","learned","found_out","saw","is_amused","realize","is_aware","noticed","is_annoyed"))

# this will only run if you don't load plyr
agr = t_nomc %>%
  group_by(content,Trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai

ggplot(agr, aes(x=mean_ai,y=mean_proj,color=Trigger)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0,1) +
  ylim(0,1)
ggsave(file="graphs/ai-proj-bytrigger-bycontent.pdf",width=5.7,height=4)

agr = t_nomc %>%
  group_by(Trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai
ggplot(agr, aes(x=mean_ai,y=mean_proj,color=Trigger)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.35,1) +
  ylim(0.35,1)
ggsave(file="graphs/ai-proj-bytrigger.pdf",width=4.8,height=3)

ggplot(agr, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  # geom_smooth(method="lm") +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.35,1) +
  ylim(0.35,1)
ggsave(file="graphs/ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)

agr # proj means of annoyed (.92) and discover (.85) / ai means of annoyed (.94) and discover (.89)

# block effect
agr = t_nomc %>%
  group_by(Trigger,block_ai) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai
agr$Order = ifelse(agr$block_ai=="block1","ai-proj","proj-ai")
ggplot(agr, aes(x=mean_ai,y=mean_proj,color=Trigger,group=Order)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  geom_smooth(method="lm") +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0.25,1) +
  ylim(0.25,1) +
  facet_wrap(~Order)
ggsave(file="graphs/ai-proj-bytrigger-byblock.pdf",width=8.5,height=4)


# MAIN ANALYSIS OF INTEREST: PREDICT PROJECTIVITY FROM FIXED EFFECTS OF TRIGGER, AT-ISSUENESS, INTERACTION (AND CONTROL FOR BLOCK)
# We're including trigger as a random rather than as a fixed effect. Why? Too unwieldy as fixed effect because the trigger variable has too many levels. Instead, have trigger be random effect (which I think is conceptually nice because ideally one would want to be able to run this experiment with arbitrarily many triggers -- i.e., the levels of the variable don't exhaust the population)

### THE MODEL FROM EXP 1A DOESN'T CONVERGE BECAUSE THERE IS ZERO VARIATION IN BY-CONTENT INTERCEPTS, SO WE'RE REPORTING MODEL WITH NO BY-CONTENT INTERCEPTS INSTEAD. 
#THE MODEL CURRENTLY REPORTED IN THE PAPER:
m.mr.1 = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.1)

m.mr.0a = lmer(projective ~ cai + cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0a)

m.mr.0b = lmer(projective ~ cai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0b)

m.mr.0c = lmer(projective ~ cblock_ai + cai : cblock_ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.0c)

anova(m.mr.0a,m.mr.1) #p-value for interaction
anova(m.mr.0b,m.mr.1) #p-value for block
anova(m.mr.0c,m.mr.1) #p-value for at-issueness

# get p-values for random effects
m.0a = lmer(projective ~ cai * cblock_ai + (0+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0b = lmer(projective ~ cai * cblock_ai + (1|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0c = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (1+cai|short_trigger), data=t_nomc, REML=F)
m.0e = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (0+cai|short_trigger), data=t_nomc, REML=F)
m.0f = lmer(projective ~ cai * cblock_ai + (1+cai|workerid) + (0+cai|content) + (1|short_trigger), data=t_nomc, REML=F)

anova(m.0a,m.mr.1) # p-value for by-participant intercepts
anova(m.0b,m.mr.1) # p-value for by-participant slopes for at-issueness
anova(m.0c,m.mr.1) # p-value for by-content slopes for at-issueness (ns)
anova(m.0e,m.mr.1) # p-value for by-trigger intercepts
anova(m.0f,m.mr.1) # p-value for by-trigger slopes for at-issueness (ns)


# simple effects for interaction interpretation
m.mr.simple = lmer(projective ~ ai * block_ai - ai + (1+cai|workerid) + (0+cai|content) + (1+cai|short_trigger), data=t_nomc, REML=F)
summary(m.mr.simple)

# plot fixed and random effects in various ways -- exploratory
library(sjPlot)
library(sjmisc)

sjp.lmer(m.mr.1,type="fe")
sjp.lmer(m.mr.1,type="re.qq") # this looks reasonable
ranef(m.mr.1)
plot(ranef(m.mr.1)$short_trigger[,1],ranef(m.mr.1)$short_trigger[,2])
plot(ranef(m.mr.1)$content[,1],ranef(m.mr.1)$content[,2])
plot(ranef(m.mr.1)$workerid[,1],ranef(m.mr.1)$workerid[,2])


# pairwise comparisons on short_trigger using tukey
library(lsmeans)

# run the model again with trigger as fixed effect so you can do multiple comparisons (and no at-issueness or block)
m.mr.fixedtrigger = lmer(projective ~ short_trigger + (1|workerid), data=t_nomc, REML=F)
summary(m.mr.fixedtrigger)

pc = lsmeans(m.mr.fixedtrigger, revpairwise~short_trigger, adjust="tukey")
pc


### END OF JUDITH D'S ANALYSIS CODE FOR EXP 1B









# predict projectivity
proj <- lmer(projective ~ short_trigger * ai + (1+short_trigger*ai|workerid) + (1|content), data=t)
# proj does not converge
proj.1 <- lmer(projective ~ short_trigger * ai + (1+ai|workerid) + (1|content), data=t)
summary(proj.1)
# all verbs, ai and some interactions significant
proj.2 <- lmer(projective ~ short_trigger + ai + (1+ai|workerid) + (1|content), data=t)
summary(proj.2)
anova(proj.1,proj.2)
# model with interaction significantly better than without

# predict not-at-issueness
nai <- lmer(ai ~ short_trigger + (1+short_trigger|workerid) + (1|content), data=t)
# nai does not converge
nai.1 <- lmer(ai ~ short_trigger + (1|workerid) + (1|content), data=t)
summary(nai.1)

# multiple comparison wrt projectivity
comp_proj.1 <- glht(proj.1, mcp(short_trigger="Tukey"))
#covariate interactions found -- default contrast might be inappropriate
comp_proj.2 <- glht(proj.2, mcp(short_trigger="Tukey"))
summary(comp_proj.2)

# multiple comparisons wrt not-at-issueness
comp_nai <- glht(nai.1,mcp(short_trigger="Tukey"))
summary(comp_nai)

#####################  Plotting ############################ 
names(t)
head(t)
table(t$trigger_class)
theme_set(theme_bw())

### are the 20 contents more projective when realized with trigger than with main clause?
agr = aggregate(projective~short_trigger+content, data=t, FUN="mean")
agr$YMin = agr$projective - aggregate(projective~short_trigger+content, data=t, FUN="ci.low")$projective
agr$YMax = agr$projective + aggregate(projective~short_trigger+content, data=t, FUN="ci.high")$projective
agr

ggplot(agr, aes(x=short_trigger, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  facet_wrap(~content,ncol=1)
ggsave(f="graphs/contents-mean-projective-nonprojective.pdf",height=20,width=6)

### plot the projectivity of the different triggers
str(t$projective)
table(t$short_trigger)
t.proj <- droplevels(subset(t,t$short_trigger != "MC"))
table(t.proj$short_trigger)

mean_proj = aggregate(projective~short_trigger, data=t.proj, FUN="mean")
mean_proj$YMin = mean_proj$projective - aggregate(projective~short_trigger, data=t.proj, FUN="ci.low")$projective
mean_proj$YMax = mean_proj$projective + aggregate(projective~short_trigger, data=t.proj, FUN="ci.high")$projective
mean_proj

t.proj$trigger_proj <-factor(t.proj$short_trigger, levels=mean_proj[order(mean_proj$projective), "short_trigger"])

ggplot(t.proj, aes(x=trigger_proj, y=projective)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
#geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-projection.pdf",height=3,width=10)

ggplot(t.proj, aes(x=trigger_proj, y=projective)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Projectivity")+
  xlab("Projective content trigger")
ggsave(f="graphs/boxplot-projection.pdf",height=3,width=9)

### judith d's added code for investigating individual variability
p=ggplot(t.proj, aes(x=trigger_proj, y=projective)) + 
  # geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  facet_wrap(~workerid) +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
ggsave(p,file="graphs/variability-projection.pdf",height=20,width=20)

head(t.proj)

variances = t.proj %>%
  group_by(workerid) %>%
  summarise(ProjVariance = var(projective),ProjMean=mean(projective),Proj.ci.low=ci.low(projective),Proj.ci.high=ci.high(projective),AIVariance = var(ai),AIMean=mean(ai),AI.ci.low=ci.low(ai),AI.ci.high=ci.high(ai))
variances = as.data.frame(variances)

ggplot(variances, aes(x=reorder(workerid,ProjMean),y=ProjMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=ProjMean-Proj.ci.low,ymax=ProjMean+Proj.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Projectivity")
ggsave("graphs/projection-subjectmeans.pdf",height=3,width=9)

ggplot(variances, aes(x=reorder(workerid,AIMean),y=AIMean)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=AIMean-AI.ci.low,ymax=AIMean+AI.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  xlab("Participant") +
  ylab("Not-at-issueness ('asking whether')")
ggsave("graphs/ai-subjectmeans.pdf",height=3,width=9)

### plot the not-at-issueness of the different projective content triggers

# calculate mean not-at-issueness for each trigger and relevel by mean
mean_nai = aggregate(ai~short_trigger, data=t.proj, FUN="mean")
mean_nai$YMin = mean_nai$ai - aggregate(ai~short_trigger, data=t.proj, FUN="ci.low")$ai
mean_nai$YMax = mean_nai$ai + aggregate(ai~short_trigger, data=t.proj, FUN="ci.high")$ai
mean_nai

# save mean not-at-issueness for comparison to other experiments
mean_nai_Exp6 <- mean_nai
saveRDS(mean_nai_Exp6, file="data/mean_nai_Exp6.rds")

t.proj$trigger_ai <-factor(t.proj$short_trigger, levels=mean_nai[order(mean_nai$ai), "short_trigger"])

ggplot(t.proj, aes(x=trigger_ai, y=ai)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
#geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-not-at-issueness.pdf",height=3,width=10)

ggplot(t.proj, aes(x=trigger_ai, y=ai)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness ('asking whether')")+
  xlab("Projective content trigger")
ggsave(f="graphs/boxplot-not-at-issueness.pdf",height=3.1,width=9)


##### Correlation between not-at-issueness and projectivity #################

#### plot the correlation between not-at-issueness and projectivity of projective content triggers
#### based on the raw responses by each Turker to each item
AP <- lm(projective ~ ai + short_trigger, data = t.proj)
summary(AP)

ggplot(t.proj, aes(x=ai,y=projective,group=1)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  #geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  #geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  ylab("Projection response mean")+
  xlab("NAI response mean")
ggsave(file="graphs/correlation-raw-responses.pdf",width = 14, height = 6)

#### plot the correlation between mean NAI and mean projectivity for each projective content

# create data structure that codes mean nai response for each trigger (and its class)  
agr = aggregate(ai~short_trigger+trigger_class, data=t, FUN="mean")
agr$YMin = agr$ai - aggregate(ai~short_trigger+trigger_class, data=t, FUN="ci.low")$ai
agr$YMax = agr$ai + aggregate(ai~short_trigger+trigger_class, data=t, FUN="ci.high")$ai
agr

# create data structure that codes mean projection response for each trigger (and its class)
agrr = aggregate(projective~short_trigger+trigger_class, data=t, FUN="mean")
agrr$YMin = agrr$projective - aggregate(projective~short_trigger+trigger_class, data=t, FUN="ci.low")$projective
agrr$YMax = agrr$projective + aggregate(projective~short_trigger+trigger_class, data=t, FUN="ci.high")$projective
agrr

# make the row names of the agrr (projection) to be the trigger names (instead of numbers)
row.names(agrr) = agrr$short_trigger

# put the projective response column from the agrr structure into the agr structure
agr$projective = agrr$projective
agr

# record the YMin and YMax of the agrr structure in the agr structure
agr$pYMin = agrr$YMin
agr$pYMax = agrr$YMax

# now the agr structure (ai response stuff) also has the projective response stuff 
agr

# save data structure for use in comparison with Exp5/1b
mean_proj_NAI_Exp6 <- agr
saveRDS(mean_proj_NAI_Exp6, file="data/mean_proj_NAI_Exp6.rds")

# plot mean ai and projective responses by trigger class, label with trigger name
# includes main clauses
AP <- lm(projective ~ ai, data = mean_proj_NAI_Exp6)
summary(AP)

ggplot(mean_proj_NAI_Exp6, aes(x=ai,y=projective,color=trigger_class,group=1)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  ylab("Projection response mean")+
  xlab("NAI response mean")
ggsave(file="graphs/correlation-by-mean-proj-NAI.pdf",width = 14, height = 6)

# plot mean ai and projective responses by trigger class, label with trigger name
# excludes main clauses
AP2 <- lm(projective ~ ai, data = mean_proj_NAI_Exp6, trigger_class != "NonProj")
summary(AP2)

ggplot(subset(mean_proj_NAI_Exp6, trigger_class != "NonProj"), aes(x=ai,y=projective)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger, angle=0, hjust=0, vjust=1)) + 
  #geom_text(aes(label=short_trigger), vjust = 1, cex= 5,
  #position=position_jitter(h=.01,w=0.03))+
  labs(title = paste("Adj R2 = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  ylab("Mean projectivity")+
  xlab("Mean not-at-issueness ('asking whether')")
ggsave(file="graphs/correlation-by-mean.pdf",width = 8, height = 4)
