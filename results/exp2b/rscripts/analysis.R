## JD working directory
setwd('/Users/titlis/cogsci/projects/stanford/projects/projection-NAI-variability/results/exp2b/')

## JT working directory
setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/Git-variability/results/exp2b/')

## code for both starts here
source('rscripts/helpers.R')

# read in the data
d = readRDS(d, file="data/d.rds")
nrow(d) #5000 (250 Turkers x 20 responses)
names(d)
head(d)
summary(d)

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
table(d$age) #18-77
median(d$age) #29
mean(d$age) #31.4

# look at Turkers' comments
unique(d$comments)

# change the response so that what was 1/at-issue is now 0/at-issue
# by subtracting the responses from 1
table(d$short_trigger,d$response)
d$response = 1 - d$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 3 to 22 
# (2 instruction slides at beginning)
d$trial = d$slide_number_in_experiment - 2
unique(d$trial) # trial numbers from 1 to 20

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(unique(d$workerid)) #250 (250 Turkers participated)
length(which(is.na(d$language))) #no missing responses
table(d$language) 
# Cantonese             egnlish              Engish 
# 20                  20                  20 
# english             English            English  
# 1320                3500                  20 
# English and Spanish              Korean             Russian 
# 20                  20                  20 
# Spanish          Vietnamese 
# 20                  20 
d <- subset(d, (d$language != "Cantonese" & d$language != "Vietnamese" 
                & d$language != "Korean" & d$language != "Russian"
                & d$language != "Spanish"))
d = droplevels(d)
length(unique(d$workerid)) #245 (data from 5 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #245
length(which(is.na(d$ame))) #20 (one Turker did not respond)
table(d$ame) 
# Yes 
# 4880
d <- subset(d, d$ame == "Yes") #(exclude the Turker who did not respond)
d = droplevels(d)
length(unique(d$workerid)) #244 (data from 1 Turkers excluded)

################# Excuding Turkers based on main clause controls #################

### look at how Turkers did on the main clauses 
# main clauses
names(d)
d.MC <- subset(d, d$short_trigger == "MC")
d.MC <- droplevels(d.MC)
nrow(d.MC) #1952 (244 Turkers x 8 MCs)

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
# 6 Turkers give unusually high responses: 4, 5, 16, 105, 183, 193

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- subset(d.MC, d.MC$workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #48 (6 outlier Turkers x 8 main clauses)

# check if there's any systematicity to the items that they're outliers on

# for each Turker, plot their response to the 8 main clauses they saw
ggplot(outliers, aes(x=content,y=response)) +
  geom_point() +
  facet_wrap(~workerid)
# all 6 systematically gave high responses to the main clauses
# only 183 had one low response

# for each main clause content, plot the responses that the outlier Turkers gave to them
# do this in case the "outlier" Turkers all happened to see the same contents
ggplot(outliers, aes(x=workerid,y=response)) +
  geom_point() +
  facet_wrap(~content)
# 18 of 20 contents represented among outliers (so it's not about a subset of items)

# exclude all 6 outlier Turkers
d <- subset(d, !(d$workerid %in% ai$workerid))
d <- droplevels(d)
length(unique(d$workerid)) #238 remaining Turkers 

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

length(unique(d$workerid))
# 238 Turkers remain

################## Properties of the data #############
cd = readRDS(cd, file="data/cd.rds")

# age info
length(which(is.na(cd$age))) #everybody gave their age
table(cd$age) #18-77 
median(cd$age) #30
mean(cd$age) #31.5

# What's the distribution of contents across the triggers?
table(d$content,d$short_trigger)
# confessed discovered established found_out is_amused is_annoyed
# bmw             15         10          16        14        16         12
# chili            9         10           7        18        11         13
# cupcakes        12         15           9        11        21         10
# damp             9         12           8        11         8         13
# dancing         19          7          10        16        14         10
# ditch           14         19          16        17         8         21
# frisbee         11         15          11        14        14         12
# garage          19          7          11        12         9          7
# hat             10         12          14         8        11         16
# horse           12          8          12        11         6         12
# nails           15         11          14         7         7         16
# picture         10         11          18        15        12         16
# poem            13         13           9         5        14         18
# poodle          12         15          14        13         9         11
# pool            12          9          12        13        11         11
# purple           7         12          12        11        11          7
# swing           18         16          11         6        11          9
# tea              4         13           7        17        15          9
# yoga             8         12          14        12        14          6
# zoo              9         11          13         7        16          9
# 
# is_aware learned  MC noticed realize revealed saw
# bmw            13      12  84       8      11       12  15
# chili          15      14  80       9      14       19  19
# cupcakes        9      10  93       8      18        9  13
# damp            9      11 118      10       6        8  15
# dancing         9      18  87      15      16       11   6
# ditch          12       7  91      11       8        3  11
# frisbee         9      13  97      13       8        8  13
# garage         14      11  92      18      12       11  15
# hat            12      12  98       6      17       12  10
# horse           9      12 107      13      12       15   9
# nails           6      10 106      13       7       12  14
# picture        11       4  93      12       8       16  12
# poem           12      16  82      19      13       10  14
# poodle         11      10  94      11      14       15   9
# pool           12      15  93      20      10       11   9
# purple         14      14  96       7      16       17  14
# swing          11       9  98      15      14       11   9
# tea            19      12  97      10      14       10  11
# yoga           18      10  94       9      14       16  11
# zoo            13      18 104      11       6       12   9

################## Analyses ############################

# make a data structure tmp that includes only info relevant to the analyses
t = d %>%
  dplyr::select(response, short_trigger, content, trigger_class, workerid)
head(t)

# save data structure t 
saveRDS(t, file="data/t.rds")


############# START OF JD'S ANALYSIS CODE ######################
t <- readRDS(file="data/t.rds")

# aggregate responses by trigger for merging in means from exp 1b to plot
tagr = t %>%
  group_by(short_trigger) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# load and aggregate data from exp 1a to merge in projectivity means
t.proj <- readRDS(file="../exp1b/data/t.rds")
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

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("established","confessed","revealed","discovered","learned","found_out","saw","is_amused","realized","is_aware","noticed","is_annoyed"))

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


# aggregate responses by trigger and content for merging in means from exp 1a to plot and run regression analysis
tagr = t %>%
  group_by(short_trigger, content) %>%
  summarise(mean_ai = mean(response), ci_low_ai = ci.low(response), ci_high_ai = ci.high(response))
tagr = as.data.frame(tagr)
tagr$ci_min_ai = tagr$mean_ai - tagr$ci_low_ai
tagr$ci_max_ai = tagr$mean_ai + tagr$ci_high_ai

# load and aggregate data from exp 1a to merge in projectivity means
t.proj <- readRDS(file="../exp1b/data/t.rds")
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

nrow(means) # this leaves 260 data points for analysis (of which 20 are MCs, i.e., 240)
saveRDS(means, file="data/means.rds")

means_nomc = droplevels(means[means$short_trigger != "MC",])
nrow(means_nomc)
means_nomc$cmean_ai = myCenter(means_nomc$mean_ai)

means_nomc$Trigger = factor(x=as.character(means_nomc$short_trigger),levels=c("established","confessed","revealed","discovered","learned","found_out","saw","is_amused","realized","is_aware","noticed","is_annoyed"))

# plot it all -- this will only run if you don't load plyr
ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,color=Trigger,group=1)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  # geom_smooth(method="lm") +
  geom_point() +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0,1) +
  ylim(0,1)
ggsave(file="graphs/ai-proj-bytrigger-bycontent.pdf",width=5.7,height=4)

# plot with ai-effect smoothers by trigger
ggplot(means_nomc, aes(x=mean_ai,y=mean_proj,color=Trigger)) +
  geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_errorbar(aes(ymin=ci_min_proj,ymax=ci_max_proj),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=ci_min_ai,xmax=ci_max_ai),color="gray50",alpha=.5) +
  geom_point(alpha=.5) +
  geom_smooth(method="lm",se=F) +
  xlab("Mean not-at-issueness rating") +
  ylab("Mean projectivity rating") +
  xlim(0,1) +
  ylim(0,1)
ggsave(file="graphs/ai-proj-bytrigger-bycontent-smoothers.pdf",width=5.7,height=4)


m = lmer(mean_proj ~ cmean_ai + (1+cmean_ai|short_trigger) + (1+cmean_ai|content), data = means_nomc, REML=F)
summary(m)

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
anova(m.3,m) # p-value for by-trigger intercept -- .0001
anova(m.4,m) # p-value for by-content intercept -- .ns

# so, only include by-trigger intercepts. this is the model reported in the paper:
m.report = lmer(mean_proj ~ cmean_ai + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report)

m.report.0 = lmer(mean_proj ~ 1 + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report.0)

anova(m.report.0,m.report) # p-value for at-issueness -- ns

m.report = lmer(mean_proj ~ exp(cmean_ai) + (1|short_trigger), data = means_nomc, REML=F)
summary(m.report)

m.norandom = lm(mean_proj ~ cmean_ai, data = means_nomc)
summary(m.norandom)

ggplot(means_nomc, aes(x=mean_ai,y=mean_proj)) +
  geom_point() +
  geom_smooth(method="lm")

############# END OF JD'S ANALYSIS CODE ######################

############# Plotting ######################
t <- readRDS(t, file="data/t.rds")
nrow(t) #4760 / 20 = 238 Turkers

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
mean_nai_Exp7 <- mean_nai
saveRDS(mean_nai_Exp7, file="data/mean_nai_Exp7.rds")

t.proj$trigger_ai <-factor(t.proj$short_trigger, levels=mean_nai[order(mean_nai$response), "short_trigger"])

ggplot(t.proj, aes(x=trigger_ai, y=response)) + 
  geom_violin(trim=TRUE,scale="area",adjust=1,alpha=.5) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) 
#geom_boxplot(width=0.1,position=position_dodge(.9))
ggsave(f="graphs/violin-not-at-issueness.pdf",height=3,width=10)

ggplot(t.proj, aes(x=trigger_ai, y=response)) + 
  geom_boxplot(width=0.2,position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", color="blue", size=2,position=position_dodge(.9)) +
  theme(text = element_text(size=12)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-0.05,1.05),breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  ylab("Not-at-issueness ('asking whether')")+
  xlab("Projective content trigger")
ggsave(f="graphs/boxplot-not-at-issueness.pdf",height=3.1,width=9)

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
ggsave("graphs/ai-subjectmeans.pdf",height=3,width=9)




######## Analysis ################
library(lmerTest)

#### compare the not-at-issueness of the projective triggers

# make subset of data with only the factive verbs
table(t$trigger_class)
c = droplevels(subset(t, trigger_class == "C"))
nrow(c)#2856 (= 238 Turkers x 12 factive verbs)

# predict not-at-issueness from the trigger
triggers = lm(response ~ short_trigger, data = c)
summary(triggers) #model with confessed as reference level, comparisons to confessed

comp_nai <- glht(triggers, mcp(short_trigger="Tukey"))
summary(comp_nai)
out<-capture.output(summary(comp_nai))
cat(out,file="../analysis/comp-nai.txt",sep="\n",append=FALSE)


################## MERGE RESULTS FROM EXPERIMENTS 6 and 7 (CURRENT) ###################

### Plotting and analysis based on means 

# Current means data structure has the new not-at-issue responses for the 12 predicates and MCs
head(mean_nai_Exp7)
table(mean_nai_Exp7$short_trigger)
means.7 <- mean_nai_Exp7

# Current working directory:
#setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/7-NAI-factive-verbs/results')
# read in the means data structure from experiment 6
means.6 <- readRDS("../../6-factive-verbs/results/data/mean_proj_NAI_Exp6.rds")
table(means.6$short_trigger)
# remove main clauses
means.6 <- droplevels(subset(means.6, short_trigger != "MC"))

head(means.6)
head(means.7)

# change the column names of means.7
colnames(means.7) <- c("short_trigger", "ai_new", "New_YMin", "New_YMax")
head(means.7)

# merge means.6 and means.7
means.67 <- merge(means.6,means.7,"short_trigger")
head(means.67)

# save merged data for comparison with experiments about 9 triggers
saveRDS(means.67, file="data/means.67.rds")

## Plotting
means.67 <- readRDS("data/means.67.rds")

# plot mean not-at-issueness (2 measures) and projectivity
AP <- lm(projective ~ ai, data = means.67)
summary(AP)
AP2 <- lm(projective ~ ai_new, data = means.67)
summary(AP2)

ggplot(subset(means.67)) + 
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
AP3 <- lm(ai ~ ai_new, data = means.67)
summary(AP3)

ggplot(subset(means.67)) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 NAI x NAI= ",signif(summary(AP3)$adj.r.squared, 5),
                     "Intercept =",signif(AP3$coef[[1]],5 ),
                     " Slope =",signif(AP3$coef[[2]], 5),
                     " P =",signif(summary(AP3)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02))+
  xlab("Mean not-at-issueness ('asking whether')") +
  ylab("Mean not-at-issueness ('Are you sure?')")
ggsave(file="graphs/correlation-means-NAI-NAI.pdf",width = 14, height = 6)


################## COMPARISONS ACROSS THE FOUR EXPERIMENTS ######################

### Plot not-at-issueness results from experiments 5 and 7
# Experiment 4: projection and not-at-issueness, class B/C contents
# Experiment 5: only not-at-issueness, measured through "are you sure?" diagnostic, class B/C contents
# Experiment 6: projection and not-at-issueness, 12 factive verbs
# Experiment 7: only not-at-issueness, measured through "are you sure?" diagnostic, 12 factive verbs

# load the relevant data
NAI_5 <- readRDS("../../5-NAI/results/data/nai_new.rds")
NAI_5
NAI_5$short_trigger <- gsub("MC","MC_5",NAI_5$short_trigger) #change MC to MC_5
NAI_5

NAI_7 <- readRDS("../../7-NAI-factive-verbs/results/data/nai_new.rds")
NAI_7

# bind the two sets of data
merged_57 <- rbind(NAI_5,NAI_7)
merged_57

# reorder by mean
merged_57$short_trigger2 <-factor(merged_57$short_trigger, 
                                  levels=merged_57[order(merged_57$response), "short_trigger"])

merged_57

# plot not-at-issueness values for experiments 5 and 7
ggplot(merged_57, aes(x=short_trigger2,y=response,group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax)) +
  geom_text(aes(label=round(response*100,0)), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab("(Non-)projective contents")+
  ylab("NAIness")
ggsave(file="graphs/not-at-issueness-of-all-exp57-contents.pdf",width = 14, height = 6)



############# Comparing across all experiments ################ 
  
# Experiment 4: projection and not-at-issueness, class B/C contents
# Experiment 5: only not-at-issueness, measured through "are you sure?" diagnostic, class B/C contents
# Experiment 6: projection and not-at-issueness, 12 factive verbs
# Experiment 7: only not-at-issueness, measured through "are you sure?" diagnostic, 12 factive verbs

### load the relevant data
head(merged) # data from experiments 6 and 7
merged_67 <- merged #change name of merged file to reflect experiments it came from
merged_67$short_trigger <- gsub("MC","MC_67",merged_67$short_trigger) #change MC to MC_67
merged_67

# data from experiments 4 and 5
merged_45 <- readRDS("../../5-NAI/results/data/merged-4-and-5.rds")
head(merged_45)
table(merged_45$short_trigger)

# bind the two sets of data together
merged_4567 <- rbind(merged_45,merged_67)
merged_4567
nrow(merged_4567) #23 (45: MC + 9 triggers; 67: MC_67 + 12 triggers)

### predict projectivity from not-at-issueness
# all contents
AP <- lm(projective ~ ai, data = merged_4567)
summary(AP)
AP2 <- lm(projective ~ ai_new, data = merged_4567)
summary(AP2)

ggplot(merged_4567) + 
  geom_point(aes(x = ai, y = projective, color = "ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 ai= ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 ai-new = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means') +
  ylab('Projection response means')
ggsave(file="graphs/4567-projectivity-by-NAI-all-contents.pdf",width = 14, height = 6)

# only projective contents
AP <- lm(projective ~ ai, data = merged_4567, trigger_class != "NonProj")
summary(AP)
AP2 <- lm(projective ~ ai_new, data = merged_4567, trigger_class != "NonProj")
summary(AP2)

ggplot(subset(merged_4567, trigger_class != "NonProj")) + 
  geom_point(aes(x = ai, y = projective, color = "ai",shape="ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new",shape="ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 asking = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 sure = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02)) +
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02)) +
  theme(legend.title=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme(legend.justification=c(0,0), legend.position=c(0,0.8)) +
  theme(legend.text = element_text(colour="black", size = 16, face = "bold")) +
  scale_color_discrete(name="Not-at-issueness diagnostic",
                        breaks=c("ai", "ai_new"),
                        labels=c("Asking whether", "Are you sure?")) +
  scale_shape_discrete(name="Not-at-issueness diagnostic",
                       breaks=c("ai", "ai_new"),
                       labels=c("Asking whether", "Are you sure?")) +
  xlab('Not-at-issueness response means ("asking whether" and "are you sure?" diagnostics)') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20,face="bold")) +
  ylab('Projectivity response means')
ggsave(file="graphs/4567-projectivity-by-NAI-projective-contents.pdf",width = 14, height = 8)

# projectivity and diagnostic "asking whether" (parallel to the above plot)

ggplot(subset(merged_4567, trigger_class != "NonProj")) + 
  geom_point(aes(x = ai, y = projective, color = "ai",shape="ai")) +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  labs(title = paste("Adj R2 asking = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, 
            position=position_jitter(h=.01,w=0.02)) +
  theme(legend.title=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme(legend.justification=c(0,0), legend.position=c(0,0.8)) +
  theme(legend.text = element_text(colour="black", size = 16, face = "bold")) +
  scale_color_discrete(name="Not-at-issueness diagnostic",
                       breaks=c("ai", "ai_new"),
                       labels=c("Asking whether", "Are you sure?")) +
  scale_shape_discrete(name="Not-at-issueness diagnostic",
                       breaks=c("ai", "ai_new"),
                       labels=c("Asking whether", "Are you sure?")) +
  xlab('Not-at-issueness response means ("asking whether" diagnostic)') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20,face="bold")) +
  ylab('Projectivity response means')
ggsave(file="graphs/4567-projectivity-by-NAI-diagnostic1-projective-contents.pdf",width = 14, height = 9)



# only class C contents
AP <- lm(projective ~ ai, data = merged_4567, trigger_class == "C")
summary(AP)
AP2 <- lm(projective ~ ai_new, data = merged_4567, trigger_class == "C")
summary(AP2)

ggplot(subset(merged_4567, trigger_class == "C")) + 
  geom_point(aes(x = ai, y = projective, color = "ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 ai= ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 ai-new = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means') +
  ylab('Projection response means')
ggsave(file="graphs/4567-projectivity-by-NAI-class-C-contents.pdf",width = 14, height = 6)

# factive verbs
AP <- lm(projective ~ ai, data = merged_4567, (trigger_class == "C" 
                                               & (short_trigger != "only" 
                                               & short_trigger != "stop"
                                               & short_trigger != "stupid")))
summary(AP)
AP2 <- lm(projective ~ ai_new, data = merged_4567, (trigger_class == "C" 
                                                    & (short_trigger != "only" 
                                                       & short_trigger != "stop"
                                                       & short_trigger != "stupid")))
summary(AP2)

ggplot(subset(merged_4567, (trigger_class == "C" 
                            & (short_trigger != "only" 
                               & short_trigger != "stop"
                               & short_trigger != "stupid")))) + 
  geom_point(aes(x = ai, y = projective, color = "ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 ai= ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 ai-new = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means') +
  ylab('Projection response means')
ggsave(file="graphs/4567-projectivity-by-NAI-factive-verbs.pdf",width = 14, height = 6)

#### compare the two NAI measures by exploring their correlation
# all contents
AP <- lm(ai ~ ai_new, data = merged_4567)
summary(AP)

ggplot(merged_4567) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means (question)') +
  ylab('NAI response means ("Are you sure?")')
ggsave(file="graphs/4567-compare-NAI-measures-all-contents.pdf",width = 14, height = 6)

# projective contents
AP <- lm(ai ~ ai_new, data = merged_4567, trigger_class != "NonProj"))
summary(AP)

ggplot(subset(merged_4567, trigger_class != "NonProj")) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means (question)') +
  ylab('NAI response means ("Are you sure?")')
ggsave(file="graphs/4567-compare-NAI-measures-projective-contents.pdf",width = 12, height = 8)

# class C contents
AP <- lm(ai ~ ai_new, data = merged_4567, trigger_class == "C"))
summary(AP)

ggplot(subset(merged_4567, trigger_class == "C")) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means (question)') +
  ylab('NAI response means ("Are you sure?")')
ggsave(file="graphs/4567-compare-NAI-measures-class-C-contents.pdf",width = 14, height = 6)

# factive verbs
AP <- lm(ai ~ ai_new, data = merged_4567, (trigger_class == "C" 
                                           & (short_trigger != "only" 
                                              & short_trigger != "stop"
                                              & short_trigger != "stupid")))
summary(AP)

ggplot(subset(merged_4567, (trigger_class == "C" 
                            & (short_trigger != "only" 
                               & short_trigger != "stop"
                               & short_trigger != "stupid")))) + 
  geom_point(aes(x = ai, y = ai_new)) +
  geom_smooth(aes(x = ai, y = ai_new), method="lm", color = "red") +
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = ai_new, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means (question)') +
  ylab('NAI response means ("Are you sure?")')
ggsave(file="graphs/4567-compare-NAI-measures-factive-verbs.pdf",width = 14, height = 6)

###### Pool data from various experiments to explore differences in projectivity
###### among class C contents

# data structure t from 4-ProjAI
t_4 <- readRDS("../../4-ProjAI/results/data/t.rds")

# data structure t from 5-NAI
t_5 <- readRDS("../../5-NAI/results/data/t.rds")

# data structure t from 6-factive-verbs
t_6 <- readRDS("../../6-factive-verbs/results/data/t.rds")

# data structure t from 7-NAI-factive-verbs
t_7 <- readRDS("../../7-NAI-factive-verbs/results/data/t.rds")

# bind data structures
t_46 = rbind(t_4,t_6)
nrow(t_46)

head(t_5)
t_57 = rbind(t_5,t_7)

# load library for multiple comparisons
library(multcomp)

t_57$short_trigger = as.factor(t_57$short_trigger)

t_46$short_trigger = as.factor(t_46$short_trigger) 
contrasts(t_46$short_trigger) #confessed is reference level

# compare not-at-issueness of trigger classes in experiments 5-NAI and 7-NAI-factive verbs
triggers = lm(response ~ trigger_class, subset(t_57, trigger_class != "NonProj"))
summary(triggers) 

# compare not-at-issueness of class C triggers in experiments 5-NAI and 7-NAI-factive verbs
triggers = lm(response ~ short_trigger, subset(t_57, trigger_class == "C"))
summary(triggers) 

comp_NAI_57 <- glht(triggers, mcp(short_trigger="Tukey"))
summary(comp_NAI_57)
out<-capture.output(summary(comp_NAI_57))
cat(out,file="../analysis/comp-NAI-57s.txt",sep="\n",append=FALSE)

# compare projectivity of class C triggers
triggers = lm(projective ~ short_trigger, subset(t_46, trigger_class == "C"))
summary(triggers) 

comp_proj_all <- glht(triggers, mcp(short_trigger="Tukey"))
summary(comp_proj_all)
out<-capture.output(summary(comp_proj_all))
cat(out,file="../analysis/comp-proj-all.txt",sep="\n",append=FALSE)

# plot the mean projectivity scores for class C
merged_4567$short_trigger2 <-factor(merged_4567$short_trigger, 
                                    levels=merged_4567[order(merged_4567$projective), "short_trigger"])

ggplot(subset(merged_4567,trigger_class == "C"), 
       aes(x=short_trigger2,y=projective,group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_text(aes(label=round(projective*100,0)), vjust=1)+
  xlab("Class C projective contents")+
  ylab("Projectivity")
ggsave(file="graphs/46-projectivity-class-C-contents.pdf",width = 14, height = 6)

# plot the mean projectivity scores for the projective contents 
merged_4567$short_trigger2 <-factor(merged_4567$short_trigger, 
                                    levels=merged_4567[order(merged_4567$projective), "short_trigger"])

ggplot(subset(merged_4567,trigger_class != "NonProj"), 
         aes(x=short_trigger2,y=projective,group=1)) +
    geom_point() +
    geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
    #geom_text(aes(label=round(projective*100,0)), vjust=1)+
    theme(axis.text.x = element_text(angle=60, hjust=1))+
    theme(axis.text=element_text(size=16),
         axis.title=element_text(size=22))+
    xlab("Projective content triggers")+
    ylab("Mean certainty rating")
ggsave(file="graphs/46-projectivity-projective-contents.pdf",width = 9.5, height = 5)

# plot mean ai and projective responses by trigger class, label with trigger name
AP <- lm(projective ~ ai, data = merged_4567)
summary(AP)

ggplot(merged_4567, aes(x=ai,y=projective,color=trigger_class,group=1)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5))) +
  ylab("Projection response mean")+
  xlab("NAI response mean")
ggsave(file="graphs/46-projection-by-NAI-all-contents.pdf",width = 14, height = 6)


# excluding main clauses, plot mean ai and projective responses by trigger class, label trigger name
AP2 <- lm(projective ~ ai, data = merged_4567, trigger_class != "NonProj")
summary(AP2)

ggplot(subset(merged_4567, trigger_class != "NonProj"), 
       aes(x=ai,y=projective,color=trigger_class,group=1)) +
  geom_point() +
  geom_smooth(method="lm", col = "red") +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_errorbarh(aes(xmin=YMin, xmax=YMax)) +
  geom_text(aes(label=short_trigger), vjust = 1, cex= 5, offset = 10,
            position=position_jitter(h=.01,w=0.02))+
  labs(title = paste("Adj R2 = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  ylab("Projection response mean")+
  xlab("NAI response mean")
ggsave(file="graphs/46-projection-by-NAI-projective-contents.pdf",width = 12, height = 8)

########### Combine results from Exps 4 and 5 (class B and C contents) with cleft experiment ######

# relevant data from experiments 4 and 5
merged_45 <- readRDS("../../5-NAI/results/data/merged-4-and-5.rds")
head(merged_45)
table(merged_45$short_trigger)

# complete data from cleft experiment
clefts <- readRDS("../../../4-clefts/results/data/t.rds")
head(clefts)
names(clefts)
table(clefts$constr)
table(clefts$implication)

# make relevant subset (clefts, definite / exhaust, exist)
cd <- droplevels(subset(clefts, ((clefts$constr == "cleft" | clefts$constr == "def") &  
                        (clefts$implication == "exh" | clefts$implication == "exist"))))
nrow(cd) #4460 (4460 / 223 = 20)
length(unique(cd$workerid)) #223
head(cd)
table(cd$constr,cd$implication)
#        exh exist
# cleft 1338   892
# def   1338   892

# make data subsets for the different responses
ns <- droplevels(subset(cd, cd$embedding == "question" & cd$question_type == "nai_sure"))
na <- droplevels(subset(cd, cd$embedding == "question" & cd$question_type == "nai_ask"))
proj <- droplevels(subset(cd, cd$embedding == "question" & cd$question_type == "proj"))

# calculate mean responses and sd, and make columns fit
# nai_sure
agr = aggregate(response~constr+implication, data=ns, FUN="mean")
agr$YMin = agr$response - aggregate(response~constr+implication, data=ns, FUN="ci.low")$response
agr$YMax = agr$response + aggregate(response~constr+implication, data=ns, FUN="ci.high")$response
agr
# make the right columns
agr$short_trigger = paste(agr$constr, agr$implication)
agr$constr <- NULL
agr$implication <- NULL
agr
# change column order
agr <- agr[c(4,1,2,3)]
agr
# change column names
colnames(agr) <- c("short_trigger", "ai_new", "New_YMin", "New_YMax")
agr_ai_new = agr
agr_ai_new

# nai_ask
agr = aggregate(response~constr+implication, data=na, FUN="mean")
agr$YMin = agr$response - aggregate(response~constr+implication, data=na, FUN="ci.low")$response
agr$YMax = agr$response + aggregate(response~constr+implication, data=na, FUN="ci.high")$response
agr
# make the right columns
agr$short_trigger = paste(agr$constr, agr$implication)
agr$constr <- NULL
agr$implication <- NULL
agr
# change column order
agr <- agr[c(4,1,2,3)]
agr
# change column names
colnames(agr) <- c("short_trigger", "ai", "YMin", "YMax")
agr_ai = agr
agr_ai

# projection
agr = aggregate(response~constr+implication, data=proj, FUN="mean")
agr$YMin = agr$response - aggregate(response~constr+implication, data=proj, FUN="ci.low")$response
agr$YMax = agr$response + aggregate(response~constr+implication, data=proj, FUN="ci.high")$response
agr
# make the right columns
agr$short_trigger = paste(agr$constr, agr$implication)
agr$constr <- NULL
agr$implication <- NULL
agr
# change column order
agr <- agr[c(4,1,2,3)]
agr
# change column names
colnames(agr) <- c("short_trigger", "projective", "pYMin", "pYMax")
agr_proj = agr
agr_proj

# now that we have the right data, put it into the right format
# short_trigger trigger_class ai YMin YMax projective pYMin pYMax ai_new NewYMin New_YMax
agr_ai_new
agr_ai
agr_proj
# merge the data
cd2 <- merge(agr_ai,agr_proj,"short_trigger")
cd <- merge(cd2,agr_ai_new,"short_trigger")
head(cd)

# add trigger_class column
cd$trigger_class <- "C"
cd
cd <- cd[c(1,11,2:10)]
cd
str(cd$trigger_class)

# combine the clefts data with the data from Exps 4 and 5
clefts_45 <- rbind(merged_45,cd)
clefts_45

# calculate correlation and make plots (only projective contents)
AP <- lm(projective ~ ai, data = clefts_45, trigger_class != "NonProj")
summary(AP)
AP2 <- lm(projective ~ ai_new, data = clefts_45, trigger_class != "NonProj")
summary(AP2)

ggplot(subset(clefts_45, trigger_class != "NonProj")) + 
  geom_point(aes(x = ai, y = projective, color = "ai")) +
  geom_point(aes(x = ai_new, y = projective, color = "ai_new"))  +
  geom_smooth(aes(x = ai, y = projective), method="lm", color = "red") +
  geom_smooth(aes(x = ai_new, y = projective), method="lm", color = "blue") +
  labs(title = paste("Adj R2 ai= ",signif(summary(AP)$adj.r.squared, 5),
                     "Intercept =",signif(AP$coef[[1]],5 ),
                     " Slope =",signif(AP$coef[[2]], 5),
                     " P =",signif(summary(AP)$coef[2,4], 5),
                     "\n Adj R2 ai-new = ",signif(summary(AP2)$adj.r.squared, 5),
                     "Intercept =",signif(AP2$coef[[1]],5 ),
                     " Slope =",signif(AP2$coef[[2]], 5),
                     " P =",signif(summary(AP2)$coef[2,4], 5))) +
  geom_text(aes(x=ai, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  geom_text(aes(x=ai_new, y = projective, label=short_trigger), vjust = 1, cex= 5, offset = 10, 
            position=position_jitter(h=.01,w=0.02))+
  xlab('NAI response means') +
  ylab('Projection response means')
ggsave(file="graphs/45-clefts-projectivity-by-NAI.pdf",width = 12, height = 8)

# make plots
clefts_45

# projection
clefts_45$short_trigger2 <- factor(clefts_45$short_trigger, 
                                   levels=clefts_45[order(clefts_45$projective), "short_trigger"])

ggplot(clefts_45, aes(x=short_trigger2,y=projective,color=trigger_class,group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=pYMin, ymax=pYMax)) +
  geom_text(aes(label=round(projective*100,0)), vjust=1) +
  xlab("(Non-)projective contents")+
  ylab("Projectivity")
ggsave(file="graphs/clefts_45-projectivity.pdf",width = 8, height = 3)

# nai ask
clefts_45$short_trigger2 <- factor(clefts_45$short_trigger, 
                                   levels=clefts_45[order(clefts_45$ai), "short_trigger"])

ggplot(clefts_45, aes(x=short_trigger2,y=ai,color=trigger_class,group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax)) +
  geom_text(aes(label=round(ai*100,0)), vjust=1) +
  xlab("(Non-)projective contents")+
  ylab("Not-at-issueness (ask)")
ggsave(file="graphs/clefts_45-NAI-ask.pdf",width = 8, height = 3)

# nai sure
clefts_45$short_trigger2 <- factor(clefts_45$short_trigger, 
                                   levels=clefts_45[order(clefts_45$ai_new), "short_trigger"])

ggplot(clefts_45, aes(x=short_trigger2,y=ai_new,color=trigger_class,group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=New_YMin, ymax=New_YMax)) +
  geom_text(aes(label=round(ai_new*100,0)), vjust=1) +
  xlab("(Non-)projective contents")+
  ylab("Not-at-issueness (sure)")
ggsave(file="graphs/clefts_45-NAI-sure.pdf",width = 8, height = 3)



