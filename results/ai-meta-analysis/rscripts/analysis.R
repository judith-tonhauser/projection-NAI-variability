# set working directory
setwd('/Users/titlis/cogsci/projects/stanford/projects/projection-NAI-variability/results/ai-meta-analysis/')

## JT working directory
setwd('/Users/tonhauser.1/Documents/current-research-topics/NSF-NAI/prop-att-experiments/1factive-verbs/Git-variability/results/ai-meta-analysis/')

# load required packages
require(tidyverse)
library(ggrepel)

# load helper functions
source('../helpers.R')

# read in the data
exp1a = read.csv(file="../exp1a/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  spread(question_type,response) %>%
  select(ai,short_trigger,content) %>%
  mutate(SubExperiment="a")
exp1b = read.csv(file="../exp1b/data/data_preprocessed.csv") %>%
  select(workerid,content,short_trigger,question_type,response) %>%
  spread(question_type,response) %>%
  select(ai,short_trigger,content) %>%
  mutate(SubExperiment="b")
exp2a = read.csv(file="../exp2a/data/data_preprocessed.csv") %>%
  select(response,short_trigger,content) %>%
  mutate(SubExperiment="a")
exp2b = read.csv(file="../exp2b/data/data_preprocessed.csv") %>%
  select(response,short_trigger,content) %>%
  mutate(SubExperiment="b")
nrow(exp1a)
nrow(exp1b)
nrow(exp2a)
nrow(exp2b)

# change trigger name to short version
str(exp1b$short_trigger)
str(exp2b$short_trigger)
exp1b$short_trigger <- as.factor(exp1b$short_trigger)

table(exp1b$short_trigger)
table(exp2b$short_trigger)

levels(exp1b$short_trigger) <- c('confess','discover','establish','find_out','amused','annoyed','aware','learn','MC','notice','realize','reveal','see')
levels(exp2b$short_trigger) <- c('confess','discover','establish','find_out','amused','annoyed','aware','learn','MC','notice','realize','reveal','see')

# exp1a$Experiment = "1"
# exp1b$Experiment = "1"
# exp2a$Experiment = "2"
# exp2b$Experiment = "2"

exp1 = rbind(exp1a,exp1b)
exp2 = rbind(exp2a,exp2b)

agr1 = exp1 %>%
  group_by(short_trigger,SubExperiment) %>%
  summarise(mean_exp1 = mean(ai), ci.low_exp1=ci.low(ai), ci.high_exp1=ci.high(ai))
agr2 = exp2 %>%
  group_by(short_trigger,SubExperiment) %>%
  summarise(mean_exp2 = mean(response), ci.low_exp2=ci.low(response), ci.high_exp2=ci.high(response))

d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .62
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

cbPalette <- c("#0072B2", "#D55E00")

# figure 11
ggplot(d, aes(x=mean_exp1,y=mean_exp2,color=SubExperiment)) +
  geom_abline(intercept=0,slope=1, color="gray", linetype="dashed") +
  geom_text_repel(aes(label=short_trigger),nudge_x=-.05,size=4,show.legend=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.8,color="gray") +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),alpha=.8,color="gray") +
  geom_point() +
  scale_color_manual(values=cbPalette,name="Sub-experiment") +
  xlab("Mean not-at-issueness ('asking whether', Exp. 1)") +
  ylab("Mean not-at-issueness ('are you sure', Exp. 2)") +
  xlim(.3,1) +
  ylim(.3,1) +
  theme(legend.position = "top",axis.title = element_text(size=14),legend.text = element_text(size=14),legend.title=element_text(size=14))
ggsave("graphs/correlation-bytrigger.pdf",width=6,height=6)


# aggregate by both trigger and content
agr1 = exp1 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp1 = mean(ai), ci.low_exp1=ci.low(ai), ci.high_exp1=ci.high(ai))
agr2 = exp2 %>%
  group_by(short_trigger,content,SubExperiment) %>%
  summarise(mean_exp2 = mean(response), ci.low_exp2=ci.low(response), ci.high_exp2=ci.high(response))

d = agr1 %>%
  inner_join(agr2,by=c("short_trigger","content","SubExperiment"))
nrow(d)
d = as.data.frame(d) %>%
  filter(short_trigger != "MC") %>%
  mutate(YMin=mean_exp2 - ci.low_exp2, YMax=mean_exp2 + ci.high_exp2, XMin=mean_exp1 - ci.low_exp1, XMax=mean_exp1+ci.high_exp1)

# correlations overall and by sub-experiment
cor(d$mean_exp1,d$mean_exp2) # .31
d %>%
  group_by(SubExperiment) %>%
  summarise(r=cor(mean_exp1,mean_exp2))

d$cmean_exp1 = myCenter(d$mean_exp1)
d$SubExp = ifelse(d$SubExperiment == "a",0,1)
d$cSubExperiment = myCenter(d$SubExp)

# how much does exp 1 measure explain exp 2 measure? (footnote 13)
m = lmer(mean_exp2 ~ 1+cmean_exp1 * cSubExperiment + (1|content), data = d)
summary(m)

m.simple = lmer(mean_exp2 ~ 1+cmean_exp1 * SubExperiment - cmean_exp1 + (1|content), data = d)
summary(m.simple)


