###########################################################################
## Code to accompany "Lottery Judgments: A Philosophical and Experimental 
## Study" by Ebert, P., Smith, M., Durbach, I. Published in Philosophical
## Psychology (2017).
## Last modified: 21/6/2017
## FILE 2: Analysis of data from Survey 1
###########################################################################

library(lsmeans) 
library(car)
library(multcomp)
library(geepack) 
library(ggplot2)
library(dplyr)
library(gridExtra)

# set working directory if you need to
# setwd()

# read in data file
load("s1.RData")

# order levels of categorical variables as desired for plots
Y$workerid = factor(Y$workerid)
Y$task = factor(Y$task,levels=c("Statistical","Testimonial","Mixed"))
Y$taskwording = factor(Y$taskwording, levels=c("Very likely to have lost","Has lost"))
Y$condition = factor(Y$condition, levels=c("Has lost (U)","Has lost (P)","Very likely lost"))

# for model 1 and 3, drop PLP and only look at LP (LP and PLP differences analyzed in model 2 and 4)
Yt = subset(Y,condition!="Very likely lost")
Yt$condition = droplevels(Yt$condition)

#### Figure 1 

## Left-hand plot

# Compute proportions supporting justified belief in LP and PLP by evidence type
mm <- Y %>% group_by(task,condition) %>% dplyr::summarize(mjust = mean(justified,na.rm=T),
                                                          sejust = sd(justified,na.rm=T)/sqrt(n()))

# Plot the proportions
fig1lhs <- ggplot(mm, aes(x = task, y = mjust, fill=condition)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + theme_bw(base_size=20) + 
  geom_linerange(aes(ymin=mjust-1.96*sejust,ymax=mjust+1.96*sejust),position=position_dodge(width=0.5)) +
  ylab("Proportion justified") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_discrete(name = "Response") + 
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

## Right-hand plot

# Compute proportions confident in responses to LP and PLP questions, by evidence type
mm <- filter(Y,!is.na(justified)) %>% group_by(justified,task,condition) %>% dplyr::summarize(mconf = mean(confidence.t,na.rm=T),
                                                                                              seconf = sd(confidence.t,na.rm=T)/sqrt(n()))
# Better labels for plot
mm$justified = factor(mm$justified, levels=c(1,0), labels=c("Justified","Not justified"))

# Plot the proportions
fig1rhs <- ggplot(mm, aes(x = task, y = mconf, fill=condition)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + facet_grid(justified ~ .) +
  geom_linerange(aes(ymin=mconf-1.96*seconf,ymax=mconf+1.96*seconf),position=position_dodge(width=0.5)) +
  theme_bw(base_size=24) + ylab("Proportion totally confident") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  scale_fill_discrete(name = "Response") + 
  coord_cartesian(ylim=c(0,1)) +
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

ggsave("fig1.png", arrangeGrob(fig1lhs, fig1rhs,ncol=2),width=15,height=7.5,dpi=200)

#### Model fitting

# model 1 (see Table 1): response ~ type of evidence + priming
# used for result 2, 4
m1 <- geeglm(justified ~ task * condition ,id = workerid, data=Yt, family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise ~ task, adjust="sidak"))
gee_cont1

# model 2 (see Table 1): response ~ type of evidence + proposition
# used for result 3
m1 <- geeglm(justified ~ task * taskwording ,id = workerid, data=Y, family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~task, adjust="sidak"))
gee_cont1

# model 3 (see Table 1): confidence ~ response + type of evidence + priming
# used for result 2, 4
m1 <- geeglm(confidence.t ~ factor(justified) + condition + factor(justified)*task + condition*task,id = workerid, data=Yt[complete.cases(Yt),], family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~task, adjust="sidak"))
gee_cont1
# only interested in 6 comparisons, so adjust p
gee_cont1 = summary(lsmeans(m1, pairwise~factor(justified)*task, adjust="none"))
gee_cont1$contrasts$p.adj = round(1 - (1-gee_cont1$contrasts$p.value)^6,4)
gee_cont1$contrasts[,c(1,5,7)]

gee_cont1 = summary(lsmeans(m1, pairwise~factor(justified)*condition, adjust="sidak"))
gee_cont1

# model 4 (see Table 1): confidence ~ response + type of evidence + proposition
# result 1, 3
m1 <- geeglm(confidence.t ~ factor(justified)*task + factor(justified)*taskwording ,id = workerid, data=Y[complete.cases(Y),], family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~factor(justified)*taskwording, adjust="sidak"))
gee_cont1

