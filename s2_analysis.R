###########################################################################
## Code to accompany "Lottery Judgments: A Philosophical and Experimental 
## Study" by Ebert, P., Smith, M., Durbach, I. Published in Philosophical
## Psychology (2017).
## Last modified: 21/6/2017
## FILE 4: Analysis of data from Survey 2
###########################################################################

library(lsmeans) 
library(car)
library(multcomp)
library(geepack) 
library(ggplot2)
library(dplyr)

# set working directory if you need to
# setwd()

# read in data file
load("s2.RData")

# order levels of categorical variables as desired for plots
Y$workerid = factor(Y$workerid)
Y$survey = factor(Y$survey)
Y$task = factor(Y$task,levels=c("Anchor","1:14m","1:100m","1:100m (Mafia)"))
Y$taskwording = factor(Y$taskwording, levels=c("Justified","Knowledge"))
Y$survey = factor(Y$survey, levels=c("Small","Universal"))

#### For Figure 2 and associated models, drop "Anchor" task (analyzed separately)
YY = subset(Y,((task=="1:14m")|(task=="1:100m")|(task=="1:100m (Mafia)")))
YY$task = droplevels(YY$task)

#### Figure 2 

## Left-hand plot

# Compute proportions supporting knowledge or justified belief in LP,
# by evidence type and anchor
mm <- YY %>% group_by(taskwording,survey,task) %>% dplyr::summarize(mjust = mean(justified,na.rm=T),
                                                                    sejust = sd(justified,na.rm=T)/sqrt(n()))

# Plot the proportions
fig2lhs <- ggplot(mm, aes(x = task, y = mjust, fill=survey)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + facet_grid(taskwording ~ .) +
  geom_linerange(aes(ymin=mjust-1.96*sejust,ymax=mjust+1.96*sejust),position=position_dodge(width=0.5)) +
  theme_bw(base_size=24) + ylab("Proportion agreeing") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  scale_fill_discrete(name = "Anchor") + 
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(labels=c("14m","100m","100m\n(Mafia)")) +
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

## Right-hand plot

# Compute proportions confident in responses to above knowledge or JB questions,
# by evidence type and anchor
mm <- filter(YY,!is.na(justified)) %>% group_by(justified,taskwording,survey,task) %>% dplyr::summarize(mconf = mean(confidence.t,na.rm=T),
                                                                                                        seconf = sd(confidence.t,na.rm=T)/sqrt(n()))
mm$justified = factor(mm$justified, levels=c(1,0), labels=c("Supporting","Opposing"))

fig2rhs <- ggplot(mm, aes(x = task, y = mconf, fill=survey)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + facet_grid(taskwording ~ justified) +
  geom_linerange(aes(ymin=mconf-1.96*seconf,ymax=mconf+1.96*seconf),position=position_dodge(width=0.5)) +
  theme_bw(base_size=24) + ylab("Proportion totally confident") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  scale_fill_discrete(name = "Anchor") + 
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(labels=c("14m","100m","100m\n(Mafia)")) +
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

ggsave("fig2.png", arrangeGrob(fig2lhs, fig2rhs,ncol=2),width=15,height=7.5,dpi=200)

#### Model fitting

# model 1 (see Table 2): response ~ type of evidence + epi status + anchor
# used for result 1, 2, 4
m1 <- geeglm(justified ~ task * survey + taskwording * survey,id = workerid, data=YY, family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~survey*taskwording, adjust="sidak"))
gee_cont1
# only interested in 6 comparisons, so adjust p
gee_cont = summary(lsmeans(m1, pairwise~survey*taskwording, adjust="none"))
gee_cont$contrasts$p.adj = round(1 - (1-gee_cont$contrasts$p.value)^6,4)
gee_cont$contrasts[,c(1,5,7)]

# model 2 (see Table 2): confidence ~ response + type of evidence + epi status + anchor
# used for result 1, 2, 4
m1 <- geeglm(confidence.t ~ factor(justified) * taskwording + task + survey,id = workerid, data=YY[complete.cases(YY),], family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~task, adjust="sidak"))
gee_cont1

#### For Figure 3 and associated models, keep "Anchor" task only (analyzed separately)
YY = subset(Y,task=="Anchor")
YY$task = droplevels(YY$task)

#### Figure 3

## Left-hand plot

# Compute proportions supporting knowledge or justified belief in LP, by anchor
mm <- YY %>% group_by(taskwording,survey) %>% dplyr::summarize(mjust = mean(justified,na.rm=T),
                                                               sejust = sd(justified,na.rm=T)/sqrt(n()))

# Plot the proportions
fig3lhs <- ggplot(mm, aes(x = taskwording, y = mjust, fill = survey )) + 
  geom_bar(stat="identity",position="dodge",width=0.5) +
  geom_linerange(aes(ymin=mjust-1.96*sejust,ymax=mjust+1.96*sejust),position=position_dodge(width=0.5)) +
  theme_bw(base_size=24) + ylab("Proportion agreeing") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  scale_fill_discrete(name = "Anchor") + 
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(labels=c("Justified","Knowledge")) +
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

## Right-hand plot

# Compute proportions confident in responses to above knowledge or JB questions,
# by anchor
mm <- filter(YY,!is.na(justified)) %>% group_by(justified,taskwording,survey) %>% dplyr::summarize(mconf = mean(confidence.t,na.rm=T),
                                                                                                   seconf = sd(confidence.t,na.rm=T)/sqrt(n()))
mm$justified = factor(mm$justified, levels=c(1,0), labels=c("Supporting","Opposing"))

# Plot the proportions
fig3rhs <- ggplot(mm, aes(x = taskwording, y = mconf, fill=survey)) + 
  geom_bar(stat="identity",position="dodge",width=0.5) + facet_grid(. ~ justified) +
  geom_linerange(aes(ymin=mconf-1.96*seconf,ymax=mconf+1.96*seconf),position=position_dodge(width=0.5)) +
  theme_bw(base_size=24) + ylab("Proportion totally confident") + xlab("") +
  theme(axis.text=element_text(size=20),axis.title=element_text(size=18)) +
  scale_fill_discrete(name = "Anchor") + 
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(labels=c("Justified","Knowledge")) +
  theme(legend.position = "bottom",legend.text=element_text(size=18),
        legend.title=element_text(size=18))

ggsave("fig3.png", arrangeGrob(fig3lhs, fig3rhs,ncol=2),width=15,height=7.5,dpi=200)

#### Model fitting

# model 1 (see Table 3): response ~ type of evidence + epi status + anchor
# used for result 3
m1 <- geeglm(justified ~ survey *taskwording  ,id = workerid, data=YY, family=binomial, corstr="independence")
summary(m1)
anova(m1)
# only interested in 4 comparisons, so adjust p
gee_cont1 = summary(lsmeans(m1, pairwise~survey*taskwording, adjust="none"))
gee_cont1
gee_cont1$contrasts$p.adj = round(1 - (1-gee_cont1$contrasts$p.value)^4,4)
gee_cont1$contrasts[,c(1,5,7)]

# model 2 (see Table 3): confidence ~ response + type of evidence + epi status + anchor
# used for result 3
m1 <- geeglm(confidence.t ~ taskwording + survey + factor(justified)*taskwording + taskwording*survey,id = workerid, data=YY[complete.cases(YY),], family=binomial, corstr="independence")
summary(m1)
anova(m1)
gee_cont1 = summary(lsmeans(m1, pairwise~taskwording*survey, adjust="sidak"))
gee_cont1
# only interested in 2 comparisons, so adjust p
gee_cont1 = summary(lsmeans(m1, pairwise~taskwording*justified, adjust="none"))
gee_cont1
gee_cont1$contrasts$p.adj = round(1 - (1-gee_cont1$contrasts$p.value)^2,4)
gee_cont1$contrasts[,c(1,5,7)]
