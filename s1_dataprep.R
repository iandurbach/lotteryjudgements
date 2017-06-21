###########################################################################
## Code to accompany "Lottery Judgments: A Philosophical and Experimental 
## Study" by Ebert, P., Smith, M., Durbach, I. Published in Philosophical
## Psychology (2017).
## Last modified: 21/6/2017
## FILE 1: Preparation of data from Survey 1
## inputs: s1v1.RData, s1v2.RData
## outputs: s1.RData (to be used be s1_analysis.R)
###########################################################################

# set working directory here
#setwd("")

# read in raw survey data
load("s1v1.RData")
load("s1v2.RData")

X1 = as.data.frame(X1)
X2 = as.data.frame(X2)

# sample sizes
n1 = nrow(X1)
n2 = nrow(X2)

# add ID variable
X1$id = 1:n1
X2$id = (n1+1):(n2+n1) 

# add binary indicator = 1 if subject shold be excluded from analysis 
X1$exclude = pmax(X1$FailPrevq,X1$FailTime3,X1$FailCheck,X1$FailSer,X1$FailEnglish)
X2$exclude = pmax(X2$FailPrevq,X2$FailTime3,X2$FailCheck,X2$FailSer,X2$FailEnglish)

# concatenate justification and confidence questions for S1
justified_X1 = c(X1$Q1,X1$Q2,X1$Q4)
confidence_X1 = c(X1$Q1conf,X1$Q2conf,X1$Q4conf)

# concatenate justification and confidence questions for S2
justified_X2 = c(X2$Q1,X2$Q3,X2$Q6,X2$Q2,X2$Q4,X2$Q7)
confidence_X2 = c(X2$Q1conf,X2$Q3conf,X2$Q6conf,X2$Q2conf,X2$Q4conf,X2$Q7conf)

# create data frame in long format for S1
Y1 = data.frame(survey = 1,
                workerid = rep(X1$WorkerId,3),
                exclude = rep(X1$exclude,3),
                task = rep(c(1,2,3),each=n1),
                taskwording = rep(2,n1),
                justified = justified_X1,
                confidence = confidence_X1,
                age = rep(X1$Age,3),
                education = rep(X1$Education,3))

# create data frame in long format for S2
Y2 = data.frame(survey = 2,
                workerid = rep(X2$WorkerId,6),
                exclude = rep(X2$exclude,6),
                task = rep(c(1,2,3),each=n2,times=2),
                taskwording = rep(c(1,2),each=n2),
                justified = justified_X2,
                confidence = confidence_X2,
                age = rep(X2$Age,6),
                education = rep(X2$Education,6))

# merge data frames from S1 and S2
Y = rbind(Y1,Y2)

# define categorical variables as such, add labels, etc
Y$workerid = factor(Y$workerid)
Y$survey = factor(Y$survey)
Y$task = factor(Y$task,levels=c(1,2,3),labels=c("Statistical","Testimonial","Mixed"))
Y$taskwording = factor(Y$taskwording, levels=c(1,2), labels=c("Very likely to have lost","Has lost"))
Y$condition = paste(Y$taskwording,Y$survey)
Y$condition = factor(Y$condition, levels=c("Has lost 1", "Has lost 2", "Very likely to have lost 2"),
                     labels=c("Has lost (U)","Has lost (P)","Very likely lost"))

# sort by workerid
Y = Y[order(Y$workerid),]

# drop participants whom violate any of the entry requirements (see above)
Y = subset(Y,exclude==0)

# create binary confidence variable = 1 if totally confident, 0 otherwise 
# note: done because confidence ratings highly skew
Y$confidence.t = ifelse(Y$confidence==5,1,0)

# construct new data frame with only those variables needed for analysis
Y = cbind.data.frame(Y$confidence.t,Y$justified,Y$task,Y$taskwording,Y$condition,Y$workerid)
colnames(Y) = c("confidence.t","justified","task","taskwording","condition","workerid")

# write data to file
save(Y, file = "s1.RData")
