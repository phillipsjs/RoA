####   Authors: Phillips, J., Luguri, J. & Knobe, J.                  ###
####                                                                  ###
####   Title: Unifying morality's influence on non-moral judgments:   ###
####               The relevance of alternative possibilities         ###
####                                                                  ###
####   Contact: phillips01@g.harvard.edu                              ###
  
#### directory and packages #####
setwd("C:/Users/Jphil/Documents/currentProjects/Counterfactuals/Cognition_Revision")

library(tidyr)
library(mediation)
library(lsr)
se <- function(x) {sd(x)/sqrt(length(x))}

#### data and demographics ####
d.1a <- read.csv("Data/Study1a_force.csv")
d.1b <- read.csv("Data/Study1b_force.csv")
d.2a <- read.csv("Data/Study2a_causation.csv")
d.2a.1 <- read.csv("Data/Study2afn_causation.csv")
d.2b <- read.csv("Data/Study2b_causation.csv")
d.3a <- read.csv("Data/Study3a_doingVallowing.csv")
d.3b <- read.csv("Data/Study3b_doingVallowing.csv")
d.4a <- read.csv("Data/Study4a_intentionalAction.csv")
d.4b  <- read.csv("Data/Study4b_intentionalAction.csv")
d.5 <- read.csv("Data/Study5.csv")

d.1a$study <- "Study 1a"
d.1b$study <- "Study 1b"
d.2a$study <- "Study 2a"
d.2a.1$study <- "Study 2a.1"
d.2b$study <- "Study 2b"
d.3a$study <- "Study 3a"
d.3b$study <- "Study 3b"
d.4a$study <- "Study 4a"
d.4b$study <- "Study 4b"
d.5$study <- "Study 5"  

##demographic data
demog.data <- rbind(d.1a[5:12],
                    d.1b[,5:12],
                    d.2a[,5:12],
                    d.2a.1[,6:13],
                    d.2b[,5:12],
                    d.3a[,6:13], 
                    d.3b[,6:13],
                    d.4a[,5:12],
                    d.4b[,5:12],
                    d.5[,6:13])

demog.data$gender <- factor(c("Male","Female")[demog.data$gender], exclude=NULL)
demog.data$race <- factor(c("White","Black","Latino/a","Asian","Other")[demog.data$race])
demog.data$income <- factor(c("<$25,000","$25,000-$49,999","$50,000-$74,999","$75,000-$99,999",
                              "$100,000-$149,999","$150,000-$249,999",">$250,000")[demog.data$income])

demog.age <- aggregate(age~study, demog.data, FUN=function(x) c(M =mean(x), SD =sd(x)))
demog.gender <- aggregate(gender~study, demog.data, FUN=table)
print(cbind(demog.age,demog.gender[,2]))

demog.race <- aggregate(race~study, demog.data, FUN=table)
demog.income <- aggregate(income~study, demog.data, FUN=table)

#### Study 1a-b ####
#Study 1a
d.1a$condition <- factor(c("bad","neutral")[d.1a$condition])
d.1a$mediator <- 8 - d.1a$mediator ## reverse code the mediating variable so that it's cf relevance

d.1a_long <- gather(d.1a, question, response, mediator:dv) ## convert data to long format

##force judgments
aggregate(dv~condition,d.1a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="dv"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="dv"])
t.test(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="dv"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="dv"],var.equal=T)
cohensD(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="dv"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="dv"])
###t(398)= -20.004, p < .001, d = 2.000

## judgments of the relevance of the alternative
aggregate(mediator~condition,d.1a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="mediator"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="mediator"])
t.test(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="mediator"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="mediator"])
cohensD(
  d.1a_long$response[d.1a_long$condition=="bad"&d.1a_long$question=="mediator"],
  d.1a_long$response[d.1a_long$condition=="neutral"&d.1a_long$question=="mediator"])
###t(388.48)= 12.529, p < .001, d = 1.253

## mediation
b.1 <- lm(mediator ~ condition, data=d.1a)
c.1 <- lm(dv ~ condition + mediator, data=d.1a)
summary(c.1)
summary(mediate(b.1, c.1, sims=100, treat="condition", mediator="mediator", control.value="neutral", treat.value="bad"))

## reverse mediation model
b.1.1 <- lm(dv ~ condition, data=d.1a)
c.1.1 <- lm(mediator ~ condition + dv, data=d.1a)
summary(c.1.1)
summary(mediate(b.1.1, c.1.1, sims=100, treat="condition", mediator="dv", control.value="neutral", treat.value="bad"))

# Study 1b
d.1b$condition <- factor(c("counterfactual","control")[d.1b$condition])

aggregate(dv~condition,d.1b,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(d.1b$dv[d.1b$condition=="counterfactual"],
         d.1b$dv[d.1b$condition=="control"])
t.test(d.1b$dv[d.1b$condition=="counterfactual"],
       d.1b$dv[d.1b$condition=="control"], var.equal=T)
cohensD(d.1b$dv[d.1b$condition=="counterfactual"],
        d.1b$dv[d.1b$condition=="control"])
###t(104) = -2.36, p=.02, d=0.46

#### Study 2a-b ####
#Study 2a
d.2a$condition <- factor(c("bad","neutral")[d.2a$condition])
d.2a <- d.2a[-319,] ## remove the participant who didn't complete the study
d.2a$mediator <- 8 - d.2a$mediator ## reverse code the mediating variable so that it's cf relevance

d.2a_long <- gather(d.2a, question, response, mediator:dv)

## causation judgments
aggregate(dv~condition,d.2a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="dv"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="dv"])
t.test(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="dv"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="dv"],var.equal=T)
cohensD(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="dv"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="dv"])
###t(397)= 15.58, p < .001, d = 1.56

## judgments of the relevance of the alternative
aggregate(mediator~condition,d.2a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="mediator"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="mediator"])
t.test(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="mediator"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="mediator"],var.equal=T)
cohensD(
  d.2a_long$response[d.2a_long$condition=="bad"&d.2a_long$question=="mediator"],
  d.2a_long$response[d.2a_long$condition=="neutral"&d.2a_long$question=="mediator"])
###t(397)= 9.14, p < .001, d = 0.915

## mediation
b.2 <- lm(mediator ~ condition, data=d.2a)
c.2 <- lm(dv ~ condition + mediator, data=d.2a)
summary(c.2)
summary(mediate(b.2, c.2, sims=100, treat="condition", mediator="mediator", control.value="neutral", treat.value="bad"))

## reverse mediation model
b.2.1 <- lm(dv ~ condition, data=d.2a)
c.2.1 <- lm(mediator ~ condition + dv, data=d.2a)
summary(c.2.1)
summary(mediate(b.2.1, c.2.1, sims=100, treat="condition", mediator="dv", control.value="neutral", treat.value="bad"))

#study 2a.1 (Within-subjects test reported in footnote 1)
d.2a.1$condition <- factor(c("Assistant","Professor")[d.2a.1$condition])
d.2a.1$mediator <- 8 - d.2a.1$mediator ## reverse code the mediating variable so that it's cf relevance

d.2a.1_long <- gather(d.2a.1[,1:5], question, response, mediator:asstcause)

## causation judgments
aggregate(response~question,d.2a.1_long[d.2a.1_long$question!="mediator",],FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.2a.1_long$response[d.2a.1_long$question=="dv"],
  d.2a.1_long$response[d.2a.1_long$question=="asstcause"])
t.test(
  d.2a.1_long$response[d.2a.1_long$question=="dv"],
  d.2a.1_long$response[d.2a.1_long$question=="asstcause"],paired=T,var.equal=T)
cohensD(
  d.2a.1_long$response[d.2a.1_long$question=="dv"],
  d.2a.1_long$response[d.2a.1_long$question=="asstcause"])
# t(399)=20.306, p<.001, d=1.648 

## judgments of the relevance of the alternative
aggregate(response~condition,d.2a.1_long[d.2a.1_long$question=="mediator",],FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.2a.1_long$response[d.2a.1_long$condition=="Professor"&d.2a.1_long$question=="mediator"],
  d.2a.1_long$response[d.2a.1_long$condition=="Assistant"&d.2a.1_long$question=="mediator"])
t.test(
  d.2a.1_long$response[d.2a.1_long$condition=="Professor"&d.2a.1_long$question=="mediator"],
  d.2a.1_long$response[d.2a.1_long$condition=="Assistant"&d.2a.1_long$question=="mediator"],var.equal=T)
cohensD(
  d.2a.1_long$response[d.2a.1_long$condition=="Professor"&d.2a.1_long$question=="mediator"],
  d.2a.1_long$response[d.2a.1_long$condition=="Assistant"&d.2a.1_long$question=="mediator"])
# t(398)=10.47, p<.001, d=1.05

#Study 2b
d.2b$condition <- factor(c("counterfactual","control")[d.2b$condition])

aggregate(dv~condition,d.2b,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(d.2b$dv[d.2b$condition=="counterfactual"],
         d.2b$dv[d.2b$condition=="control"])
t.test(d.2b$dv[d.2b$condition=="counterfactual"],
       d.2b$dv[d.2b$condition=="control"])
cohensD(d.2b$dv[d.2b$condition=="counterfactual"],
        d.2b$dv[d.2b$condition=="control"])
###t(322.00)=3.31, p=.001, d=0.42

#### Study 3a-b ####
# Study 3a
d.3a$condition <- factor(c("bad","neutral")[d.3a$condition])
d.3a$mediator <- 8 - d.3a$mediator ## reverse code the mediating variable so that it's cf relevance

d.3a_long <- gather(d.3a, question, response, mediator:moraljudg)

## judgments of doing vs. allowing
aggregate(dv~condition,d.3a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="dv"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="dv"])
t.test(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="dv"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="dv"],var.equal=T)
cohensD(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="dv"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="dv"])
###t(397)= 7.05, p < .001, d = 0.71

## judgments of the relevance of the alternative
aggregate(mediator~condition,d.3a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="mediator"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="mediator"])
t.test(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="mediator"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="mediator"],var.equal=T)
cohensD(
  d.3a_long$response[d.3a_long$condition=="bad"&d.3a_long$question=="mediator"],
  d.3a_long$response[d.3a_long$condition=="neutral"&d.3a_long$question=="mediator"])
###t(397)= 3.79, p < .001, d = 0.379

##mediation
b.3 <- lm(mediator ~ condition, data=d.3a)
c.3 <- lm(dv ~ condition + mediator, data=d.3a)
summary(c.3)
summary(mediate(b.3, c.3, sims=100, treat="condition", mediator="mediator", control.value="neutral", treat.value="bad"))

##reverse mediation model
b.3.1 <- lm(dv ~ condition, data=d.3a)
c.3.1 <- lm(mediator ~ condition + dv, data=d.3a)
summary(c.3.1)
summary(mediate(b.3.1, c.3.1, sims=100, treat="condition", mediator="dv", control.value="neutral", treat.value="bad"))

#Study 3b
d.3b$condition <- factor(c("Alternative","Control")[d.3b$condition])

aggregate(dv~condition,d.3b, FUN=function(x) c(M =mean(x), SD =sd(x), n =length(x)))
var.test(d.3b$dv[d.3b$condition=="Alternative"],
         d.3b$dv[d.3b$condition=="Control"])
t.test(d.3b$dv[d.3b$condition=="Alternative"],
       d.3b$dv[d.3b$condition=="Control"], var.equal=T)
cohensD(d.3b$dv[d.3b$condition=="Alternative"],
        d.3b$dv[d.3b$condition=="Control"])
###t(237)=-2.09, p=.038, d=.27.

#### Study 4a-b ####
#Study 4a
d.4a$condition <- factor(c("bad","neutral")[d.4a$condition])
d.4a$mediator <- 8 - d.4a$mediator # reverse code the mediating variable so that it's cf relevance

d.4a_long <- gather(d.4a, question, response, mediator:dv)
## judgments of intentional action
aggregate(dv~condition,d.4a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="dv"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="dv"])
t.test(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="dv"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="dv"],var.equal=T)
cohensD(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="dv"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="dv"])
###t(399)= 32.10, p < .001, d = 3.21

## judgments of the relevance of the alternative
aggregate(mediator~condition,d.4a,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="mediator"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="mediator"])
t.test(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="mediator"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="mediator"],var.equal=T)
cohensD(
  d.4a_long$response[d.4a_long$condition=="bad"&d.4a_long$question=="mediator"],
  d.4a_long$response[d.4a_long$condition=="neutral"&d.4a_long$question=="mediator"])
###t(399)= 12.10, p < .001, d = 1.209

## mediation
b.4 <- lm(mediator ~ condition, data=d.4a)
c.4 <- lm(dv ~ condition + mediator, data=d.4a)
summary(c.4)
summary(mediate(b.4, c.4, sims=1000, treat="condition", mediator="mediator", control.value="neutral", treat.value="bad"))

## reverse mediation
b.4.1 <- lm(dv ~ condition, data=d.4a)
c.4.1 <- lm(mediator ~ condition + dv, data=d.4a)
summary(c.4.1)
summary(mediate(b.4.1, c.4.1, sims=100, treat="condition", mediator="dv", control.value="neutral", treat.value="bad"))

#Study 4b
d.4b$condition <- factor(c("counterfactual","control")[d.4b$condition])

aggregate(dv~condition,d.4b,FUN=function(x) c(M =mean(x), SD =sd(x)))
var.test(d.4b$dv[d.4b$condition=="counterfactual"],
         d.4b$dv[d.4b$condition=="control"])
t.test(d.4b$dv[d.4b$condition=="counterfactual"],
       d.4b$dv[d.4b$condition=="control"])
cohensD(d.4b$dv[d.4b$condition=="counterfactual"],
        d.4b$dv[d.4b$condition=="control"])
#t(226) = 2.47, p=.016, d=0.324

#### Study 5 ####
d.5$condition <- factor(c("Irrelevant Counterfactual","Relevant Counterfactual")[d.5$condition+1])
d.5$possibility_result <- factor(c("Did not alter outcome","Altered outcome")[d.5$possibility_result+1])

# all responses to causation question
aggregate(dv~condition,d.5,FUN=function(x) c(M =mean(x), SD =sd(x), n =length(x)))
var.test(d.5$dv[d.5$condition=="Irrelevant Possibility"],
         d.5$dv[d.5$condition=="Relevant Possibility"])
t.test(d.5$dv[d.5$condition=="Irrelevant Possibility"],
       d.5$dv[d.5$condition=="Relevant Possibility"])
cohensD(d.5$dv[d.5$condition=="Irrelevant Possibility"],
        d.5$dv[d.5$condition=="Relevant Possibility"])
# t(187.999)=-5.849, p<.001, d=0.761

# comparison of number of generated possibilities that involved an altered outcome
aggregate(possibility_result~condition,FUN=table,data=d.5)
d.5.1 <- matrix(c(102,30,69,49),ncol=2)
chisq.test(d.5.1) # Chi-squared =  9.3348, df = 1, p-value = 0.002248
cramersV(d.5.1) # V = 0.1932

# only responses where possibility included altered outcome
aggregate(dv~condition,d.5[d.5$possibility_result=="Altered outcome",],FUN=function(x) c(M =mean(x), SD =sd(x), n =length(x)))
var.test(d.5$dv[d.5$condition=="Irrelevant Possibility" & d.5$possibility_result=="Altered outcome"],
         d.5$dv[d.5$condition=="Relevant Possibility" & d.5$possibility_result=="Altered outcome"])
t.test(d.5$dv[d.5$condition=="Irrelevant Possibility" & d.5$possibility_result=="Altered outcome"],
       d.5$dv[d.5$condition=="Relevant Possibility" & d.5$possibility_result=="Altered outcome"])
cohensD(d.5$dv[d.5$condition=="Irrelevant Possibility" & d.5$possibility_result=="Altered outcome"],
        d.5$dv[d.5$condition=="Relevant Possibility" & d.5$possibility_result=="Altered outcome"])
# t(98.046)=-7.040, p<.001, d=1.212


d1.sum <- ddply(d.5, c("condition"), summarise,
                N    = length(dv),
                mean = mean(dv, na.rm=TRUE),
                sd   = sd(dv,na.rm=TRUE),
                se   = sd / sqrt(N) )

d1.plot <- ggplot(d1.sum, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=wes_palette("Royal1",2)) + 
  #facet_wrap(~withIn_between) +
  ylab("Professor Smith Caused Martin's failing") +
  xlab("") +
  coord_cartesian(ylim=c(1,4)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="null"
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )


d1.plot2 <- ggplot(d.5,aes(x=condition, fill=possibility_result)) +
  geom_bar(position="stack") +
  scale_fill_manual(values=wes_palette("Royal1",2)) + 
  #facet_grid( ~ Agent) +
  theme_bw() +
  xlab("") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position="null"
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )
