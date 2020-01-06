library(readxl)
library(readr)
library(lme4) # this is the one to use for modeling
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
library(R.matlab)
library(xtable)
library(Hmisc)
library(foreign)
library(MASS)
library("lsmeans")
library(effects)
library(arm)
library(dplyr)
library(data.table)
library(zoo)
library(corrplot)
library(data.table)
library(multcompView)
library(compareGroups)

######################################
## glossary to cobra game variables ##
######################################
# gender: 1 = male, 2 = female
# gameExp: rated from 1 to 5, 1 being no/almost no experience at all (any game on computers, smartphones, tablets counts as experience)
# rankEstim 1 and 2: final rank estimated by participants after the practice round (rankEstim1) and at the end of the game (rankEstim2) (warning: rank 1 = highest!)
# avatarChoice: participants' choice of 1 out of 4 avatars they are presented with
# consentChoice: 1 = agree to be listed in the ranking for subsequent players, 0 = does not agree
# cobraLevel: cobra's speed established based on performance during practice phase. 1 = easy/slow, 2 = medium, 3 = difficult/fast
# excited, upset, scared, hostile, proud, irritable, alert, ashamed, nervous, determined: the 10 PANAS affects best related to competition (assessed both at baseline and when finishing the game)
# trial: number of rounds (max. 24)
# score: number of apples eaten on a given round/trial
# oppScore: opponent's score computed based on scoreDiff, below such as oppScore = score - scoreDiff (this variable is not displayed in the current version of the game)
# scoreDiff: rigged score differences between player and opponent
# win: a given round's/trial's outcome: 1 = victory, 0 = defeat (rigged, so same for all players!)
# close: based on scoreDiff, 1 = tight competition, 0 = big score difference between player and opponent (displayed during cobra game for the last 10 seconds of the game)
# oppName: name sequence of opponents; predefined and the same for all players (displayed at the beginning of each round/trial)
# oppRank: opponents'ranks; predefined sequence that is the same for all players (displayed at the beginning of each round/trial) (warning: rank 1 = highest!)
# appleChoice: player's choice of how many apples to take away from opponent before playing. 1 = none, 2 = 1 apple, 3 = 2 apples, 4 = 5 apples, 5 = 10 apples.
# rankChoice: player's choice of buying a booster to increase rank at the end of a given round/trial. 1 = none, 2 = + 1 rank, 3 = + 2 ranks, 4 = + 3 ranks, 5 = + 5 ranks.
# rankStart: player's rank at the end of the round/trial before booster choice (warning: rank 1 = highest!)
# rankEnd: player's rank at the end of the round/trial (after boosters) (warning: rank 1 = highest!)
# mot1-8: motivation questions, assessed on a Lickert scale going from 1 = Strongly disagree to 5 = Strongly agree; 'I wanted to perform as well as I possibly could on the task.', 'Maximizing my personal record of apples eaten was important to me.', 'I wanted to perform better than everyone else on the task.', 'I did not want to perform more poorly than everyone else on the task.', 'Attaining the highest rank among all the competitors was important to me.', 'I wanted to take revenge on people who defeated me.', 'I wanted to avoid performing less than my best on the task.', 'I wanted to ensure that I win.'
# enjoyed: how much participant enjoyed playing on a scale from 1 to 10, 1 = not at all, 10 = extremely
# satisfied: how much participant is satisified with own performance  on a scale from 1 to 10, 1 = not at all, 10 = extremely
# fair: how much participant judged the opponents' behavior as fair
# credible: manipulation check, i.e. how much participant believed that he could control the outcome of the game on a scale from 1 to 10, 1 = not at all, 10 = extremely

# household_income:  1 = a)\tLess than $25,000   2 = b)\t$25,000 - $49,999 3 = c)\t$50,000 - $74,999 4= d)\t$75,000 - $99,999 5 = e)\t$100,000 - $149,999 6 = f)\t$150,000 or above
########################################
# clear environment
rm(list=ls())
setwd("C:/Users/szuec/Dropbox/USA/Pittsburgh/GitHub/dominance2")

#  for collinearity diagnostics
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

### ANALYSIS BEGINS HERE
load('snake_tot.Rda')

## basic analytics:
par(mfrow=c(2,2))
hist(snake_tot$appleChoice)
hist(snake_tot$appleChoice_wi_0)
hist(snake_tot$rankChoice)
hist(snake_tot$rankChoice_wi_0)
hist(snake_tot$score)

hist(snake_tot$appleChoice_b_logit)
hist(snake_tot$rankChoice_b_logit)

par(mfrow=c(1,1))

#spaghetti plots
ggplot(data=snake_tot, aes(x=trial, y=appleChoice, group = ID)) +
  geom_path() + facet_wrap(~ID)
ggplot(data=snake_tot, aes(x=trial, y=rankChoice, group = ID)) +
  geom_path() + facet_wrap(~ID)


library(corrplot)
library(data.table)

chars <- snake_tot[,c('appleChoice','rankChoice','score','rankEnd.minus1','oppRank', 'close', 'win', 'gameExp', 'age.y', 'gender.y', 'hrsd_no_sui', 'ipipds_total', 'ffni_total', 'ffni_GRAN','ffni_VUL', 'bpni_total', 'bpni_GRAN', 'bpni_VUL', 'group1_5n')]
chars$close <- as.numeric(chars$close)
chars$win <- as.numeric(chars$win)
chars$gender.y <- as.numeric(chars$gender.y)
chars$group1_5n <- as.numeric(chars$group1_5n)
corrplot(cor(chars, method = "spearman", use = "na.or.complete"), method = "number")
corrplot.mixed(cor(chars, method = "spearman", use = "na.or.complete"), lower.col = "black", number.cex = 1.1)

## Table 1

chars <- as.data.frame(snake_basic[,c("age.y","gender.y","gameExp", "hrsd_no_sui", "ffni_total", "ffni_VUL", "ffni_GRAN", "bpni_total", "bpni_VUL", "bpni_GRAN", "ipipds_total")])
c1 <- compareGroups::compareGroups(chars, y = snake_basic$group1_5n, include.miss = FALSE)
t1 <- compareGroups::createTable(c1, hide.no = 0, digits = 0, show.p.mul = TRUE)
compareGroups::export2html(t1, "snake2_demographics_basic_allp.html")


#describe.by(chars,group = snake_basic$group1_5n)
c <- compareGroups(snake_basic[,c("age.y","gender.y","gameExp", "hrsd_no_sui")], subset = snake_basic$group1_5n)
tc <- createTable(c, hide.no = 0, digits = 1, show.p.mul = TRUE)
export2html(tc, "GOOD_Table1SIDP.expanded.html")

# design variables

mappleA1 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA1)
car::Anova(mappleA1, type = 'III')

mappleA1c <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot_cleaned, na.action = na.omit)
summary(mappleA1c)
car::Anova(mappleA1c, type = 'III')

mappleA1_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(score_new.minus1) + scale(rankEnd_inv.minus1) + scale(age.y) + gender.y + scale(gameExp) + scale(hrsd_no_sui) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA1_sens)
car::Anova(mappleA1_sens, type = 'III')

mappleA2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleA2)
car::Anova(mappleA2, type = 'III')

mrankA1 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA1)
car::Anova(mrankA1, type = 'III')

mrankA2 <- lmer(rankChoice_wi_0 ~ scale(trial) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankA2)
car::Anova(mrankA2, type = 'III')

mrankA2c <- lmer(rankChoice_wi_0 ~ scale(trial) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot_cleaned, na.action = na.omit)
summary(mrankA2c)
car::Anova(mrankA2c, type = 'III')

## narcissistic scales : ffni

mappleB2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ffni_total) + close.minus1 + win.minus1 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleB2)
car::Anova(mappleB2, type = 'III')

mrankB2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ffni_total) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankB2)
car::Anova(mrankB2, type = 'III')

## narcissistic scales : bpni
mappleC2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(bpni_total) + close.minus1 + win.minus1 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleC2)
car::Anova(mappleC2, type = 'III')

mrankC2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(bpni_total) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankC2)
car::Anova(mrankC2, type = 'III')

## dominance scale : ipip-ds
mappleD2 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipipds_total) + close.minus1 + win.minus1 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleD2)
car::Anova(mappleD2, type = 'III')

mappleD3 <- lmer(appleChoice_wi_0 ~ scale(trial)*scale(ipipds_total) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(ipipds_total) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleD3)
car::Anova(mappleD3, type = 'III')

mrankD2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(ipipds_total) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankD2)
car::Anova(mrankD2, type = 'III')

## hrsd
mappleE2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(hrsd_no_sui) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleE2)
car::Anova(mappleE2, type = 'III')

mrankE2 <- lmer(rankChoice_wi_0 ~ scale(trial)*scale(hrsd_no_sui) + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankE2)
car::Anova(mrankE2, type = 'III')

## hrsd*ffni
mappleF2 <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(hrsd_no_sui)*scale(ffni_total) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleF2)
car::Anova(mappleF2, type = 'III')

mappleF2_sens <- lmer(appleChoice_wi_0 ~ scale(trial) + close.minus1 + win.minus1 + scale(oppRank_inv)*scale(hrsd_no_sui)*scale(ffni_total) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + scale(age.y) + gender.y + scale(gameExp) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleF2_sens)
car::Anova(mappleF2_sens, type = 'III')

## group
mappleG2 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5n + close.minus1 + win.minus1 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleG2)
car::Anova(mappleG2, type = 'III')

emmip(mappleG2, group1_5n ~ trial, at = list(trial = c(1,12,24)), CIs = TRUE)
em_mappleG2 = emmeans(mappleG2, list(pairwise ~ group1_5n|trial), at = list(trial = c(1,12,24)))

mappleG3 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5 + close.minus1 + win.minus1*group1_5 + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleG3)
car::Anova(mappleG3, type = 'III')

mappleG4 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5 + close.minus1 + win.minus1*group1_5 + scale(oppRank_inv)*group1_5 + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleG4)
car::Anova(mappleG4, type = 'III')

mappleG5 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5n + close.minus1 + win.minus1*group1_5n + scale(oppRank_inv)*group1_5 + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mappleG5)
car::Anova(mappleG5, type = 'III')

emmip(mappleG5, group1_5n ~ oppRank_inv, at = list(oppRank_inv = c(1,100,200)), CIs = TRUE)
em_mappleG2 = emmeans(mappleG2, list(pairwise ~ group1_5n|oppRank_inv), at = list(oppRank_inv = c(1,100,200)))

emmip(mappleG5, group1_5n ~ win.minus1, CIs = TRUE, sort = FALSE)
em_mappleG2 = emmeans(mappleG2, list(pairwise ~ group1_5n|oppRank_inv), at = list(oppRank_inv = c(1,100,200)))



mrankE2 <- lmer(rankChoice_wi_0 ~ scale(trial) + group1_5n + scale(oppRank_inv) + win + scale(rankStart_inv) + scale(score_new) + scale(rankChoice_wi_0.minus1) + (1|ID),  data = snake_tot, na.action = na.omit)
summary(mrankE2)
car::Anova(mrankE2, type = 'III')

## testing groups without HC
snake_tot_noHC <- snake_tot[!snake_tot$group1_5n == "1",]

mapple_noHC_G4 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5n + close.minus1 + win.minus1*group1_5n + scale(oppRank_inv)*group1_5n + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot_noHC, na.action = na.omit)
summary(mapple_noHC_G4)
car::Anova(mapple_noHC_G4, type = 'III')

mapple_noHC_G3 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5n + close.minus1 + win.minus1*group1_5n + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot_noHC, na.action = na.omit)
summary(mapple_noHC_G3)
car::Anova(mapple_noHC_G3, type = 'III')

emmip(mapple_noHC_G3, group1_5n ~ trial, at = list(trial = c(1,12,24)), CIs = TRUE)
em_mapple_noHC_G3 = emmeans(mapple_noHC_G3, list(pairwise ~ group1_5n|trial), at = list(trial = c(1,12,24)))

emmip(mapple_noHC_G3, group1_5n ~ win.minus1, CIs = TRUE)
em_mapple_noHC_G3_2 = emmeans(mapple_noHC_G3, list(pairwise ~ group1_5n|win.minus1))

## group-narcissism : none
mapple_noHC_G3 <- lmer(appleChoice_wi_0 ~ scale(trial)*group1_5n + close.minus1 + win.minus1*group1_5n + scale(oppRank_inv) + scale(score_new.minus1) + scale(rankEnd_inv.minus1) + (1|ID),  data = snake_tot_noHC, na.action = na.omit)
summary(mapple_noHC_G3)
car::Anova(mapple_noHC_G3, type = 'III')



## basic analysis
hist(snake_basic$ffni_total)
hist(snake_basic$bpni_total)
hist(snake_basic$ipipds_total)
hist(snake_basic$appleChoice_b_logit)
hist(snake_basic$rankChoice_b_logit)

mffni <- lm(ffni_total ~ group1_5n, data = snake_basic)
summary(mffni)
car::Anova(mffni, type = 'III')

mffni2 <- lm(ffni_total ~ group1_5n + age.y + gender.y, data = snake_basic)
summary(mffni2)
car::Anova(mffni2, type = 'III')

ls_mffni2 <- lsmeans(mffni2,"group1_5n")
plot(ls_mffni2, type ~ group1_5n, horiz=F, ylab = "ffni total", xlab = "study groups")
cld(ls_mffni2, sort = FALSE)

mbpni <- lm(bpni_total ~ group1_5n, data = snake_basic)
summary(mbpni)
car::Anova(mbpni, type = 'III')

mbpni2 <- lm(bpni_total ~ group1_5n + age.y + gender.y, data = snake_basic)
summary(mbpni2)
car::Anova(mbpni2, type = 'III')

ls_mbpni2 <- lsmeans(mbpni2,"group1_5n")
plot(ls_mbpni2, type ~ group1_5n, horiz=F, ylab = "bpni total", xlab = "study groups")
cld(ls_mbpni2, sort = FALSE)


mipip <- lm(ipipds_total ~ group1_5n, data = snake_basic)
summary(mipip)
car::Anova(mipip, type = 'III')

mipip2 <- lm(ipipds_total ~ group1_5n + age.y + gender.y, data = snake_basic)
summary(mipip2)
car::Anova(mipip2, type = 'III')
