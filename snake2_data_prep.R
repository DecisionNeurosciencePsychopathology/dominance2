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

setwd("C:/Users/szuec/Dropbox/USA/Pittsburgh/GitHub/dominance2/data")



## create .Rda file by combining the participants' output files. Skip this part until line 317 if the dataset has already been created)
#the following command reads a table with the participants'ids, which are also their output files' names (the files are named [id].csv); this file should be in the working directory
part <- read.csv("IDlist_full.csv")
names(part) <- c("participants", "participants_full")
part$participants_full <- as.character(part$participants_full)

snake_ds <- read.csv(part$participants_full[1], header = TRUE, sep = ';')
snake_ds[1:nrow(snake_ds)-1, (ncol(snake_ds)-22):ncol(snake_ds)] = snake_ds[nrow(snake_ds), (ncol(snake_ds)-22):ncol(snake_ds)]
snake_ds$n <- 1


for (i in 2:(nrow(part)))
{
  prov <- read.csv(part$participants_full[i], header = TRUE, sep = ';')
  prov[1:nrow(prov)-1, (ncol(prov)-22):ncol(prov)] = prov[nrow(prov), (ncol(prov)-22):ncol(prov)]
  prov$n = i
  snake_ds <- bind_rows(snake_ds, prov, .id = NULL)
}


snake_ds$name <- as.factor(snake_ds$name)
snake_ds$n <- as.factor(snake_ds$n)
snake_ds$ID <- as.factor(snake_ds$ID)

table(snake_ds$ID)

setwd("C:/Users/szuec/Dropbox/USA/Pittsburgh/GitHub/dominance2")
# people with less trials: 11019 with 5 trials, 42318 with 17 trials, 210100 with 14 trials, 440072 with 12 trials

# change classes of categorical variables
summary(snake_ds)

snake_ds <- transform(snake_ds, gender = as.factor(gender), win = as.factor(win), close = as.factor(close), avatarChoice = as.factor(avatarChoice), consentChoice = as.factor(consentChoice))

# rectify some variables (score contains the same trial's apple choice due to interface-related reasons ; opponent's rank and others rank measures should be in the increasing direction to avoid confusion)
snake_ds$oppRank_inv <- 201 - snake_ds$oppRank
snake_ds$rankStart_inv <- 201 - snake_ds$rankStart
snake_ds$rankEnd_inv <- 201 - snake_ds$rankEnd
snake_ds$score_new <- snake_ds$score - snake_ds$appleChoice

# add additional useful variables & create lagged variables
snake_ds <- snake_ds %>% group_by(ID) %>% mutate(score.minus1 = lag(score, n=1, order_by=trial),
                                         score.minus2 = lag(score, n=2, order_by=trial),
                                         score_new.minus1 = lag(score_new, n=1, order_by=trial),
                                        score_new.minus2 = lag(score_new, n=2, order_by=trial),
                                         win.minus1 = lag(win, n=1, order_by = trial),
                                     win.minus2 = lag(win, n=2, order_by = trial),
                                     scoreDiff.minus1 = lag(scoreDiff, n=1, order_by = trial),
                                     scoreDiff.minus2 = lag(scoreDiff, n=2, order_by = trial),
                                     appleChoice.minus1 = lag(appleChoice, n=1, order_by=trial),
                                     appleChoice.minus2 = lag(appleChoice, n=2, order_by=trial),
                                     rankChoice.minus1 = lag(rankChoice, n=1, order_by=trial),
                                     rankChoice.minus2 = lag(rankChoice, n=2, order_by=trial),
                                     close.minus1 = lag(close, n=1, order_by=trial),
                                     close.minus2 = lag(close, n=2, order_by=trial),
                                     rankEnd.minus1 = lag(rankEnd, n=1, order_by=trial),
                                     rankEnd.minus2 = lag(rankEnd, n=2, order_by=trial),
                                     rankStart.minus1 = lag(rankStart, n=1, order_by=trial),
                                     rankStart.minus2 = lag(rankStart, n=2, order_by=trial),
                                     rankEnd_inv.minus1 = lag(rankEnd_inv, n=1, order_by=trial),
                                     rankEnd_inv.minus2 = lag(rankEnd_inv, n=2, order_by=trial),
                                     rankStart_inv.minus1 = lag(rankStart_inv, n=1, order_by=trial),
                                     rankStart_inv.minus2 = lag(rankStart_inv, n=2, order_by=trial),
                                     oppRank.minus1 = lag(oppRank, n=1, order_by=trial),
                                     oppRank.minus2 = lag(oppRank, n=2, order_by=trial),
                                     oppRank_inv.minus1 = lag(oppRank_inv, n=1, order_by=trial),
                                     oppRank_inv.minus2 = lag(oppRank_inv, n=2, order_by=trial))
                                
snake_ds$appleChoiceDelta <- snake_ds$appleChoice - snake_ds$appleChoice.minus1
snake_ds$rankChoiceDelta <- snake_ds$rankChoice - snake_ds$rankChoice.minus1
snake_ds$scoreDelta <- snake_ds$score_new - snake_ds$score_new.minus1

snake_ds <- snake_ds %>% group_by(ID) %>% mutate(appleChoiceDelta.minus1 = lag(appleChoiceDelta, n=1, order_by=trial),
                                       appleChoiceDelta.minus2 = lag(appleChoiceDelta, n=2, order_by=trial),
                                       rankChoiceDelta.minus1 = lag(rankChoiceDelta, n=1, order_by=trial),
                                       rankChoiceDelta.minus2 = lag(rankChoiceDelta, n=2, order_by=trial),
                                       scoreDelta.minus1 = lag(scoreDelta, n=1, order_by=trial),
                                       scoreDelta.minus2 = lag(scoreDelta, n=2, order_by=trial))

snake_ds$rankGain_initial <- snake_ds$rankStart - snake_ds$rankEnd.minus1
snake_ds$rankGain_final <- snake_ds$rankEnd - snake_ds$rankEnd.minus1

#adding within- and between-subject means:
scale_this <- function(x) as.vector(scale(x))

## mean-centering variables
snake_ds = snake_ds %>% group_by(ID) %>% 
  mutate(
    appleChoice_wi = scale_this(appleChoice),
    appleChoice_b = mean(appleChoice, na.rm = TRUE),
    rankChoice_wi = scale_this(rankChoice),
    rankChoice_b = mean(rankChoice, na.rm = TRUE)
  ) %>% ungroup()


snake_ds$appleChoice_b_logit <- psych::logit(snake_ds$appleChoice_b/6.25)
snake_ds$rankChoice_b_logit <- psych::logit(snake_ds$rankChoice_b/6.25)

snake_ds$appleChoice_wi_0 <- snake_ds$appleChoice_wi
snake_ds$appleChoice_wi_0[is.nan(snake_ds$appleChoice_wi_0)] <- 0

snake_ds$rankChoice_wi_0 <- snake_ds$rankChoice_wi
snake_ds$rankChoice_wi_0[is.nan(snake_ds$rankChoice_wi_0)] <- 0


snake_ds <- snake_ds %>% group_by(ID) %>% mutate(rankChoice_wi_0.minus1 = lag(rankChoice_wi_0, n=1, order_by=trial),
                                                     rankChoice_wi_0.minus2 = lag(rankChoice_wi_0, n=2, order_by=trial),
                                                     appleChoice_wi_0.minus1 = lag(appleChoice_wi_0, n=1, order_by = trial),
                                                     appleChoice_wi_0.minus2 = lag(appleChoice_wi_0, n=2, order_by = trial))



save(snake_ds, file="snake_ds.Rda")


# preparing and adding the dataset with demographic info and narcissistic questionnaires
snake_suppl <- read.csv("data/snake_1218.csv")

names(snake_suppl)
snake_suppl <- transform(snake_suppl, ID = as.factor(ID), group1_5 = as.factor(registration_group), gender = as.factor(registration_gender), dob = as.Date(registration_dob),
                         hrsd = as.numeric(ham_hamtotal_17items), hrsd_no_sui = as.numeric(ham17minus3))

#creating a group variable with numbers
snake_suppl$group1_5n <- NA
snake_suppl$group1_5n[snake_suppl$group1_5 == 'HC'] <- '1'
snake_suppl$group1_5n[snake_suppl$group1_5 == 'DEP'] <- '2'
snake_suppl$group1_5n[snake_suppl$group1_5 == 'IDE'] <- '4'
snake_suppl$group1_5n[snake_suppl$group1_5 == 'ATT'] <- '5'

table(snake_suppl$group1_5)
table(snake_suppl$group1_5n)

## deriving ffni scores

#verifying that the reverse-keyed items had to be inverted
chars <- snake_suppl[,c('ffni_1','ffni_2','ffni_3','ffni_4','ffni_5','ffni_6','ffni_7','ffni_8','ffni_9','ffni_10','ffni_11','ffni_12','ffni_13','ffni_14','ffni_15','ffni_16','ffni_17','ffni_18','ffni_19','ffni_20','ffni_21','ffni_22','ffni_23','ffni_24','ffni_25','ffni_26','ffni_27','ffni_28','ffni_29','ffni_30','ffni_31','ffni_32','ffni_33','ffni_34','ffni_35','ffni_36', 'ffni_37', 'ffni_38', 'ffni_39', 'ffni_40', 'ffni_41','ffni_42','ffni_43','ffni_44','ffni_45','ffni_46','ffni_47','ffni_48','ffni_49','ffni_50','ffni_51','ffni_52','ffni_53','ffni_54','ffni_55','ffni_56','ffni_57','ffni_58','ffni_59','ffni_60')]
corrplot(cor(chars, method = "pearson", use = "na.or.complete"), method = "color")

#reverse-keyed items
snake_suppl$ffni_19r <- 6 -snake_suppl$ffni_19
snake_suppl$ffni_27r <- 6 -snake_suppl$ffni_27
snake_suppl$ffni_38r <- 6 -snake_suppl$ffni_38

#ffni facets
snake_suppl$ffni_acclaim_seeking <- rowSums(snake_suppl[,c("ffni_1", "ffni_16", "ffni_31", "ffni_46")], na.rm = FALSE)
snake_suppl$ffni_arrogance <- rowSums(snake_suppl[,c("ffni_2", "ffni_17", "ffni_32", "ffni_47")], na.rm = FALSE)
snake_suppl$ffni_authoritativeness <- rowSums(snake_suppl[,c("ffni_3", "ffni_18", "ffni_33", "ffni_48")], na.rm = FALSE)
snake_suppl$ffni_distrust <- rowSums(snake_suppl[,c("ffni_4", "ffni_19r", "ffni_34", "ffni_49")], na.rm = FALSE)
snake_suppl$ffni_entitlement <- rowSums(snake_suppl[,c("ffni_5", "ffni_20", "ffni_35", "ffni_50")], na.rm = FALSE)
snake_suppl$ffni_exhibitionism <- rowSums(snake_suppl[,c("ffni_6", "ffni_21", "ffni_36", "ffni_51")], na.rm = FALSE)
snake_suppl$ffni_exploitativeness <- rowSums(snake_suppl[,c("ffni_7", "ffni_22", "ffni_37", "ffni_52")], na.rm = FALSE)
snake_suppl$ffni_grandiose_fantasies <- rowSums(snake_suppl[,c("ffni_8", "ffni_23", "ffni_38r", "ffni_53")], na.rm = FALSE)
snake_suppl$ffni_indifference <- rowSums(snake_suppl[,c("ffni_9", "ffni_24", "ffni_39", "ffni_54")], na.rm = FALSE)
snake_suppl$ffni_lack_of_empathy <- rowSums(snake_suppl[,c("ffni_10", "ffni_25", "ffni_40", "ffni_55")], na.rm = FALSE)
snake_suppl$ffni_manipulativeness <- rowSums(snake_suppl[,c("ffni_11", "ffni_26", "ffni_41", "ffni_56")], na.rm = FALSE)
snake_suppl$ffni_need_for_admiration <- rowSums(snake_suppl[,c("ffni_12", "ffni_27r", "ffni_42", "ffni_57")], na.rm = FALSE)
snake_suppl$ffni_reactive_anger <- rowSums(snake_suppl[,c("ffni_13", "ffni_28", "ffni_43", "ffni_58")], na.rm = FALSE)
snake_suppl$ffni_shame <- rowSums(snake_suppl[,c("ffni_14", "ffni_29", "ffni_44", "ffni_59")], na.rm = FALSE)
snake_suppl$ffni_thrill_seeking <- rowSums(snake_suppl[,c("ffni_15", "ffni_30", "ffni_45", "ffni_60")], na.rm = FALSE)

snake_suppl$ffni_indifference_r <- 24 - snake_suppl$ffni_indifference 

snake_suppl$ffni_total <- rowSums(snake_suppl[,c("ffni_indifference", "ffni_exhibitionism", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_acclaim_seeking", "ffni_thrill_seeking","ffni_reactive_anger", "ffni_shame", "ffni_need_for_admiration", "ffni_distrust")], na.rm = FALSE)
snake_suppl$ffni_VUL <- rowSums(snake_suppl[,c("ffni_reactive_anger", "ffni_shame", "ffni_need_for_admiration", "ffni_distrust")], na.rm = FALSE)
snake_suppl$ffni_GRAN <- rowSums(snake_suppl[,c("ffni_indifference", "ffni_exhibitionism", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_acclaim_seeking", "ffni_thrill_seeking")], na.rm = FALSE)

#three-factor model based on Miller et al. 2014
snake_suppl$ffni_ANTAGONISM <- rowSums(snake_suppl[,c("ffni_manipulativeness", "ffni_exploitativeness", "ffni_entitlement", "ffni_lack_of_empathy", "ffni_arrogance", "ffni_reactive_anger", "ffni_distrust", "ffni_thrill_seeking")], na.rm = FALSE)
snake_suppl$ffni_EXTRAVERSION <- rowSums(snake_suppl[,c("ffni_acclaim_seeking", "ffni_authoritativeness", "ffni_grandiose_fantasies", "ffni_exhibitionism")], na.rm = FALSE)
snake_suppl$ffni_NEUROTICISM <- rowSums(snake_suppl[,c("ffni_shame", "ffni_indifference_r", "ffni_need_for_admiration")], na.rm = FALSE)


## deriving bpni scores
snake_suppl$bpni_exploitativeness <- rowSums(snake_suppl[,c("bpni_1", "bpni_4", "bpni_6", "bpni_11")], na.rm = FALSE)
snake_suppl$bpni_self_sacrificing_self_enhancement <- rowSums(snake_suppl[,c("bpni_10", "bpni_12", "bpni_19", "bpni_24")], na.rm = FALSE)
snake_suppl$bpni_grandiose_fantasy <- rowSums(snake_suppl[,c("bpni_13", "bpni_17", "bpni_25", "bpni_26")], na.rm = FALSE)

snake_suppl$bpni_contingent_self_esteem <- rowSums(snake_suppl[,c("bpni_2", "bpni_16", "bpni_18", "bpni_21")], na.rm = FALSE)
snake_suppl$bpni_hiding_the_self <- rowSums(snake_suppl[,c("bpni_3", "bpni_15", "bpni_27", "bpni_28")], na.rm = FALSE)
snake_suppl$bpni_devaluing <- rowSums(snake_suppl[,c("bpni_7", "bpni_9", "bpni_14", "bpni_20")], na.rm = FALSE)
snake_suppl$bpni_entitlement_rage <- rowSums(snake_suppl[,c("bpni_5", "bpni_8", "bpni_22", "bpni_23")], na.rm = FALSE)

snake_suppl$bpni_GRAN <- rowSums(snake_suppl[,c("bpni_exploitativeness", "bpni_self_sacrificing_self_enhancement", "bpni_grandiose_fantasy")], na.rm = FALSE)
snake_suppl$bpni_VUL <- rowSums(snake_suppl[,c("bpni_contingent_self_esteem", "bpni_hiding_the_self", "bpni_devaluing", "bpni_entitlement_rage")], na.rm = FALSE)
snake_suppl$bpni_total <- rowSums(snake_suppl[,c("bpni_GRAN", "bpni_VUL")], na.rm = FALSE)

## deriving ipip-ds scores

#verifying that the reverse-keyed items had to be inverted
chars <- snake_suppl[,c('ipipds_1','ipipds_2','ipipds_3','ipipds_4','ipipds_5','ipipds_6','ipipds_7','ipipds_8','ipipds_9','ipipds_10','ipipds_11')]
corrplot(cor(chars, method = "pearson", use = "na.or.complete"), method = "color")

#reversed-keyed item 2:
snake_suppl$ipipds_2r <- 6 -snake_suppl$ipipds_2

#total score
snake_suppl$ipipds_total <- rowSums(snake_suppl[,c('ipipds_1','ipipds_2r','ipipds_3','ipipds_4','ipipds_5','ipipds_6','ipipds_7','ipipds_8','ipipds_9','ipipds_10','ipipds_11')], na.rm = FALSE)

## household_income
snake_suppl$household_income <- NA
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "$5000-11999"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"
snake_suppl$household_income[snake_suppl$macarthur_6 == 1] <- "< $5000"

save(snake_suppl,file="snake_suppl.Rda")

##
load('snake_ds.Rda')
load('snake_suppl.Rda')

## missingness in demographic data:
library(mice)
md.pattern(snake_suppl)

library(VIM)
snake_suppl_aggr = aggr(snake_suppl, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(snake_suppl), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#removing duplicated lines in the dataset
snake_suppl_simp <- snake_suppl[!duplicated(snake_suppl$ID),]

#joining the two datasets
snake_tot <- left_join(snake_ds, snake_suppl_simp, by=c("ID"))
summary(snake_tot)

snake_tot$ID <- as.factor(snake_tot$ID)

save(snake_tot,file="snake_tot.Rda")

######### preparing basic dataset
load('snake_tot.Rda')

snake_basic <- snake_tot[!duplicated(snake_tot$ID),]
snake_basic <- snake_basic[-c(20:31, 55:97,99,103:108)]

save(snake_basic, file = "snake_basic.Rda")


######### datasets without people with uniform responses

straight_lines <- read.csv("images/straight_lines_overall.txt", header = FALSE)
straight_lines <- straight_lines$V1

straight_lines <- as.factor(straight_lines)

snake_tot_cleaned <- snake_tot[!is.element(snake_tot$ID, straight_lines),]
snake_basic_cleaned <- snake_basic[!is.element(snake_basic$ID, straight_lines),]

table(snake_basic_cleaned$group1_5n)

save(snake_basic_cleaned, file = "snake_basic_cleaned.Rda")
save(snake_tot_cleaned, file = "snake_tot_cleaned.Rda")
