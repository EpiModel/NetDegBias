############# Preliminary setup ############# 

setwd("C:/Users/Stephen/Documents/Laptop Files/.Thesis Analysis")

library("readr")
library("tidyverse")
library("gee")
library("ggplot2")
library("gridExtra")
library("sandwich")
library("ggpubr")
library("broom")

ind_orig <- read_csv("individuals.csv")
part <- read_csv("partners.csv")

# PARTNERS INCLUDE ONLY THOSE THAT MEET INCLUSION CRITERIA AND HAVE MALE PARTNERS
p <- filter(part, `_INCLUDE` == 1 & GENDER == 1 & baseline_hiv == "Negative")

# INDIVIDUALS INCLUDE THOSE THAT MEET INCLUSION CRITERIA, ARE HIV-NEGATIVE, HAVE MALE PARTNERS
    # Extract from the partner dataset, takes the first observation it runs into, and then delete partner-level variables
ind <- p[!duplicated(p[,"STUDY_ID"]),]
ind <- rename(ind, AGECAT = `_AGECAT`)
ind <- select(ind, names(ind)[names(ind) %in% names(ind_orig)])

# Create factor variable for age categories
ind$AGECATF = factor(ind$AGECAT, labels = c("18-19", "20-24", "25-29", "30-39", "40 or greater"))
levels(ind$AGECATF) <- c("18-29", "18-29", "18-29", "30-39", "40 or greater")
  
# CODE ONGOING UNKNOWN: NO/MISSING
p$ONGOING.DKN <- ifelse(p$ONGOING == 2, 0, p$ONGOING)
p$AGREEMENT.DKN <- ifelse(p$ONGOING.DKN == p$prev_MP_sex, 1, 0)

p$ONGOING.DKM <- ifelse(p$ONGOING == 2, NA, p$ONGOING)
p$AGREEMENT.DKM <- ifelse(p$ONGOING.DKM == p$prev_MP_sex, 1, 0)

##################### FREQUENCY TABLE #####################
(deg.main <- table(p$ONGOING[p$main_rev == "More than once, Main"], 
                   p$prev_MP_sex[p$main_rev == "More than once, Main"]))
prop.table(deg.main, 1)*100

(deg.casual <- table(p$ONGOING[p$main_rev == "More than once, Casu"], 
                 p$prev_MP_sex[p$main_rev == "More than once, Casu"]))
prop.table(deg.casual, 1)*100

(deg.once <- table(p$ONGOING[p$main_rev == "Once"], 
                     p$prev_MP_sex[p$main_rev == "Once"]))
prop.table(deg.once, 1)*100

(deg.all <- table(p$ONGOING, p$prev_MP_sex))
prop.table(deg.all, 1)*100

##################### INDIVIDUAL MODELS - MEAN DEGREE ANALYSIS #####################

### DATA PREP ###
p.once <- p[p$main_rev == "Once",]
p.casual <- p[p$main_rev == "More than once, Casu",]
p.main <- p[p$main_rev == "More than once, Main",]

p.gb <- group_by(p, STUDY_ID)
p.gb.casual.main <- p.gb[p.gb$main_rev != "Once",]
p.gb.casual <- p.gb[p.gb$main_rev == "More than once, Casu",]
p.gb.main <- p.gb[p.gb$main_rev == "More than once, Main",]

means_casual_main <- summarise(p.gb.casual.main,
                       deg.dkn = sum(ONGOING.DKN),
                       deg.dkm = sum(ONGOING.DKM),
                       deg.adj = sum(prev_MP_sex))
means_casual <- summarise(p.gb.casual, 
                          deg.dkn = sum(ONGOING.DKN),
                          deg.dkm = sum(ONGOING.DKM),
                          deg.adj = sum(prev_MP_sex))
means_main <- summarise(p.gb.main, 
                        deg.dkn = sum(ONGOING.DKN),
                        deg.dkm = sum(ONGOING.DKM),
                        deg.adj = sum(prev_MP_sex))

  # NOTE: These overlap!! These individual (all, casual, main) sample sizes do not add up to ind
    # This is because each individual could have had multiple types of partnerships (e.g. main and a casual partnership)

means_casual_main$STUDY_ID <- as.character(means_casual_main$STUDY_ID)
ind_all <- right_join(ind, means_casual_main, by = "STUDY_ID")

means_casual$STUDY_ID <- as.character(means_casual$STUDY_ID)
ind_casual <- right_join(ind, means_casual, by = "STUDY_ID")

means_main$STUDY_ID <- as.character(means_main$STUDY_ID)
ind_main <- right_join(ind, means_main, by = "STUDY_ID")

###### UNSTRATIFIED MODELS #####

    # MAIN OR CASUAL #
unadj_allN <- glm(deg.adj ~ deg.dkn, data = ind_all, family = poisson (link = log))
unadj_allM <- glm(deg.adj ~ deg.dkm, data = ind_all, family = poisson (link = log))
coef(unadj_allN)
coef(unadj_allM)
confint(unadj_allN)
confint(unadj_allM)

    # MAIN #
unadj_mainN <- glm(deg.adj ~ deg.dkn, data = ind_main, family = poisson (link = log))
unadj_mainM <- glm(deg.adj ~ deg.dkm, data = ind_main, family = poisson (link = log))
coef(unadj_mainN)
coef(unadj_mainM)
confint(unadj_mainN)
confint(unadj_mainM)

    # CASUAL #
unadj_casualN <- glm(deg.adj ~ deg.dkn, data = ind_casual, family = poisson (link = log))
unadj_casualM <- glm(deg.adj ~ deg.dkm, data = ind_casual, family = poisson (link = log))
coef(unadj_casualN)
coef(unadj_casualM)
confint(unadj_casualN)
confint(unadj_casualM)
table(ind_casual$deg.dkm)
  #### FIGURE 1. Unstratified Mean Degree Comparison #####

# Prepare Data Frame for Prediction Estimates
  # For casual and casual/main
gridn <- expand.grid(deg.dkn = 0:5)
  # For main
gridn.m <- expand.grid(deg.dkn = 0:2)
# Predictions
  # Casual/Main
pred.uaN <- predict(unadj_allN, newdata = gridn, se.fit = TRUE)
pred.uaNc <- cbind(gridn, est = pred.uaN$fit, se = pred.uaN$se.fit, type = "Main or Casual")
  # Main
pred.umN <- predict(unadj_mainN, newdata = gridn.m, se.fit = TRUE)
pred.umNc <- cbind(gridn.m, est = pred.umN$fit, se = pred.umN$se.fit, type = "Main")
  # casual
pred.ucN <- predict(unadj_casualN, newdata = gridn, se.fit = TRUE)
pred.ucNc <- cbind(gridn, est = pred.ucN$fit, se = pred.ucN$se.fit, type = "Casual")
  # Combine all
preduN <- rbind(pred.umNc, pred.ucNc)

  # Produce exponentiated estimates and 95% confidence intervals
preduN <- mutate(preduN, exp.est = exp(est),
                         lowCI = exp(est - 1.96*se),
                         highCI = exp(est + 1.96*se))
  # Plot (900 x 550, w x h)
ggplot(preduN, aes(x = deg.dkn, y = (exp.est - deg.dkn))) +
  geom_point() + geom_line() +
  geom_ribbon(data = preduN, aes(ymin = lowCI - deg.dkn, ymax = highCI - deg.dkn), alpha = 0.15) +
  geom_hline(yintercept = 0) + labs(x = "Degree at Baseline", y = "Difference Between Predicted and Baseline Degrees") +
  theme_bw() + facet_grid( ~ type, scales = "free_x", space = "free_x") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif"))

###### STRATIFIED MODELS #####

  #### Continuous Age ####
    # MAIN OR CASUAL #
adj_allN <- glm(deg.adj ~ deg.dkn + age_baseline + RACE_INC, data = ind_all, family = poisson (link = log))
adj_allM <- glm(deg.adj ~ deg.dkm + age_baseline + RACE_INC, data = ind_all, family = poisson (link = log))
coef(adj_allN)
coef(adj_allM)
confint(adj_allN)
confint(adj_allM)
    # MAIN #
adj_mainN <- glm(deg.adj ~ deg.dkn + age_baseline + RACE_INC, data = ind_main, family = poisson (link = log))
adj_mainM <- glm(deg.adj ~ deg.dkm + age_baseline + RACE_INC, data = ind_main, family = poisson (link = log))
coef(adj_mainN)
coef(adj_mainM)
confint(adj_mainN)
confint(adj_mainM)
    # CASUAL #
adj_casualN <- glm(deg.adj ~ deg.dkn + age_baseline + RACE_INC, data = ind_casual, family = poisson (link = log))
adj_casualM <- glm(deg.adj ~ deg.dkm + age_baseline + RACE_INC, data = ind_casual, family = poisson (link = log))
coef(adj_casualN)
coef(adj_casualM)
confint(adj_casualN)
confint(adj_casualM)

  #### Categorical Age ####
    # MAIN OR CASUAL #
adj_allN <- glm(deg.adj ~ deg.dkn + AGECATF + RACE_INC, data = ind_all, family = poisson (link = log))
    # MAIN #
adj_mainN <- glm(deg.adj ~ deg.dkn + AGECATF + RACE_INC, data = ind_main, family = poisson (link = log))
    # CASUAL #
adj_casualN <- glm(deg.adj ~ deg.dkn + AGECATF + RACE_INC, data = ind_casual, family = poisson (link = log))

#### FIGURE 2. Stratified Mean Degree Comparison - Using Categorical Age #####

  # Prepare Data Frame for Prediction Estimates
gridn <- expand.grid(deg.dkn = 0:5, RACE_INC = unique(ind_all$RACE_INC), AGECATF = unique(ind_all$AGECATF))
  # Predictions
pred.aaN <- predict(adj_allN, newdata = gridn, se.fit = TRUE)
pred.aaNc <- cbind(gridn, est = pred.aaN$fit, se = pred.aaN$se.fit, type = "Main or Casual")
  # Produce exponentiated estimates and 95% confidence intervals
predN <- pred.aaNc %>% 
  mutate(exp.est = exp(est), lowCI = exp(est - 1.96*se), highCI = exp(est + 1.96*se)) %>% 
  filter(AGECATF != "40 or greater")
  # Plot (900 x 550, w x h)
ggplot(predN, aes(x = deg.dkn, y = exp.est, color = AGECATF)) +
  geom_point() + geom_line() + geom_abline(slope = 1) +
  geom_ribbon(data = predN, aes(ymin = lowCI, ymax = highCI, color = AGECATF), alpha = 0.05) +
  labs(color = "Age Group", x = "Degree at Baseline", y = "Difference Between Predicted and Baseline Degrees") +
  theme_bw() + facet_grid(. ~ RACE_INC) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif"))

##################### PARTNER MODELS - AGREEMENT ANALYSIS #####################

# Set reference levels
p$AGREEMENT <- factor(p$AGREEMENT)
p <- within(p, AGREEMENT <- relevel(AGREEMENT, ref = 4))
p.main$AGREEMENT <- factor(p.main$AGREEMENT)
p.main <- within(p.main, AGREEMENT <- relevel(AGREEMENT, ref = 4))
p.casual$AGREEMENT <- factor(p.casual$AGREEMENT)
p.casual <- within(p.casual, AGREEMENT <- relevel(AGREEMENT, ref = 4))

## OVERALL ##
fitag_allN <- glm(AGREEMENT.DKN ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
               data = p, family = binomial(link = "logit"))
exp(coef(fitag_allN))
sandwich_se <- diag(vcovHC(fitag_allN, type = "HC"))^0.5
exp(coef(fitag_allN)-1.96*sandwich_se)
exp(coef(fitag_allN)+1.96*sandwich_se)

fitag_allM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p, family = binomial(link = "logit"))
exp(coef(fitag_allM))
sandwich_se <- diag(vcovHC(fitag_allM, type = "HC"))^0.5
exp(coef(fitag_allM)-1.96*sandwich_se)
exp(coef(fitag_allM)+1.96*sandwich_se)

## BY PARTNERSHIP TYPE ##
  # MAIN #
fitag_mainN <- glm(AGREEMENT.DKN ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.main, family = binomial(link = "logit"))
exp(coef(fitag_mainN))
sandwich_se <- diag(vcovHC(fitag_mainN, type = "HC"))^0.5
exp(coef(fitag_mainN)-1.96*sandwich_se)
exp(coef(fitag_mainN)+1.96*sandwich_se)

fitag_mainM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT, 
                  data = p.main, family = binomial(link = "logit"))
exp(coef(fitag_mainM))
sandwich_se <- diag(vcovHC(fitag_mainM, type = "HC"))^0.5
exp(coef(fitag_mainM)-1.96*sandwich_se)
exp(coef(fitag_mainM)+1.96*sandwich_se)

  # CASUAL #
fitag_casualN <- glm(AGREEMENT.DKN ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.casual, family = binomial(link = "logit"))
exp(coef(fitag_casualN))
sandwich_se <- diag(vcovHC(fitag_casualN, type = "HC"))^0.5
exp(coef(fitag_casualN)-1.96*sandwich_se)
exp(coef(fitag_casualN)+1.96*sandwich_se)

fitag_casualM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.casual, family = binomial(link = "logit"))
exp(coef(fitag_casualM))
sandwich_se <- diag(vcovHC(fitag_casualM, type = "HC"))^0.5
exp(coef(fitag_casualM)-1.96*sandwich_se)
exp(coef(fitag_casualM)+1.96*sandwich_se)
