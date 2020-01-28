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
ind$AGECATF_5 <- ind$AGECATF
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
  theme_bw() + facet_grid( ~ type, scales = "free_x") + 
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
  # For 5 year estimates and confidence intervals
coef(adj_allN)*5
coef(adj_allM)*5
confint(adj_allN)*5
confint(adj_allM)*5
    # MAIN #
adj_mainN <- glm(deg.adj ~ deg.dkn + age_baseline + RACE_INC, data = ind_main, family = poisson (link = log))
adj_mainM <- glm(deg.adj ~ deg.dkm + age_baseline + RACE_INC, data = ind_main, family = poisson (link = log))
coef(adj_mainN)
coef(adj_mainM)
confint(adj_mainN)
confint(adj_mainM)
coef(adj_mainN)*5
coef(adj_mainM)*5
confint(adj_mainN)*5
confint(adj_mainM)*5
    # CASUAL #
adj_casualN <- glm(deg.adj ~ deg.dkn + age_baseline + RACE_INC, data = ind_casual, family = poisson (link = log))
adj_casualM <- glm(deg.adj ~ deg.dkm + age_baseline + RACE_INC, data = ind_casual, family = poisson (link = log))
coef(adj_casualN)
coef(adj_casualM)
confint(adj_casualN)
confint(adj_casualM)
coef(adj_casualN)*5
coef(adj_casualM)*5
confint(adj_casualN)*5
confint(adj_casualM)*5

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
  labs(color = "Age Group", x = "Degree at Baseline", y = "Predicted Degree") +
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
exp(5*coef(fitag_allN))
exp(5*(coef(fitag_allN)-1.96*sandwich_se))
exp(5*(coef(fitag_allN)+1.96*sandwich_se))

fitag_allM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p, family = binomial(link = "logit"))
exp(coef(fitag_allM))
sandwich_se <- diag(vcovHC(fitag_allM, type = "HC"))^0.5
exp(coef(fitag_allM)-1.96*sandwich_se)
exp(coef(fitag_allM)+1.96*sandwich_se)
exp(5*coef(fitag_allM))
exp(5*(coef(fitag_allM)-1.96*sandwich_se))
exp(5*(coef(fitag_allM)+1.96*sandwich_se))

## BY PARTNERSHIP TYPE ##
  # MAIN #
fitag_mainN <- glm(AGREEMENT.DKN ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.main, family = binomial(link = "logit"))
exp(coef(fitag_mainN))
sandwich_se <- diag(vcovHC(fitag_mainN, type = "HC"))^0.5
exp(coef(fitag_mainN)-1.96*sandwich_se)
exp(coef(fitag_mainN)+1.96*sandwich_se)
exp(5*coef(fitag_mainN))
exp(5*(coef(fitag_mainN)-1.96*sandwich_se))
exp(5*(coef(fitag_mainN)+1.96*sandwich_se))

fitag_mainM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT, 
                  data = p.main, family = binomial(link = "logit"))
exp(coef(fitag_mainM))
sandwich_se <- diag(vcovHC(fitag_mainM, type = "HC"))^0.5
exp(coef(fitag_mainM)-1.96*sandwich_se)
exp(coef(fitag_mainM)+1.96*sandwich_se)
exp(5*coef(fitag_mainM))
exp(5*(coef(fitag_mainM)-1.96*sandwich_se))
exp(5*(coef(fitag_mainM)+1.96*sandwich_se))


  # CASUAL #
fitag_casualN <- glm(AGREEMENT.DKN ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.casual, family = binomial(link = "logit"))
exp(coef(fitag_casualN))
sandwich_se <- diag(vcovHC(fitag_casualN, type = "HC"))^0.5
exp(coef(fitag_casualN)-1.96*sandwich_se)
exp(coef(fitag_casualN)+1.96*sandwich_se)
exp(5*coef(fitag_casualN))
exp(5*(coef(fitag_casualN)-1.96*sandwich_se))
exp(5*(coef(fitag_casualN)+1.96*sandwich_se))

fitag_casualM <- glm(AGREEMENT.DKM ~ `_AGEDIFF` + factor(`_RACEMIX`) + SEXFREQMP + perc_conc + AGREEMENT,
                  data = p.casual, family = binomial(link = "logit"))
exp(coef(fitag_casualM))
sandwich_se <- diag(vcovHC(fitag_casualM, type = "HC"))^0.5
exp(coef(fitag_casualM)-1.96*sandwich_se)
exp(coef(fitag_casualM)+1.96*sandwich_se)
exp(5*coef(fitag_casualM))
exp(5*(coef(fitag_casualM)-1.96*sandwich_se))
exp(5*(coef(fitag_casualM)+1.96*sandwich_se))


##################### APPENDIX - OTHER ANALYSIS CODE ##################### 
 
#### Figure A1. Unstratified Mean Degree Comparison - By Partnership Type: Predicted Degree (Vs. Difference Between Degrees) #####
# Prepare Data Frame for Prediction Estimates
preduN_nodiff <- mutate(preduN, exp.est = exp(est))
# Plot Figure
ggplot(preduN_nodiff, aes(x = deg.dkn, y = est)) +
  geom_point() + geom_line() +
  geom_smooth(method="lm", formula= (y ~ exp(x)), se=FALSE, linetype = 1) + 
  labs(x = "Degree at Baseline", y = "Predicted Degrees") +
  theme_bw() + facet_grid( ~ type, scales = "free_x") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif"))

#### Figure A2. Unstratified Mean Degree Comparison - By Partnership Type: Baseline Degree Don't Know Coded as Missing (Vs. No) #####
# Prepare Data Frame for Prediction Estimates
  # For casual and casual/main
gridm <- expand.grid(deg.dkm = 0:5)
  # For main
gridm.m <- expand.grid(deg.dkm = 0:2)
# Predictions
  # Casual/Main
pred.uaM <- predict(unadj_allM, newdata = gridm, se.fit = TRUE)
pred.uaMc <- cbind(gridm, est = pred.uaM$fit, se = pred.uaM$se.fit, type = "Main or Casual")
  # Main
pred.umM <- predict(unadj_mainM, newdata = gridm.m, se.fit = TRUE)
pred.umMc <- cbind(gridm.m, est = pred.umM$fit, se = pred.umM$se.fit, type = "Main")
  # casual
pred.ucM <- predict(unadj_casualM, newdata = gridm, se.fit = TRUE)
pred.ucMc <- cbind(gridm, est = pred.ucM$fit, se = pred.ucM$se.fit, type = "Casual")
  # Combine all
preduM <- rbind(pred.umMc, pred.ucMc)
# Produce exponentiated estimates and 95% confidence intervals
preduM <- mutate(preduM, exp.est = exp(est),
                 lowCI = exp(est - 1.96*se),
                 highCI = exp(est + 1.96*se))
# Plot Figure
ggplot(preduM, aes(x = deg.dkm, y = (exp.est - deg.dkm))) +
  geom_point() + geom_line() +
  geom_ribbon(data = preduM, aes(ymin = lowCI - deg.dkm, ymax = highCI - deg.dkm), alpha = 0.15) +
  geom_hline(yintercept = 0) + labs(x = "Degree at Baseline", y = "Difference Between Predicted and Baseline Degrees") +
  theme_bw() + facet_grid( ~ type, scales = "free_x") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif"))

#### Figure A3. Stratified Mean Degree Comparison - Using Categorical Age: View By Partnership Type and Don't Know Coded as Missing #####
  # 4 Age categories Included (40+ group excluded in plot)
  # View Plot for Don't Know Coded as Missing
  # View by Partnership Type
#### Categorical Age ####
  # MAIN OR CASUAL #
adj_allN_agecat5 <- glm(deg.adj ~ deg.dkn + AGECATF_5 + RACE_INC, data = ind_all, family = poisson (link = log))
adj_allM_agecat5 <- glm(deg.adj ~ deg.dkm + AGECATF_5 + RACE_INC, data = ind_all, family = poisson (link = log))
  # MAIN #
adj_mainN_agecat5 <- glm(deg.adj ~ deg.dkn + AGECATF_5 + RACE_INC, data = ind_main, family = poisson (link = log))
adj_mainM_agecat5 <- glm(deg.adj ~ deg.dkm + AGECATF_5 + RACE_INC, data = ind_main, family = poisson (link = log))
  # CASUAL #
adj_casualN_agecat5 <- glm(deg.adj ~ deg.dkn + AGECATF_5 + RACE_INC, data = ind_casual, family = poisson (link = log))
adj_casualM_agecat5 <- glm(deg.adj ~ deg.dkm + AGECATF_5 + RACE_INC, data = ind_casual, family = poisson (link = log))
# Prepare Data Frame for Prediction Estimates
gridn <- expand.grid(deg.dkn = 0:5, RACE_INC = unique(ind_all$RACE_INC), AGECATF_5 = unique(ind_all$AGECATF_5))
gridm <- expand.grid(deg.dkm = 0:5, RACE_INC = unique(ind_all$RACE_INC), AGECATF_5 = unique(ind_all$AGECATF_5))
gridn.m <- expand.grid(deg.dkn = 0:2, RACE_INC = unique(ind_all$RACE_INC), AGECATF_5 = unique(ind_all$AGECATF_5))
gridm.m <- expand.grid(deg.dkm = 0:2, RACE_INC = unique(ind_all$RACE_INC), AGECATF_5 = unique(ind_all$AGECATF_5))
# Predictions
  # All Except One Time
pred.aaN <- predict(adj_allN_agecat5, newdata = gridn, se.fit = TRUE)
pred.aaNc <- cbind(gridn, est = pred.aaN$fit, se = pred.aaN$se.fit, type = "Main or Casual")
pred.aaM <- predict(adj_allM_agecat5, newdata = gridm, se.fit = TRUE)
pred.aaMc <- cbind(gridm, est = pred.aaM$fit, se = pred.aaM$se.fit, type = "Main or Casual")
  # Main
pred.amN <- predict(adj_mainN_agecat5, newdata = gridn.m, se.fit = TRUE)
pred.amNc <- cbind(gridn.m, est = pred.amN$fit, se = pred.amN$se.fit, type = "Main")
pred.amM <- predict(adj_mainM_agecat5, newdata = gridm.m, se.fit = TRUE)
pred.amMc <- cbind(gridm.m, est = pred.amM$fit, se = pred.amM$se.fit, type = "Main")
  # Casual
pred.acN <- predict(adj_casualN_agecat5, newdata = gridn, se.fit = TRUE)
pred.acNc <- cbind(gridn, est = pred.acN$fit, se = pred.acN$se.fit, type = "Casual")
pred.acM <- predict(adj_casualM_agecat5, newdata = gridm, se.fit = TRUE)
pred.acMc <- cbind(gridm, est = pred.acM$fit, se = pred.acM$se.fit, type = "Casual")
# Combine all
predN <- rbind(pred.aaNc, pred.amNc, pred.acNc)
predN <- predN %>% 
  mutate(exp.est = exp(est), lowCI = exp(est - 1.96*se), highCI = exp(est + 1.96*se)) %>% 
  filter(AGECATF_5 != "40 or greater")
predM <- rbind(pred.aaMc, pred.amMc, pred.acMc)
predM <- predM %>% 
  mutate(exp.est = exp(est), lowCI = exp(est - 1.96*se), highCI = exp(est + 1.96*se)) %>% 
  filter(AGECATF_5 != "40 or greater")
# Plot Figures (900 x 550, w x h)
  # Don't Know Coded No
ggplot(predN, aes(x = deg.dkn, y = exp.est, color = AGECATF_5)) +
  geom_point() + geom_line() + geom_abline(slope = 1) +
  geom_ribbon(data = predN, aes(ymin = lowCI, ymax = highCI, color = AGECATF_5), alpha = 0.05) +
  labs(color = "Age Group", x = "Degree at Baseline", y = "Predicted Degree") +
  theme_bw() + facet_grid(. ~ RACE_INC) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif")) +
  facet_grid(type ~ RACE_INC)
  # Don't Know Coded Missing
ggplot(predM, aes(x = deg.dkm, y = exp.est, color = AGECATF_5)) +
  geom_point() + geom_line() + geom_abline(slope = 1) +
  geom_ribbon(data = predM, aes(ymin = lowCI, ymax = highCI, color = AGECATF_5), alpha = 0.05) +
  labs(color = "Age Group", x = "Degree at Baseline", y = "Predicted Degree") +
  theme_bw() + facet_grid(. ~ RACE_INC) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 12, family="serif")) +
  facet_grid(type ~ RACE_INC)

### Figure A4. Same Plots as Original Analysis Plots but with Parameters Adjusted for Poster Presentation (900 x 550, w x h)
# Figure A4.1 - Unstratified Models
ggplot(preduN, aes(x = deg.dkn, y = (exp.est - deg.dkn))) +
  geom_point() + geom_line() +
  geom_ribbon(data = preduN, aes(ymin = lowCI - deg.dkn, ymax = highCI - deg.dkn), alpha = 0.15) +
  geom_hline(yintercept = 0) + labs(x = "Degree at Baseline", y = "Difference Between Predicted and Baseline Degrees") +
  theme_bw() + facet_grid( ~ type, scales = "free_x") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 16, family="serif"),
        strip.text.x = element_text(size = 16))
# Figure A4.2 - Stratified Models
ggplot(predN, aes(x = deg.dkn, y = exp.est, color = AGECATF)) +
  geom_point() + geom_line() + geom_abline(slope = 1) +
  geom_ribbon(data = predN, aes(ymin = lowCI, ymax = highCI, color = AGECATF), alpha = 0.05) +
  labs(color = "Age Group", x = "Degree at Baseline", y = "Predicted Degree") +
  theme_bw() + facet_grid(. ~ RACE_INC) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), strip.background = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"), text = element_text(size = 16, family="serif"),
        strip.text.x = element_text(size = 16))
