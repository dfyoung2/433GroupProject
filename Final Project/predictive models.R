library(SASxport)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(aod)

demo_info <- read.xport("Final Project/data/DEMO_J.XPT")
body_mx <- read.xport("Final Project/data/BMX_J.XPT")
blood_px <- read.xport("Final Project/data/BPX_J.XPT")
nutrient_intake <- read.xport("Final Project/data/DR1TOT_J.XPT")
glucose_data <- read.xport("Final Project/data/GLU_J.XPT")

multi_inner <- Reduce(
  inner_join, 
  list(demo_info, body_mx, blood_px, nutrient_intake, glucose_data)
)

diabetes_data <- multi_inner %>%
  mutate(AGE_GROUP = ifelse((RIDAGEYR >= 0 & RIDAGEYR < 20), 1, 
                            ifelse((RIDAGEYR > 20 & RIDAGEYR < 40), 2, 
                                   ifelse((RIDAGEYR > 40 & RIDAGEYR < 60), 3, 4
                                   )))) %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIABETES_OUTCOME = ifelse(LBXGLU >= 100, 1, 0)) %>%
  select(SEQN, RIAGENDR, RIDAGEYR, AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, BPXDI1, DR1TCARB, DR1TSUGR, DR1TSODI, DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  drop_na()

# binomial logistic regression
diabetes_data$AGE_GROUP <- factor(diabetes_data$AGE_GROUP)
diabetes_logit <- glm(DIABETES_OUTCOME ~ BMXBMI + WH_RATIO + BPXSY1 + BPXDI1 + DR1TCARB + DR1TSUGR + DR1TSODI + DR1TFIBE + AGE_GROUP, data = diabetes_data, family = "binomial")
summary(diabetes_logit)

# predict using varying BMI
predict_BMI <- with(diabetes_data, data.frame(BMXBMI = rep(seq(from = 12.3, to = 86.2, length.out = 100), 4), 
                                              WH_RATIO = mean(WH_RATIO),
                                              BPXSY1 = mean(BPXSY1), 
                                              BPXDI1 = mean(BPXDI1),
                                              DR1TCARB = mean(DR1TCARB),
                                              DR1TSUGR = mean(DR1TSUGR),
                                              DR1TSODI = mean(DR1TSODI),
                                              DR1TFIBE = mean(DR1TFIBE),
                                              AGE_GROUP = factor(rep(1:4, each = 100))))

predict_BMI2 <- cbind(predict_BMI, predict(diabetes_logit, newdata = predict_BMI, type = "link", se = TRUE))
predict_BMI2 <- within(predict_BMI2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# predict using varying waist-hip ratio
predict_WHR <- with(diabetes_data, data.frame(BMXBMI = mean(BMXBMI), 
                                              WH_RATIO = rep(seq(from = 0.7, to = 1.2, length.out = 100), 4),
                                              BPXSY1 = mean(BPXSY1), 
                                              BPXDI1 = mean(BPXDI1),
                                              DR1TCARB = mean(DR1TCARB),
                                              DR1TSUGR = mean(DR1TSUGR),
                                              DR1TSODI = mean(DR1TSODI),
                                              DR1TFIBE = mean(DR1TFIBE),
                                              AGE_GROUP = factor(rep(1:4, each = 100))))

predict_WHR2 <- cbind(predict_WHR, predict(diabetes_logit, newdata = predict_WHR, type = "link", se = TRUE))
predict_WHR2 <- within(predict_WHR2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# predict using varying systolic blood pressure
predict_WHR <- with(diabetes_data, data.frame(BMXBMI = mean(BMXBMI), 
                                              WH_RATIO = mean(WH_RATIO),
                                              BPXSY1 = rep(seq(from = 72, to = 228, length.out = 100), 4), 
                                              BPXDI1 = mean(BPXDI1),
                                              DR1TCARB = mean(DR1TCARB),
                                              DR1TSUGR = mean(DR1TSUGR),
                                              DR1TSODI = mean(DR1TSODI),
                                              DR1TFIBE = mean(DR1TFIBE),
                                              AGE_GROUP = factor(rep(1:4, each = 100))))

predict_WHR2 <- cbind(predict_WHR, predict(diabetes_logit, newdata = predict_WHR, type = "link", se = TRUE))
predict_WHR2 <- within(predict_WHR2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# predict using varying daily fiber intake
predict_WHR <- with(diabetes_data, data.frame(BMXBMI = mean(BMXBMI), 
                                              WH_RATIO = mean(WH_RATIO),
                                              BPXSY1 = mean(BPXSY1), 
                                              BPXDI1 = mean(BPXDI1),
                                              DR1TCARB = mean(DR1TCARB),
                                              DR1TSUGR = mean(DR1TSUGR),
                                              DR1TSODI = mean(DR1TSODI),
                                              DR1TFIBE = rep(seq(from = 0, to = 107.8, length.out = 100), 4),
                                              AGE_GROUP = factor(rep(1:4, each = 100))))

predict_WHR2 <- cbind(predict_WHR, predict(diabetes_logit, newdata = predict_WHR, type = "link", se = TRUE))
predict_WHR2 <- within(predict_WHR2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

BMI_predict <- ggplot(predict_BMI2, aes(x = BMXBMI, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = AGE_GROUP), alpha = 0.2) + 
  geom_line(aes(colour = AGE_GROUP), size = 1)+
  ggtitle(label = "Using BMI as a predictor")

WHR_predict <- ggplot(predict_WHR2, aes(x = WH_RATIO, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = AGE_GROUP), alpha = 0.2) + 
  geom_line(aes(colour = AGE_GROUP), size = 1)+
  ggtitle(label = "Using Waist-Hip Ratio as a predictor")

grid.arrange(BMI_predict, WHR_predict, ncol = 2)