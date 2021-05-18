library(SASxport)
library(tidyverse)
library(ggplot2)
library(gridExtra)

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
diabetes_logit1 <- glm(DIABETES_OUTCOME ~ AGE_GROUP, data = diabetes_data, family = "binomial")
summary(diabetes_logit1)

diabetes_logit2 <- glm(DIABETES_OUTCOME ~ BMXBMI + WH_RATIO + BPXSY1 + BPXDI1 + AGE_GROUP, data = diabetes_data, family = "binomial")
summary(diabetes_logit2)

diabetes_logit3 <- glm(DIABETES_OUTCOME ~ BMXBMI + WH_RATIO + BPXSY1 + BPXDI1 + DR1TCARB + DR1TSUGR + DR1TSODI + DR1TFIBE + AGE_GROUP, data = diabetes_data, family = "binomial")
summary(diabetes_logit3)