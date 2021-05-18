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
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, DR1TFIBE, DIABETES_OUTCOME) %>%
  drop_na()

# binomial logistic regression
diabetes_data$AGE_GROUP <- factor(diabetes_data$AGE_GROUP)
diabetes_logit <- glm(DIABETES_OUTCOME ~ BMXBMI + WH_RATIO + BPXSY1 + DR1TFIBE + AGE_GROUP, data = diabetes_data, family = "binomial")
summary(diabetes_logit)

# group 1
g1_data <- multi_inner %>%
  mutate(AGE_GROUP = ifelse((RIDAGEYR >= 0 & RIDAGEYR < 20), 1, 
                            ifelse((RIDAGEYR > 20 & RIDAGEYR < 40), 2, 
                                   ifelse((RIDAGEYR > 40 & RIDAGEYR < 60), 3, 4
                                   )))) %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIABETES_OUTCOME = ifelse(LBXGLU >= 100, 1, 0)) %>%
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  filter(AGE_GROUP == 1) %>%
  drop_na()

g1_BMI_plot <- ggplot(g1_data) +
  geom_point(aes(x = BMXBMI, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("BMI") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g1_WHR_plot <- ggplot(g1_data) +
  geom_point(aes(x = WH_RATIO, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Waist-Hip Ratio") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g1_BPSY_plot <- ggplot(g1_data) +
  geom_point(aes(x = BPXSY1, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g1_fiber_plot <- ggplot(g1_data) +
  geom_point(aes(x = DR1TFIBE, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Daily Fiber Intake (gm)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

# group 2
g2_data <- multi_inner %>%
  mutate(AGE_GROUP = ifelse((RIDAGEYR >= 0 & RIDAGEYR < 20), 1, 
                            ifelse((RIDAGEYR > 20 & RIDAGEYR < 40), 2, 
                                   ifelse((RIDAGEYR > 40 & RIDAGEYR < 60), 3, 4
                                   )))) %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIABETES_OUTCOME = ifelse(LBXGLU >= 100, 1, 0)) %>%
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  filter(AGE_GROUP == 2) %>%
  drop_na()

g2_BMI_plot <- ggplot(g2_data) +
  geom_point(aes(x = BMXBMI, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("BMI") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g2_WHR_plot <- ggplot(g2_data) +
  geom_point(aes(x = WH_RATIO, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Waist-Hip Ratio") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g2_BPSY_plot <- ggplot(g2_data) +
  geom_point(aes(x = BPXSY1, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g2_fiber_plot <- ggplot(g2_data) +
  geom_point(aes(x = DR1TFIBE, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Daily Fiber Intake (gm)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

# group 3
g3_data <- multi_inner %>%
  mutate(AGE_GROUP = ifelse((RIDAGEYR >= 0 & RIDAGEYR < 20), 1, 
                            ifelse((RIDAGEYR > 20 & RIDAGEYR < 40), 2, 
                                   ifelse((RIDAGEYR > 40 & RIDAGEYR < 60), 3, 4
                                   )))) %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIABETES_OUTCOME = ifelse(LBXGLU >= 100, 1, 0)) %>%
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  filter(AGE_GROUP == 3) %>%
  drop_na()

g3_BMI_plot <- ggplot(g3_data) +
  geom_point(aes(x = BMXBMI, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("BMI") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g3_WHR_plot <- ggplot(g3_data) +
  geom_point(aes(x = WH_RATIO, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Waist-Hip Ratio") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g3_BPSY_plot <- ggplot(g3_data) +
  geom_point(aes(x = BPXSY1, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g3_fiber_plot <- ggplot(g3_data) +
  geom_point(aes(x = DR1TFIBE, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Daily Fiber Intake (gm)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

# group 4
g4_data <- multi_inner %>%
  mutate(AGE_GROUP = ifelse((RIDAGEYR >= 0 & RIDAGEYR < 20), 1, 
                            ifelse((RIDAGEYR > 20 & RIDAGEYR < 40), 2, 
                                   ifelse((RIDAGEYR > 40 & RIDAGEYR < 60), 3, 4
                                   )))) %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIABETES_OUTCOME = ifelse(LBXGLU >= 100, 1, 0)) %>%
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  filter(AGE_GROUP == 4) %>%
  drop_na()

g4_BMI_plot <- ggplot(g4_data) +
  geom_point(aes(x = BMXBMI, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("BMI") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g4_WHR_plot <- ggplot(g4_data) +
  geom_point(aes(x = WH_RATIO, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Waist-Hip Ratio") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g4_BPSY_plot <- ggplot(g4_data) +
  geom_point(aes(x = BPXSY1, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")

g4_fiber_plot <- ggplot(g4_data) +
  geom_point(aes(x = DR1TFIBE, y = LBXGLU)) +
  ylim(50, 150) +
  xlab("Daily Fiber Intake (gm)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red")


# plot all groups
grid.arrange(g1_BMI_plot, g1_WHR_plot, g1_BPSY_plot, g1_fiber_plot, 
             g2_BMI_plot, g2_WHR_plot, g2_BPSY_plot, g2_fiber_plot,
             g3_BMI_plot, g3_WHR_plot, g3_BPSY_plot, g3_fiber_plot,
             g4_BMI_plot, g4_WHR_plot, g4_BPSY_plot, g4_fiber_plot,
             ncol = 4)