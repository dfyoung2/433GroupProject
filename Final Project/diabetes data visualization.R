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
  select(AGE_GROUP, BMXBMI, WH_RATIO, BPXSY1, DR1TFIBE, LBXGLU, DIABETES_OUTCOME) %>%
  drop_na()

BMI_plot <- ggplot(diabetes_data) +
  geom_point(aes(x = BMXBMI, y = LBXGLU, color = AGE_GROUP)) +
  ylim(50, 150) +
  xlab("BMI") +
  ylab("Fasting Plasma Glucose (mg/dL)") +
  ggtitle(label = "BMI vs. Fasting Plasma Glucose")+
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red") +
  scale_color_continuous(name = "Age Group", labels = c("0", "20", "40", "60+"))
  

WHR_plot <- ggplot(diabetes_data) +
  geom_point(aes(x = WH_RATIO, y = LBXGLU, color = AGE_GROUP)) +
  ylim(50, 150) +
  xlab("Waist-Hip Ratio") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red") +
  scale_color_continuous(name = "Age Group", labels = c("0", "20", "40", "60+"))+
  ggtitle(label = "Waist-Hip Ratio vs. Fasting Plasma Glucose")

BPSY_plot <- ggplot(diabetes_data) +
  geom_point(aes(x = BPXSY1, y = LBXGLU, color = AGE_GROUP)) +
  ylim(50, 150) +
  xlab("Systolic Blood Pressure (mmHg)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red") +
  scale_color_continuous(name = "Age Group", labels = c("0", "20", "40", "60+"))+
  ggtitle(label = "Systolic Blood Pressure vs. Fasting Plasma Glucose")

fiber_plot <- ggplot(diabetes_data) +
  geom_point(aes(x = DR1TFIBE, y = LBXGLU, color = AGE_GROUP)) +
  ylim(50, 150) +
  xlab("Daily Fiber Intake (gm)") + 
  ylab("Fasting Plasma Glucose (mg/dL)") +
  geom_hline(yintercept = 100, linetype="dashed", color = "red") +
  geom_hline(yintercept = 126, linetype="dashed", color = "red") +
  scale_color_continuous(name = "Age Group", labels = c("0", "20", "40", "60+"))+
  ggtitle(label = "Daily Fiber vs. Fasting Plasma Glucose")

# plot all groups
grid.arrange(BMI_plot, WHR_plot, BPSY_plot, fiber_plot, ncol = 2)

