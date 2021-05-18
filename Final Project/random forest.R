library(SASxport)
library(tidyverse)
library(randomForest)

demo_info <- read.xport("data/DEMO_J.XPT")
body_mx <- read.xport("data/BMX_J.XPT")
blood_px <- read.xport("data/BPX_J.XPT")
nutrient_intake <- read.xport("data/DR1TOT_J.XPT")
glucose_data <- read.xport("data/GLU_J.XPT")

multi_inner <- Reduce(
  inner_join, 
  list(demo_info, body_mx, blood_px, nutrient_intake, glucose_data)
)

cleaned_data <- multi_inner %>%
  mutate(WH_RATIO = BMXWAIST / BMXHIP) %>%
  mutate(DIAGNOSIS = ifelse(LBXGLU >= 100, "Diabetes", "Healthy"), DIAGNOSIS = factor(DIAGNOSIS, levels = c("Diabetes", "Healthy"))) %>%
  select(RIDAGEYR, BMXBMI, WH_RATIO, BPXSY1, BPXDI1, DR1TCARB, DR1TSUGR, DR1TSODI, DR1TFIBE, DIAGNOSIS) %>%
  drop_na()

subset_list <- split(cleaned_data, cleaned_data$DIAGNOSIS)
diabetes_data <- subset_list[[1]]
healthy_data <- subset_list[[2]]

rf_data <- bind_rows(diabetes_data[1:900, ], healthy_data[1:900, ])


#split data set into training and testing sets
set.seed(3)
data_set_size <- floor(nrow(rf_data)/2)
index <- sample(1:nrow(rf_data), size = data_set_size)
rf_train <- rf_data[index, ]
rf_test <- rf_data[-index, ]

# train the model
rf_classifier = randomForest(DIAGNOSIS ~ BMXBMI + WH_RATIO + BPXSY1 + DR1TFIBE, data=rf_train, ntree=1000, mtry=2, importance=TRUE)
rf_classifier

