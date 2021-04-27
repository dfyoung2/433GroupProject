
#title: "Nutrition"
#author: "Jonah Maroszek"
#date: "4/13/2021"
#output: html_document



knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(gridExtra)
library(knitr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(car)
library(ggfortify)


## Background Information and Motivation
#We were presented with the data set "BodyFat.csv", which describes different measurements from 252 men. The measurements (predictors) include age; weight (lbs); height (in); adiposity (BMI); and circumference (cm) of neck, chest, abdomen, hip, thigh, knee, ankle, biceps, forearm, and wrist. Each participant received a unique ID number, which is not a predictor variable. We also do not consider density to be a predictor since it is nearly an exact replicate of the outcome variable, body fat. 

#The subjects had their body fat percentage accurately measured by subjection to underwater weighing, which uses water displacement to determine near true values of body fat percentage; thus, we can use these accurate outcome variable data in order to construct an optimal prediction model using only the most significant predictor variables.

# linear model from body fat data set, echo=FALSE}
bodyfat <- read_csv("BodyFat.csv", col_types = cols())


#plots of body fat data, echo=FALSE}
p1 = ggplot(bodyfat) +
  geom_density(aes(x = BODYFAT)) +
  geom_vline(xintercept = mean(bodyfat$BODYFAT), color = "red", linetype = "dashed") +
  xlab("Body Fat Percentage") +
  ylab("Density") 
  
p2 = ggplot(bodyfat) +
  geom_density(aes(x = WEIGHT)) +
  geom_vline(xintercept = mean(bodyfat$WEIGHT), color = "red", linetype = "dashed") +
  xlab("Weight (lbs)") +
  ylab("Density") 

p3 = ggplot(bodyfat) +
  geom_density(aes(x = AGE)) +
  geom_vline(xintercept = mean(bodyfat$AGE), color = "red", linetype = "dashed") +
  xlab("Age (years)") +
  ylab("Density") 

p4 = ggplot(bodyfat, aes(x = ABDOMEN)) +
  geom_density() +
  geom_vline(xintercept = mean(bodyfat$ABDOMEN), color = "red", linetype = "dashed") +
  xlab("Abdomen Circumference (cm)") +
  ylab("Density") 

grid.arrange(p1,p2,p3, p4, nrow = 2, top = "Density Plots of Key Variables")


#data cleaning body fat data

cleaned.body.fat = bodyfat %>% 
  filter(BODYFAT > 2) %>% #Remove people with improbably low body fat.
  filter(HEIGHT > 48) %>% #Remove people with height less than 4 ft. data entry error
  filter(WEIGHT < 300) %>%
  filter(WEIGHT != 148.50) %>%
  filter(WEIGHT != 148.25) %>%
  filter(WEIGHT !=  224.50) %>%
  select(-IDNO) #Dont need ID column

final.lm = lm(BODYFAT~ WEIGHT + ABDOMEN, data = cleaned.body.fat)
tab_model(final.lm)



### Strengths and Weaknesses of the Model
#The linear model made with all predicted variables included (except participant ID and density) perfroms very strongly. The adjusted $R^2$ value is 0.725. However, it seems that we are able to retain the predictive power of the model by using only two predictor variables: weight and abdomen. This model gives essentially the same $R^2$ value (0.708) while being much simpler to use in practice. An $R^2$ value of 0.708 means that 70.8% of the variation in bodyfat percentage can be explained by variation in weight and abdomen circumference. The strengths of our model include ease of use and strong predictive power despite its simplicity. A weakness of the model is that it can only be applied to males because the data set only contains males in it.

#define nutrition functions, echo = FALSE}
#model to predict body fat percentage
#ab is abdomen circumference in cm 

bf = function(weight, ab){
  return((-42.0533 -0.1241*weight + 0.9 * ab) / 100) #divide by 100 to get in decimal form
}

#calculate lean body mass from body fat percentage and weight
lbm = function(weight, bf){
  lbm.percent = 1 - bf
  return(weight * lbm.percent)
}

#Basal Metabolic Rate - Katch Mcardle Formula
#Calories burned daily not including activity level
bmr = function(lbm){
  return(370 + (9.82 * lbm))
}


# Sedentary (little to no exercise + work a desk job) = 1.2
# Lightly Active (light exercise 1-3 days / week) = 1.375
# Moderately Active (moderate exercise 3-5 days / week) = 1.55
# Very Active (heavy exercise 6-7 days / week) = 1.725
# Extremely Active (very heavy exercise, hard labor job, training 2x / day) = 1.9

#total daily energy expenditure - calories burned daily w/ activity
#DEFINE ACTIVITY BASED ON ABOVE TABLE
tdee = function(bmr, activty){
  return(bmr * activity)
}

#goal = 1 if desire fat loss
#goal = 2 if desire is weight maintenance
#goal = 3 if desire is muscle gain
#adjusts calorie needs based on goal. 20% deficit for fat loss. 20% surplus for muscle gain
cal.needs = function(tdee, goal){
  if(goal == 1){
    return(tdee * .8) 
  }else if(goal == 2){
    return(tdee)
  }else{
    return(tdee * 1.2)
  }
}

#find how many grams of each macro to eat 
#can input desired ratios as vector in the order of protein, fat, carbs. Default option provided if no specification
macros = function(cal.needs, ratio = c(0.2,0.25,0.55)){
  
  #calculate calories needed from each macro
  calories.from.protein = cal.needs * ratio[1]
  calories.from.fat = cal.needs * ratio[2]
  calories.from.carbs = cal.needs * ratio[3]
  
  #calculate grams needed from each macro
  protein.g = calories.from.protein / 4
  carb.g = calories.from.carbs / 4
  fat.g = calories.from.fat / 9
 
  return(c(protein.g, carb.g, fat.g))
}

nutrition = function(weight, ab, activity, goal){
  bf = bf(weight, ab)
  lbm = lbm(weight, bf)
  bmr = bmr(lbm)
  tdee = bmr * activity
  cal.needs = cal.needs(tdee, goal)
  macros = macros(cal.needs)

  return(c("BF%" = round(bf * 100, 2),
    "LBM" = round(lbm,2),
    "BMR" = round(bmr,2),
    "TDEE" = round(tdee,2),
    "Calories" = round(cal.needs,0),
    "Protein" = round(macros[1], 0),
    "Carbs" = round(macros[2], 0),
    "Fat" = round(macros[3],0)))
}


#examples for data visualization assignment
person.a = nutrition(209,114,1.2,1)

person.b = nutrition(150, 81,1.55,3)
person.c = nutrition(120,97,1.9,2)
person.d = nutrition(184,104,1.375,1)





