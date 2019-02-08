## loading libraries requried. 


library(dplyr)
library(ggplot2)
library(gridExtra)



## loading test and training data 

train_values <- read.csv("train_values.csv")

train_labels <- read.csv("train_labels.csv")

test_data <- read.csv("test_values.csv")


str(train_values)

str(train_labels)



all (train_labels$patient_id == train_values$patient_id)


## combining train_values and train_labels into a single data frame based on parent id





str(train_data)

train_labels <- select(train_labels, c(-1))


train_data <- cbind(train_values, train_labels)




## checking if any nas are present in the data set. 
any(is.na(train_data))

## checking for NAs in test data set

any(is.na(test_data))



## exploring individual features 

## via scatter and boxplots

boxplots  <-function(df,factors,coly)
  
{
  
  for (col in factors )
  {
    
    p <- ggplot(df) + 
      geom_boxplot(aes_string(coly, col), fill ="royalblue3")+
      labs(y = col, x=coly, title =paste(coly,"vs",col))+
      theme( panel.background = element_rect(fill =   "lavender"))
    
    print(p)
    
    
  }
}


cat_plots <- function(df, cat_cols, coly)
  
  {
  options(repr.plot.width=6, repr.plot.height=5) 

    
    temp0 <- df[df$heart_disease_present==0, ]
    temp1 <- df[df$heart_disease_present==1, ]
    for( col in cat_cols){
     
      p1<-ggplot(temp0, aes_string(col) )+
        geom_bar(fill ="royalblue3")+
  labs(y = coly, x=col, title =paste("For heart disease absent"))+
  theme(panel.background = element_rect(fill =   "lavender"))
      
      p2<-ggplot(temp1, aes_string(col) )+
        geom_bar(fill ="royalblue3")+
        labs(y = coly, x=col, title =paste("For heart disease present"))+
        theme(panel.background = element_rect(fill =   "lavender"))
      
      grid.arrange(p1,p2,nrow =1)
      
      
  
    } 
    
  
}

plot_violin = function(df, cols, col_x = 'heart_disease_present'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_violin( fill ="royalblue3")+
      labs(y = col, x=col_x, title =paste(col,"vs",col_x))+
      theme( panel.background = element_rect(fill =   "lavender"))
    print(p)
  }
}

##boxplots(train_data, names(train_data), "heart_disease_present")  





## creating vector for categorical features



cat_features <- c("slope_of_peak_exercise_st_segment", "thal","chest_pain_type",
                  "num_major_vessels", "fasting_blood_sugar_gt_120_mg_per_dl",
                  "resting_ekg_results","sex","exercise_induced_angina")


## checking for columns which need to be converted to factors based on plots 

##boxplots(train_data, names(train_data), "heart_disease_present")





## converting columns to categorical features in trrain data

train_data$slope_of_peak_exercise_st_segment <- as.factor(train_data$slope_of_peak_exercise_st_segment)


train_data$chest_pain_type <- as.factor(train_data$chest_pain_type)

train_data$num_major_vessels <- as.factor(train_data$num_major_vessels)

train_data$fasting_blood_sugar_gt_120_mg_per_dl <- as.factor(train_data$fasting_blood_sugar_gt_120_mg_per_dl)

train_data$resting_ekg_results <- as.factor(train_data$resting_ekg_results)



train_data$sex <- as.factor(train_data$sex)


train_data$exercise_induced_angina <- as.factor(train_data$exercise_induced_angina)




## converting columns to categorical features in test_data

test_data$slope_of_peak_exercise_st_segment <- as.factor(test_data$slope_of_peak_exercise_st_segment)


test_data$chest_pain_type <- as.factor(test_data$chest_pain_type)

test_data$num_major_vessels <- as.factor(test_data$num_major_vessels)

test_data$fasting_blood_sugar_gt_120_mg_per_dl <- as.factor(test_data$fasting_blood_sugar_gt_120_mg_per_dl)

test_data$resting_ekg_results <- as.factor(test_data$resting_ekg_results)



test_data$sex <- as.factor(test_data$sex)


test_data$exercise_induced_angina <- as.factor(test_data$exercise_induced_angina)


str(test_data)


## plotting categorical features

##cat_plots(train_data, cat_features, "heart_disease_present")



##cat_plots(train_data, names(train_data), "heart_disease_present")



##check max_heart, age, oldpeak_eq_st_depression,serum_cholesterol_mg_per_dl, resting



for(i in  1:length(train_data$resting_blood_pressure)){
  
 print(train_data$resting_blood_pressure[i])
 
  
  if((as.numeric(train_data$resting_blood_pressure[i]) > 110) & (as.numeric(train_data$resting_blood_pressure[i]) <= 130 )){
   print("b/w 110 and 130" )
    train_data$resting_blood_pressure[i] = "b/w 110 and 130"   
  }
  
  
  else if((as.numeric(train_data$resting_blood_pressure[i])) > 130 & ((as.numeric(train_data$resting_blood_pressure[i]) <= 150 ))){
    print("b/w 130 and 150")
    train_data$resting_blood_pressure[i] = "b/w 130 and 150"   
  }
  
  else if((as.numeric(train_data$resting_blood_pressure[i]) > 150) & (as.numeric(train_data$resting_blood_pressure[i]) <= 300 )) {
    print("greater than 150" )
    train_data$resting_blood_pressure[i] = "greater than 150"  
    
  }
  
  else if((as.numeric(train_data$resting_blood_pressure[i]) <= 110 )) {
    print("less than equal to 110"  )
    train_data$resting_blood_pressure[i] = "less than equal to 110"  
  
  }
}


train_data$resting_blood_pressure <- as.factor(train_data$resting_blood_pressure)




## converting serum_cholesterol_mg_per_dl to factor





for(i in  1:length(train_data$serum_cholesterol_mg_per_dl)){
  
   if((as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) <= 200 )) {
    
    train_data$serum_cholesterol_mg_per_dl[i] = "less than equal to 200"  
    
  }
  
  
 else if((as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) > 200) & (as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) <= 250 )){
   
    train_data$serum_cholesterol_mg_per_dl[i] = "b/w 200 and 250"   
  }
  
  
  else if((as.numeric(train_data$serum_cholesterol_mg_per_dl[i])) > 250 & ((as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) <= 300 ))){
    
    train_data$serum_cholesterol_mg_per_dl[i] = "b/w 250 and 300"   
  }
  
  else if((as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) > 300) & (as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) <= 350 )) {
   
    train_data$serum_cholesterol_mg_per_dl[i] = "b/w 300 and 350"  
    
  }
  
 else if((as.numeric(train_data$serum_cholesterol_mg_per_dl[i]) > 350 )) {
    
    train_data$serum_cholesterol_mg_per_dl[i] = "greater than 350"  
    
  }
  
 
}


train_data$serum_cholesterol_mg_per_dl <- as.factor(train_data$serum_cholesterol_mg_per_dl)




## converting oldpeak_eq_st_depression to factor





for(i in  1:length(train_data$oldpeak_eq_st_depression)){
  
  print(train_data$oldpeak_eq_st_depression[i])
  
  if((as.numeric(train_data$oldpeak_eq_st_depression[i]) == 0)) {
    
    train_data$oldpeak_eq_st_depression[i] = "0" 
    print("0")
    
  }
  
  
  else if((as.numeric(train_data$oldpeak_eq_st_depression[i])) > 0 & ((as.numeric(train_data$oldpeak_eq_st_depression[i]) <= 1.0))){
    
    train_data$oldpeak_eq_st_depression[i] = "b/w 0 and 1.0"  
    print("b/w 0 and 1.0")
  }
  
  
  else if((as.numeric(train_data$oldpeak_eq_st_depression[i])) > 1.0 & ((as.numeric(train_data$oldpeak_eq_st_depression[i]) <= 2.0))){
    
    train_data$oldpeak_eq_st_depression[i] = "b/w 1.0 and 2.0"  
    print("b/w 1.0 and 2.0" )
  }
  
  else if((as.numeric(train_data$oldpeak_eq_st_depression[i]) > 2.0) & (as.numeric(train_data$oldpeak_eq_st_depression[i]) <= 3.0 )) {
    
    train_data$oldpeak_eq_st_depression[i] = "b/w 2.0 and 3.0" 
    print("b/w 2.0 and 3.0" )
    
  }
  
  else if((as.numeric(train_data$oldpeak_eq_st_depression[i]) > 3.0 )) {
    
    train_data$oldpeak_eq_st_depression[i] = "> 3.0"  
    print("b/w 3.0 and more" )
    
  }
  
  
}


train_data$oldpeak_eq_st_depression <- as.factor(train_data$oldpeak_eq_st_depression)


# converting age to factor





for(i in  1:length(train_data$age)){
  

  
  if((as.numeric(train_data$age[i]) <= 45)) {
    
    train_data$age[i] = "<=45" 
   
  }
  
  
  else if((as.numeric(train_data$age[i])) > 45 & ((as.numeric(train_data$age[i]) <= 55))){
    
    train_data$age[i] = "b/w 45 and 55"  
   
  }
  
  
  else if((as.numeric(train_data$age[i])) > 55 & ((as.numeric(train_data$age[i]) <= 65))){
    
    train_data$age[i] = "b/w 55 and 65"  
  
  }
  

  
  else if((as.numeric(train_data$age[i]) > 65 )) {
    
    train_data$age[i] = "> 65"  
   
    
  }
  
  
}


train_data$age <- as.factor(train_data$age)




# converting max_heart_rate_achieved to factor





for(i in  1:length(train_data$max_heart_rate_achieved)){
  
  
  
  if((as.numeric(train_data$max_heart_rate_achieved[i]) <= 120)) {
    
    train_data$max_heart_rate_achieved[i] = "<=120" 
    
  }
  
  
  else if((as.numeric(train_data$max_heart_rate_achieved[i])) > 120 & ((as.numeric(train_data$max_heart_rate_achieved[i]) <= 140))){
    
    train_data$max_heart_rate_achieved[i] = "b/w 120 and 140"  
    
  }
  
  
  
  
  
  else if((as.numeric(train_data$max_heart_rate_achieved[i])) > 140 & ((as.numeric(train_data$max_heart_rate_achieved[i]) <= 160))){
    
    train_data$max_heart_rate_achieved[i] = "b/w 140 and 160"  
    
  }
  
  
  else if((as.numeric(train_data$max_heart_rate_achieved[i])) > 160 & ((as.numeric(train_data$max_heart_rate_achieved[i]) <= 180))){
    
    train_data$max_heart_rate_achieved[i] = "b/w 160 and 180"  
    
  }
  
  
  else if((as.numeric(train_data$max_heart_rate_achieved[i]) > 180 )) {
    
    train_data$max_heart_rate_achieved[i] = "> 180"  
    
    
  }
  
  
}


train_data$max_heart_rate_achieved <- as.factor(train_data$max_heart_rate_achieved)




## converting lable to factor


for(i in  1:length(train_data$heart_disease_present)){
  
  
  
  if((as.numeric(train_data$heart_disease_present[i]) == 0)) {
    
    train_data$heart_disease_present[i] = "No" 
    
  }
  
  
  else if((as.numeric(train_data$heart_disease_present[i])) ==1){
    
    train_data$heart_disease_present[i] = "Yes"  
    
  }
  
  
}


train_data$heart_disease_present <- as.factor(train_data$heart_disease_present)

  

## doing same conversions for test data set



##check max_heart, age, oldpeak_eq_st_depression,serum_cholesterol_mg_per_dl, resting



for(i in  1:length(test_data$resting_blood_pressure)){
  
  print(test_data$resting_blood_pressure[i])
  
  
  if((as.numeric(test_data$resting_blood_pressure[i]) > 110) & (as.numeric(test_data$resting_blood_pressure[i]) <= 130 )){
    print("b/w 110 and 130" )
    test_data$resting_blood_pressure[i] = "b/w 110 and 130"   
  }
  
  
  else if((as.numeric(test_data$resting_blood_pressure[i])) > 130 & ((as.numeric(test_data$resting_blood_pressure[i]) <= 150 ))){
    print("b/w 130 and 150")
    test_data$resting_blood_pressure[i] = "b/w 130 and 150"   
  }
  
  else if((as.numeric(test_data$resting_blood_pressure[i]) > 150) & (as.numeric(test_data$resting_blood_pressure[i]) <= 300 )) {
    print("greater than 150" )
    test_data$resting_blood_pressure[i] = "greater than 150"  
    
  }
  
  else if((as.numeric(test_data$resting_blood_pressure[i]) <= 110 )) {
    print("less than equal to 110"  )
    test_data$resting_blood_pressure[i] = "less than equal to 110"  
    
  }
}


test_data$resting_blood_pressure <- as.factor(test_data$resting_blood_pressure)




## converting serum_cholesterol_mg_per_dl to factor





for(i in  1:length(test_data$serum_cholesterol_mg_per_dl)){
  
  if((as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) <= 200 )) {
    
    test_data$serum_cholesterol_mg_per_dl[i] = "less than equal to 200"  
    
  }
  
  
  else if((as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) > 200) & (as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) <= 250 )){
    
    test_data$serum_cholesterol_mg_per_dl[i] = "b/w 200 and 250"   
  }
  
  
  else if((as.numeric(test_data$serum_cholesterol_mg_per_dl[i])) > 250 & ((as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) <= 300 ))){
    
    test_data$serum_cholesterol_mg_per_dl[i] = "b/w 250 and 300"   
  }
  
  else if((as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) > 300) & (as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) <= 350 )) {
    
    test_data$serum_cholesterol_mg_per_dl[i] = "b/w 300 and 350"  
    
  }
  
  else if((as.numeric(test_data$serum_cholesterol_mg_per_dl[i]) > 350 )) {
    
    test_data$serum_cholesterol_mg_per_dl[i] = "greater than 350"  
    
  }
  
  
}


test_data$serum_cholesterol_mg_per_dl <- as.factor(test_data$serum_cholesterol_mg_per_dl)




## converting oldpeak_eq_st_depression to factor





for(i in  1:length(test_data$oldpeak_eq_st_depression)){
  
  print(test_data$oldpeak_eq_st_depression[i])
  
  if((as.numeric(test_data$oldpeak_eq_st_depression[i]) == 0)) {
    
    test_data$oldpeak_eq_st_depression[i] = "0" 
    print("0")
    
  }
  
  
  else if((as.numeric(test_data$oldpeak_eq_st_depression[i])) > 0 & ((as.numeric(test_data$oldpeak_eq_st_depression[i]) <= 1.0))){
    
    test_data$oldpeak_eq_st_depression[i] = "b/w 0 and 1.0"  
    print("b/w 0 and 1.0")
  }
  
  
  else if((as.numeric(test_data$oldpeak_eq_st_depression[i])) > 1.0 & ((as.numeric(test_data$oldpeak_eq_st_depression[i]) <= 2.0))){
    
    test_data$oldpeak_eq_st_depression[i] = "b/w 1.0 and 2.0"  
    print("b/w 1.0 and 2.0" )
  }
  
  else if((as.numeric(test_data$oldpeak_eq_st_depression[i]) > 2.0) & (as.numeric(test_data$oldpeak_eq_st_depression[i]) <= 3.0 )) {
    
    test_data$oldpeak_eq_st_depression[i] = "b/w 2.0 and 3.0" 
    print("b/w 2.0 and 3.0" )
    
  }
  
  else if((as.numeric(test_data$oldpeak_eq_st_depression[i]) > 3.0 )) {
    
    test_data$oldpeak_eq_st_depression[i] = "> 3.0"  
    print("b/w 3.0 and more" )
    
  }
  
  
}


test_data$oldpeak_eq_st_depression <- as.factor(test_data$oldpeak_eq_st_depression)


# converting age to factor





for(i in  1:length(test_data$age)){
  
  
  
  if((as.numeric(test_data$age[i]) <= 45)) {
    
    test_data$age[i] = "<=45" 
    
  }
  
  
  else if((as.numeric(test_data$age[i])) > 45 & ((as.numeric(test_data$age[i]) <= 55))){
    
    test_data$age[i] = "b/w 45 and 55"  
    
  }
  
  
  else if((as.numeric(test_data$age[i])) > 55 & ((as.numeric(test_data$age[i]) <= 65))){
    
    test_data$age[i] = "b/w 55 and 65"  
    
  }
  
  
  
  else if((as.numeric(test_data$age[i]) > 65 )) {
    
    test_data$age[i] = "> 65"  
    
    
  }
  
  
}


test_data$age <- as.factor(test_data$age)




# converting max_heart_rate_achieved to factor





for(i in  1:length(test_data$max_heart_rate_achieved)){
  
  
  
  if((as.numeric(test_data$max_heart_rate_achieved[i]) <= 120)) {
    
    test_data$max_heart_rate_achieved[i] = "<=120" 
    
  }
  
  
  else if((as.numeric(test_data$max_heart_rate_achieved[i])) > 120 & ((as.numeric(test_data$max_heart_rate_achieved[i]) <= 140))){
    
    test_data$max_heart_rate_achieved[i] = "b/w 120 and 140"  
    
  }
  
  
  
  
  
  else if((as.numeric(test_data$max_heart_rate_achieved[i])) > 140 & ((as.numeric(test_data$max_heart_rate_achieved[i]) <= 160))){
    
    test_data$max_heart_rate_achieved[i] = "b/w 140 and 160"  
    
  }
  
  
  else if((as.numeric(test_data$max_heart_rate_achieved[i])) > 160 & ((as.numeric(test_data$max_heart_rate_achieved[i]) <= 180))){
    
    test_data$max_heart_rate_achieved[i] = "b/w 160 and 180"  
    
  }
  
  
  else if((as.numeric(test_data$max_heart_rate_achieved[i]) > 180 )) {
    
    test_data$max_heart_rate_achieved[i] = "> 180"  
    
    
  }
  
  
}


test_data$max_heart_rate_achieved <- as.factor(test_data$max_heart_rate_achieved)









