#############################################################
### CFA Demo - European Social Survey Wave 6 - Depression ###
#############################################################


#Packages Required
library(tidyverse) #datawrangling
library(readr) #reading in csv data
library(lavaan) #sem models 
library(semTools) #lots of useful additional sem related tools
library(semPlot) #visual plots for SEM

#Read in data - European Social Survey Wave 6
data<- read_csv("Data/ess6_data.csv")
View(data)

# Data Wrangling 
df <-data %>%
  mutate(across(c(D4R, D6R), ~5-.)) #recode D4R and D6R

########################################################
##### Confirmatory Factor Analysis for Depression ###### 
###### (items D1, D2, D3, D4R, D5, D6R, D7, D8) ########
########################################################

#One Factor Solution 

#Step 1: Specify the model 

model1 <- '
          depression =~ D1 + D2 + D3 + D4R + D5 + D6R + D7 + D8
          
'
# Step 2: Fit the model to your data, using FIML to deal with missing data
# ML is the default estimator when using continuous data, but we've been explicit
# here and specified using estimator = argument. 

m1 <- sem(model = model1, data = df, estimator = "ML", missing = "ML")

#Step 3: Output for your fitted model
#Requests fit statistics, standardized estimates, rsquare 
summary(m1, 
        fit.measures = TRUE, 
        standardized = TRUE,
        rsquare = TRUE)

#Extract specific outputs with more information
parameterEstimates(m1) #estimates including CI
standardizedSolution(m1) #standardized estimates for all parameters
fitMeasures(m1) #more fit statistics

####################
#### Activity 1 ####
####################

# Run a two-factor solution for the depression CFA with D4R and D6R loaded onto positive affect factor
# and the other items loaded onto a negative affect factor 

#Step 1: specify the model

#Step 2: fit the model to your data

#Step 3: request the summary output 


# Which model has the better fit?


###Visualising Models###
semPaths(m1, whatLabels = "std", intercepts = F)

#to see other visualisation options use ?semPaths


####################################################
#### Modification Indices and Co-varying Errors ####
####################################################


#We can request modification indices using the modindices = TRUE argument
summary(m1, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)

#Instead of a two-factor solution it may be more appropriate to covary the errors terms for
#D4R and D6R due to measurement error . 

model3<- '
          depression =~ D1 + D2 + D3 + D4R + D5 + D6R + D7 + D8
          
          #covary error terms
          D4R ~~ D6R
          
'

#Fit model to the data 
m3 <- sem(model = model3, data = df, estimator = "ML", missing = "ML")

#Output for your fitted model 
summary(m3, fit.measures = TRUE, standardized = TRUE)

###########################
#### Model Comparisons ####
###########################


#Compare m1 and m3 model using compareFit function in semTools package
output <- compareFit(m3, m1)
summary(output)


#Tip: Are my models nested? Check with net() argument
net(m1,m3)


################
#### Extras ####
################

#### Second Order CFA ####

model4 <- '
positive =~ D4R + D6R
negative =~ D1 + D2 + D3 + D5 + D7 + D8

#specify second order factor called depression
depression =~ positive + negative

'
#Fit model to the data 
m4 <- sem(model = model4, data = df, missing = "ML")

#Output for your fitted model 
summary(m4, fit.measures = TRUE, standardized = TRUE)

### Ordinal Data Variables ####
# We can tell lavaan if our variables are ordinal 
# using the ordered = TRUE argument.

model5 <- '
          depression =~ D1 + D2 + D3 + D4R + D5 + D6R + D7 + D8
          
'
#Fit the model to your data, ML for missing data not available for categorical items
#It will default to DWLS estimator

m5<- sem(model = model5, data = df,
         ordered = TRUE)

summary(m5, fit.measures = TRUE, rsquare = TRUE)

## CFA per group

# Let's run our depression CFA for GB and ES

#Create a new dataset containing just GB and ES
df_small <- filter(df, cntry == c("GB","ES")) 


#Run our model 5 using this new dataset (df_small)
#Add the group = argument, to tell lavaan which variable indicates group

m6<-sem(model = model5,
     data = df_small,
     group = "cntry")

#request output 
summary(m6, fit.measures = TRUE, rsquare = TRUE)