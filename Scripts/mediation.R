######################################################
### Mediation Demo - European Social Survey Wave 6 ###
######################################################

#This demo follows on from the morning CFA demo 

#Packages Required
library(tidyverse)
library(readr)
library(lavaan)
library(semTools)
library(semPlot)

#Read in data - European Social Survey Wave 6
data<- read_csv("Data/ess6_data.csv")


# Data Wrangling 
df <- data %>%
  mutate(D4R = 5 - D4R, #recode D4R and D6R
         D6R = 5 - D6R) %>%
  rowwise() %>%
  mutate(deptotal = mean(c_across(D1:D8)), #create average scores for each scale - depression
         trustppl = mean(c_across(trustpeople1:trustpeople3)), #trust in people
         trustpol = mean(c_across((politics1:politics7)))) #trust in politics

#filter to GB only
df <- filter(df, cntry == "GB")

#########################################
#### Simple mediation - one mediator ####
#########################################

#set seed for bootstrapping 
set.seed(13)

# optimism -> trustppl -> depression

modelmediation <- '

#measurement model 
          depression =~ D1 + D2 + D3 + D4R + D5 + D6R + D7 + D8
          
          #covary error terms
          D4R ~~ D6R
          
#structural model
          
          depression ~ c*optimism + b*trustppl
          trustppl ~ a*optimism
          
#define new parameters
          ind := a*b
          total := c + ind
          


'

m1 <- sem(model = modelmediation, data = df, missing = "ML")
summary(m1, fit.measures = TRUE, standardized = TRUE)


#Requesting bootstrapped confidence intervals, using se = "bootstrapped" 
#may take some time due to sample size, standard is 1000, 
#change sample size using bootstrap = 

#Run your model
m1 <- sem(model = modelmediation, data = df, missing = "ML", se = "bootstrap", bootstrap = 1000)

#request output
summary(m1, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

#Bias Corrected and accelerated CIs
parameterEstimates(m1, boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE)

################################################
#### Activity 2 - two mediators in parallel ####
################################################

#Add in another mediator into the model - trustpol 
# optimism -> trustppl -> depression
# optimism -> trustpol -> depression 
#You will now have two indirect effects, one via trustppl and one via trustpol
#You may want to use the a1, a2, b1, b2 naming convention 



##################################
#### Serial Mediation Example ####
##################################

#optimism -> trustppl -> trustpol -> depression

modelserial <- '

#measurement model 
          depression =~ D1 + D2 + D3 + D4R + D5 + D6R + D7 + D8
          
          #covary error terms
          D4R ~~ D6R
          
#structural model
          
          depression ~ c*optimism + b1*trustppl + b2*trustpol
          trustppl ~ a1*optimism
          trustpol ~ a2*optimism + d1*trustppl 
          
          
          
#define new parameters
          ind1 := a1*b1
          ind2 := a2*b2
          ind3 := a1*d1*b2
          totind := ind1 + ind2 + ind3
          total := c + ind1 + ind2 + ind3
          


'
m3 <- sem(model = modelserial, data = df, missing = "ML", se = "bootstrap", bootstrap = 10)
summary(m1, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

#Bias Corrected and accelarated CIs
parameterEstimates(m1, boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE)


################
#### Extras ####
################

#Plot models
semPaths(m1, whatLabels = "std", intercepts = F)

#Alternative plotting
lavaanPlot(model = m3, edge_options = list(color = "grey"), coefs = TRUE)
