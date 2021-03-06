###############################################
### Moderation and Moderated Mediation Demo ###
###############################################

library(lavaan)
library(lavaanPlot)
library(tidyverse)
library(semTools)
library(haven) #read in SPSS data 


data <- read_sav("Data/mediation_data.sav")
View(data)

# mean centre variables using scale()

df <- data %>%
mutate(across(c(read, write, math, science, socst), 
              scale,
              .names = "{col}_c")) 


### Moderation Demo - Observed Variables ###

#Set seed
set.seed(13)

#Create interaction effect
df <- df %>% 
        mutate(socstXmath = socst_c * math_c)

mod_model <- '
  # regressions
  read ~ b1*socst_c
  read ~ b2*math_c
  read ~ b3*socstXmath
  
  # define mean parameter label for centred math for use in simple slopes
  math_c ~ math.mean*1
  
  # define variance parameter label for centred math for use in simple slopes
  math_c ~~ math.var*math_c
  
  # simple slopes for condition effect
  SDbelow := b1 + b3*(math.mean - sqrt(math.var))
  mean := b1 + b3*(math.mean)
  SDabove := b1 + b3*(math.mean + sqrt(math.var))
  '

m1 <- sem(model = mod_model,
            data = df, 
            se = "bootstrap",
            bootstrap = 1000)

summary(m1, fit.measures = TRUE)
parameterEstimates(m1, boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE, standardized = FALSE)


##################################
#### Moderated Mediation Demo ####
##################################

modmed_model <- '
  # regressions
    read_c ~ a1*socst_c
    read_c ~ a2*math_c
    read_c ~ a3*socstXmath
    science ~ b1*read_c
    science ~ cdash*socst_c
    

  # define mean parameter label for centred math for use in simple slopes
  math_c ~ math.mean*1
  
  # define variance parameter label for centred math for use in simple slopes
  math_c ~~ math.var*math_c

  # index of moderated mediation
    imm := a3*b1    

  # indirect effects conditional on moderator 
    indirect.SDbelow := a1*b1 + a3*-sqrt(math.var)*b1
    indirect.mean := a1*b1 + a3*math.mean*b1
    indirect.SDabove := a1*b1 + a3*sqrt(math.var)*b1

'
m2<- sem(model = modmed_model, data = df, se = "bootstrap", bootstrap = 1000)
summary(m2)
