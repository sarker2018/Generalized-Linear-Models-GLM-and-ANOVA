
# Setting up the working directory
setwd("D:/FF_project/report") # set your own directory

# Required R packages 
library(readxl)
library(car)
library(MASS)
library(dplyr)
library(ggpubr)
library(rstatix)
library(devtools)
library(mice)
library(psych)
library(reticulate)
library(lme4)
library(DHARMa)
# 'lmerTest' package give p values in the summary output
library(lmerTest) 
library(ggplot2)
library(MuMIn)
library(effects)
library(multcomp)
library(multcompView)
library(emmeans)


# Reading the data
df = read_excel("D:/FF_project/report/silene_field.xlsx", na = "NA")

# prints the data structure
str(df)


# Gets the character variable names in a vector 
f = c('range', 'pop', 'sex')

# Converting character to numeric
for (varname in f){
  df[[varname]] <- as.factor(df[[varname]])
}
df[['dam_flo']] = as.numeric(df[['dam_flo']])

# remove index column given as "ind" in the dataset
df$ind = NULL



# Setting Plot margin and padding
par(mar=c(0,2,0,2)+0 , mfrow = c(1,1))

# Creates a  Plots of all na values of each individual column
md = md.pattern(df)
md

# canvas parameter 
theme(plot.margin = margin(-2,-2,-2,-2, "cm"))
# prints out total number of na values in the console
print(paste0('Total Number of missing values: ', sum(is.na(df))))  


# Creates density of a column and save as a R object
d <- density(df$dam_flo, na.rm = T) 
plot(d, main="Kernel Density of damaged flowers", bw = 1)

# Defines the boundary color of density Plot
polygon(d, col="red", border="blue")


# Creating our own 'mode' function to replace na by mode.
getmode <- function(vec) {
   uniqvalues <- unique(vec)
   uniqvalues[which.max(tabulate(match(vec, uniqvalues)))]}
# Replacing na values in 'dam_flo' column by mode
df$dam_flo[is.na(df$dam_flo)] <- getmode(df$dam_flo)

# Few first lines of the data including the header.
head(df)


# Returns unique values in the 'range' column
unique(df$range) 

# Returns unique values in the 'pop' column
unique(df$pop) 

# Returns unique values in the 'sex' column
unique(df$sex)  


# Creates a pair Plot of correlation among predictors
# Use 'Pearson' for formula for correlation
pairs.panels(df[ ,c(1,2,3,9,10)], method = "pearson",
             hist.col = "#00AFBB",density = TRUE,ellipses = F) 


# Creates a pair Plot of correlation among predictors and responses

pairs.panels(df, method = "pearson", 
             hist.col = "#00AFBB",density = TRUE,ellipses = F)


# Fitting model: complex --> simple
m1= lm(height~range*sex*aph_nab, df)
m2= lm(height~range*sex + aph_nab, df)
m3= lm(height~range*sex, df)
m4 = lm(height~pop, df)


# Creating an empty list to save the LM models' as object.
empty_list <- vector(mode = "list")

empty_list[[1]] = m1
empty_list[[2]] = m2
empty_list[[3]] = m3
empty_list[[4]] = m4

# Creates a summary of the LM models
# and returns the 'R-Squared' and 'Adjusted R-Squared' values.
for ( i in 1:length(empty_list)) {
  r_sq = round(summary(empty_list[[i]])$r.squared, 4)
  adj.r = round(summary(empty_list[[i]])$adj.r.squared,4)
  print(paste0('Model:', i,'  R-Sq: ', r_sq, '  Adj. R-Sq: ', adj.r))}


# Fitting mixed effect model ( fixed + random effect in the same model)
# Backward model selection method (reduces variable gradually)
m5= lmer(height~range*sex +(1|pop) + (1|gdd) + (1|vegcov), REML = F, df)
m6= lmer(height~range*sex +(1|pop) + (1|gdd), REML = F, df)
m7= lmerTest::lmer(height~0 + range*sex +(1|pop), REML = F, df)
 # Compare the AICs
AIC(m5,m6,m7)


# Simulates the residuals by 'simulateResidulas() function from'
# R package 'DHARMa'
sim_m7<- simulateResiduals(fittedModel = m7, n=1000)

#Plotting simulated residuals
plot(sim_m7, quantreg = F)

#Plotting a QQ-Plot from the normal residuals.
qqnorm(resid(m7))
qqline(resid(m7))



# Simulates the residuals by 'simulateResidulas() function from'
sim_m7<- simulateResiduals(fittedModel = m7, n=1000)
plot(sim_m7, quantreg = F)


# Plotting a QQ-Plot from the normal residuals.
qqnorm(resid(m7))
qqline(resid(m7))


# Prints out the summary of model 'm7'
s = summary(m7)
s


# Chisq two way ANOVA test
a = Anova(m7,type="III",test="Chisq")
a


# Tukey pairwise group contrasting 'Post Hoc Test'
TukeyHSD(aov(df$height~df$range * df$sex))$`df$range:df$sex`


# Interaction Effect Plot of "range vs sex"
plot(Effect(c("sex", "range"), m7), multiline=TRUE,ci.style="bands", 
     se = TRUE, main = 'Sex:Range Effect Plot',
     cex.axis=1.2,cex.lab=1.3)

# Creates height vs range:sex interaction box Plot
# Defines 4 different colors for the boxes

boxplot(height~range:sex,data=df, border="gray40",
         main= 'Sex vs Range Box Plot',
        col=rep(c("indianred4","indianred2","cadetblue4","cadetblue2"),2),
        cex.axis=1.2,cex.lab=1.3, at=c(1,2,3.5,4.5))
abline(h=mean(fitted(m7)))

# Creates object of range ~ sex interaction; 4 groups: 'eu.m, us.f, us.f, us.m'
# Total number of object = size of the dataset
df$range_sex<-interaction(df$range,df$sex)

# aggregates the height in 4 different groups, and calculates the group mean
mean1<-as.data.frame(aggregate(df['height'],list(df$range_sex),mean))
mean1$x= mean1$height


#Plot mean + SE
# setting Plot position for 4 different boxes
points(c(1,2,3.5,4.5),mean1$x,pch=19,cex=2)

# Aggregating Standard Errors of height column according to 4 different groups
# ('eu.m, us.f, us.f, us.m') in range_sex variable
SE<-as.data.frame(aggregate(df['height'],list(df$range_sex),
                            function(x) sd(x)/sqrt(length(x))))
SE$x= SE$height
arrows(c(1,2,3.5,4.5),mean1$x+SE$x,c(1,2,3.5,4.5),
       mean1$x-SE$x,code=3,length=0.25,angle=90)


# Fitting a normal glm model to find an initial assumption
poisson <- glm(dam_flo~sex*range+gdd+vegcov, df,
               family= poisson(link = "log")) 

# Simulating the residuals
sim_poisson<- simulateResiduals(poisson,n=1000)



# Plotting simulated residuals
plot(sim_poisson, quantreg = FALSE)



# Performs a dispersion test and saves in a object
tst <- testOverdispersion(poisson)

# Returns dispersion and p values
tst$statistic
tst$p.value



# Creating a negative binomial model for Poisson data.
nbglm <- glm.nb(dam_flo~sex + range + pop + vegcov + gdd, data = df)

# simulate and plot NB models or selected models
t <- simulateResiduals(nbglm,refit = F, n = 100)
plot(t)

# Perform a dispersion test of negative binomial model
# Here, the Null Hypothesis is that data are overdispersed
testDispersion(t, alternative = 'greater')

# Creates few more models by Backward Selection method
m40 <- glm.nb(dam_flo~sex * range + pop + vegcov + gdd, data = df)
m41 <- glm.nb(dam_flo~sex * range + pop + vegcov, data = df)
m42 <- glm.nb(dam_flo~sex * range + pop, data = df)
m43 <- glm.nb(dam_flo~0 + sex * range, data = df)
m44 <- glm.nb(dam_flo~sex + range, data = df)

# Looking for the model with minimum AIC
AIC(nbglm,m40,m41,m42,m43,m44)

# Prints out summary statistics of optimal model 'nbglm'
summary(aov(nbglm))

# few alternatives of pairwise test: not evaluated here

emmeans(nbglm, list(pairwise ~ range*sex), 
        adjust = "tukey")$`pairwise differences of range, sex`



# simulating and plotting of the selected NB model
t <- simulateResiduals(m43,refit = F, n = 100)
plot(t)


# dispersion test for the selected model
testDispersion(t, alternative = 'greater')


# Pairwise Tukey adjusted group contrasts 
summary(glht(m43,  test = adjusted("Tukey"))) 


# Saves the pairwise group contrasts in a object and 
# returns object's elements
pair_comp <- emmeans(m43, list(pairwise ~ range*sex), adjust = "tukey")
# pair_comp$`emmeans of range, sex`
pair_comp$`pairwise differences of range, sex`



# Creates pairwise Tukey adjusted contrast object
leastsquare1 = lsmeans(m43, pairwise~sex*range, 
                      adjust="tukey", ordered = TRUE)

# Sets up box positions on the Plot 

position<-c(1,2,3.5,4.5)
# Box Plot
boxplot(dam_flo~sex:range,data=df, border="gray40",
        col=rep(c("indianred4","indianred2","cadetblue4","cadetblue2"),2),
        at=position,
        main= 'Sex vs Range Box Plot',
        cex.axis=1.2,cex.lab=1.3,
        outline = FALSE)

# saves as a dataframe
plot<-as.data.frame(leastsquare1$lsmeans)

# Adds points at boxes' mean points
points(position,plot$lsmean,pch=19,cex=1)
arrows(position,plot$lsmean+plot$SE,position,plot$lsmean-plot$SE,code=3,
       length=0.15,angle=90)





par(mar=c(0,0,0,0)+0 , mfrow = c(1,1))

# Creates a  Plots of all na values of each individual column

# canvas parameter
# theme(plot.margin = margin(-2,-2,-2,-2, "cm"))


# Main and Interaction Effect Plot 
plot(Effect(c("sex", "range"), m43),
     multiline=TRUE,ci.style="bands", se = TRUE,
     main = 'Sex:Range Effect Plot',
     cex.axis=1.2,cex.lab=1.3)


## 

## library(knitr) # collect all the R chunks in one R-script
## knitr::purl("test2.Rmd", documentation = 0)
