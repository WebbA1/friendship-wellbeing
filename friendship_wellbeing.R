# Data has been anonymised and cleaned prior to upload to Github.
load(file = 'friendship_wellbeing.Rdata')

# Libraries to be installed/loaded for analysis.
library(memisc)
library(prettyR)
library(car)
library(psych)
library(coin)

# Create function to change factor into numeric.
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}

# Convert factor data for analysis into numeric.
df[,1] <- as.numeric.factor(df[,1])

for (i in 13:74) { # Friendship Questionnaire
  df[,i] <- as.numeric.factor(df[,i])
}

for (i in 75:86){  # Emotional Wellbeing Questionnaire
  df[,i] <- as.numeric.factor(factor(df[,i], labels = c(1,2,3,4,5), levels = c('Never','Rarely','Sometimes','Often','Always')))
}

# Score the Friendship Questionnaire (see http://www.midss.org/mcgill-friendship-questionnaire-friendship-functions)
# Abbrvs. MP (Stimulating Companionship) LP (Help) NT (Intimacy) EL (Reliable Alliance) EC (Emotional Security) AL (Self Validation)
df$male.mp <- (df$mfq.male.4 + df$mfq.male.7 + df$mfq.male.18 + df$mfq.male.22 + df$mfq.male.28)/5
df$male.lp <- (df$mfq.male.1 + df$mfq.male.9 + df$mfq.male.13 + df$mfq.male.24 + df$mfq.male.27)/5
df$male.nt <- (df$mfq.male.3 + df$mfq.male.8 + df$mfq.male.15 + df$mfq.male.21 + df$mfq.male.29)/5
df$male.el <- (df$mfq.male.5 + df$mfq.male.12 + df$mfq.male.16 + df$mfq.male.20 + df$mfq.male.26)/5
df$male.ec <- (df$mfq.male.2 + df$mfq.male.11 + df$mfq.male.14 + df$mfq.male.23 + df$mfq.male.30)/5
df$male.al <- (df$mfq.male.6 + df$mfq.male.10 + df$mfq.male.17 + df$mfq.male.19 + df$mfq.male.25)/5

df$female.mp <- (df$mfq.female.4 + df$mfq.female.7 + df$mfq.female.18 + df$mfq.female.22 + df$mfq.female.28)/5
df$female.lp <- (df$mfq.female.1 + df$mfq.female.9 + df$mfq.female.13 + df$mfq.female.24 + df$mfq.female.27)/5
df$female.nt <- (df$mfq.female.3 + df$mfq.female.8 + df$mfq.female.15 + df$mfq.female.21 + df$mfq.female.29)/5
df$female.el <- (df$mfq.female.5 + df$mfq.female.12 + df$mfq.female.16 + df$mfq.female.20 + df$mfq.female.26)/5
df$female.ec <- (df$mfq.female.2 + df$mfq.female.11 + df$mfq.female.14 + df$mfq.female.23 + df$mfq.female.30)/5
df$female.al <- (df$mfq.female.6 + df$mfq.female.10 + df$mfq.female.17 + df$mfq.female.19 + df$mfq.female.25)/5

df$male.total <- rowSums(df[,c('male.mp','male.lp','male.nt','male.el','male.ec','male.al')])
df$female.total <- rowSums(df[,c('female.mp','female.lp','female.nt','female.el','female.ec','female.al')])

# Score the Emotional Wellbeing Questionnaire (see https://internal.psychology.illinois.edu/~ediener/SPANE.html)
df$pleasant <- rowSums(df[,c('ew.1','ew.3','ew.5','ew.6','ew.8','ew.12')])
df$unpleasant <- rowSums(df[,c('ew.2','ew.4','ew.7','ew.9','ew.10','ew.11')])
df$balance <- df$pleasant - df$unpleasant

# Exclusion of some participants on the basis of low sample size (n).
df$gender.exclusions <- factor(df$gender, levels = c('Male','Female'), labels = c('Male','Female'))
df$sexuality.exclusions <- factor(df$sexuality, levels = c('Heterosexual','Homosexual','Bisexual'), labels = c('Heterosexual','Homosexual','Bisexual'))

# Compute dichotomous variables to determine male sexual interest + female sexual interest.
for (i in 1:129){
  if (df[i,'sexuality'] == 'Bisexual'){
    df[i,'male.interest'] <- 'Yes'
  }
  else if (df[i,'gender'] == 'Male' && df[i,'sexuality'] == 'Homosexual'){
    df[i,'male.interest'] <- 'Yes'
  }
  else if (df[i,'gender'] == 'Female' && df[i,'sexuality'] == 'Heterosexual'){
    df[i,'male.interest'] <- 'Yes'
  }
  else if (df[i,'sexuality'] == 'Prefer not to say' | df[i,'sexuality'] == 'Other'){
    df[i,'male.interest'] <- NA
  }
  else {
    df[i,'male.interest'] <- 'No'
  }
}

for (i in 1:129){
  if (df[i,'sexuality'] == 'Bisexual'){
    df[i,'female.interest'] <- 'Yes'
  }
  else if (df[i,'gender'] == 'Female' && df[i,'sexuality'] == 'Homosexual'){
    df[i,'female.interest'] <- 'Yes'
  }
  else if (df[i,'gender'] == 'Male' && df[i,'sexuality'] == 'Heterosexual'){
    df[i,'female.interest'] <- 'Yes'
  }
  else if (df[i,'sexuality'] == 'Prefer not to say' | df[i,'sexuality'] == 'Other'){
    df[i,'female.interest'] <- NA
  }
  else {
    df[i,'female.interest'] <- 'No'
  }
}

df$male.interest <- factor(df$male.interest, labels = c('No','Yes'), levels = c('No','Yes'))
df$female.interest <- factor(df$female.interest, labels = c('No','Yes'), levels = c('No','Yes'))

# Create numeric variable for person.contact for inclusion in GLM.
df$person.male.numeric <- as.numeric.factor(factor(df$person.contact.male, labels = c(5,4,3,2,1), levels = c('Daily','Weekly','Monthly','Every few months','Yearly')))
df$person.female.numeric <- as.numeric.factor(factor(df$person.contact.female, labels = c(5,4,3,2,1), levels = c('Daily','Weekly','Monthly','Every few months','Yearly')))

# Three participants identified as outliers - did not report male friendship. Exclude from analysis df[-no.male,].
no.male = c(20,21,40)

# Set contrast codes to Helmert.
options(contrasts = c('contr.helmert','contr.poly'))

# Hypothesis 1: does sexual interest affect the intimacy of female friendships?
intimate.anova.female <- lm(female.nt ~ female.interest, data = df)
summary(intimate.anova.female)

# Does not meet GLM assumptions.
shapiro.test(residuals(intimate.anova.female))
hist(residuals(intimate.anova.female))

# Try transform dependent variable. Still does not meet assumptions.
intimate.anova.female <- lm(log(female.nt) ~ female.interest, data = df)
shapiro.test(residuals(intimate.anova.female))
hist(residuals(intimate.anova.female))

intimate.anova.female <- lm(sqrt(female.nt) ~ female.interest, data = df)
shapiro.test(residuals(intimate.anova.female))
hist(residuals(intimate.anova.female))

# Use non-parametric alternative, as GLM assumptions are not met.
# Sexual interest in females does not predict any less intimate relationships with females (p=.15).
wilcox_test(female.nt ~ female.interest, data = df)

median(df$female.nt[df$female.interest == 'Yes'], na.rm = TRUE)
median(df$female.nt[df$female.interest == 'No'], na.rm = TRUE)

# Hypothesis 2: does sexual interest affect the intimacy of male friendships?
intimate.anova.male <- lm(male.nt ~ male.interest, data = df[-no.male,])
summary(intimate.anova.male)

# Does not meet GLM assumptions.
shapiro.test(residuals(intimate.anova.male))
hist(residuals(intimate.anova.male))

# Try transform dependent variable. Still does not meet assumptions.
intimate.anova.male <- lm(log(male.nt) ~ male.interest, data = df[-no.male,])
shapiro.test(residuals(intimate.anova.male))
hist(residuals(intimate.anova.male))

intimate.anova.male <- lm(sqrt(male.nt) ~ male.interest, data = df[-no.male,])
shapiro.test(residuals(intimate.anova.male))
hist(residuals(intimate.anova.male))

# Use non-parametric alternative, as GLM assumptions are not met.
# Sexual interest in males does not predict any less intimate relationships with males (p=.48).
wilcox_test(male.nt ~ male.interest, data = df[-no.male,])

median(df[-no.male,]$male.nt[df$male.interest == 'Yes'], na.rm = TRUE)
median(df[-no.male,]$male.nt[df$male.interest == 'No'], na.rm = TRUE)

# Hypothesis 3: does sexual interest affect quality of female friendships?
total.anova.female <- lm(female.total ~ female.interest, data = df)
summary(total.anova.female)

# Does not meet GLM assumptions.
shapiro.test(residuals(total.anova.female))
hist(residuals(total.anova.female))

# Try transform dependent variable. Still does not meet assumptions.
total.anova.female <- lm(log(female.total) ~ female.interest, data = df)
shapiro.test(residuals(total.anova.female))
hist(residuals(total.anova.female))

total.anova.female <- lm(sqrt(female.total) ~ female.interest, data = df)
shapiro.test(residuals(total.anova.female))
hist(residuals(total.anova.female))

# Use non-parametric alternative, as GLM assumptions are not met.
# Significant difference in quality of female friendship between those with and without sexual interest in females.
wilcox_test(female.total ~ female.interest, data = df)

# Total quality of female friendship is significantly better without a sexual interest.
median(df$female.total[df$female.interest == 'Yes'], na.rm = TRUE)
median(df$female.total[df$female.interest == 'No'], na.rm = TRUE)

# Hypothesis 4: does sexual interest affect quality of male friendship?
total.anova.male <- lm(male.total ~ male.interest, data = df[-no.male,])
summary(total.anova.male)

# Does not meet GLM assumptions.
shapiro.test(residuals(total.anova.male))
hist(residuals(total.anova.male))

# Try transform dependent variable. Still does not meet assumptions.
total.anova.male <- lm(log(male.total) ~ male.interest, data = df[-no.male,])
shapiro.test(residuals(total.anova.male))
hist(residuals(total.anova.male))

total.anova.male <- lm(sqrt(male.total) ~ male.interest, data = df[-no.male,])
shapiro.test(residuals(total.anova.male))
hist(residuals(total.anova.male))

# Use non-parametric alternative, as GLM assumptions are not met.
# No significant difference in quality of male friendship between those with and without sexual interest in males.
wilcox_test(male.total ~ male.interest, data = df[-no.male,])

median(df[-no.male,]$male.total[df$male.interest == 'Yes'], na.rm = TRUE)
median(df[-no.male,]$male.total[df$male.interest == 'No'], na.rm = TRUE)

# Hypothesis 5: do qualities of friendship predict better emotional wellbeing in the past 4 weeks?
wellbeing.model <- lm(balance ~ gender.exclusions + age + 
                      male.mp + male.lp + male.nt + male.el + male.ec + male.al + 
                      female.mp + female.lp + female.nt + female.el + female.ec + female.al + 
                      person.male.numeric + person.female.numeric,
                      data = df[-no.male,])
summary(wellbeing.model)

# Test GLM assumptions.
shapiro.test(residuals(wellbeing.model))
hist(residuals(wellbeing.model))

# Test for homoscedasticity.
ncvTest(wellbeing.model)

# Multicollinearity is found - update model to remove two variables (maleEC + maleAL).
vif(wellbeing.model)

# Create model that removes maleEC + maleAL to resolve collinearity.
wellbeing.model2 <- lm(balance ~ gender.exclusions + age + 
                       male.mp + male.lp + male.nt + male.el +
                       female.mp + female.lp + female.nt + female.el + female.ec + female.al + 
                       person.male.numeric + person.female.numeric,
                       data = df[-no.male,])
summary(wellbeing.model2)

# Test GLM assumptions.
shapiro.test(residuals(wellbeing.model2))
hist(residuals(wellbeing.model2))

# Test for homoscedasticity.
ncvTest(wellbeing.model2)

# Test for multicollinearity.
vif(wellbeing.model2)

# Test for interaction between sexual interest and significant predictor variables.
# No interaction found. Sexual interest doesn't moderate relationship. Reject Model 3.
wellbeing.model3 <- lm(balance ~ gender.exclusions + age + 
                       male.mp + male.lp + male.nt + male.el +
                       female.mp + female.lp + female.nt + female.el + female.ec + female.al + 
                       person.male.numeric + person.female.numeric +
                       person.male.numeric:male.interest + male.lp:male.interest + female.nt:female.interest,
                       data = df[-no.male,])
summary(wellbeing.model3)