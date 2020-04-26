library(ggplot2)
library(car)

# Task 2:
id <- "1CA1RPRYqU9oTIaHfSroitnWrI6WpUeBw"  # google file ID
d.corona <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                             id), header = T)
d.corona$country = as.factor(d.corona$country)
d.corona$sex = as.factor(d.corona$sex)

# a):
# Deceased versus country:
table(d.corona$deceased, d.corona$country)

# Deceased versus sex:
table(d.corona$deceased, d.corona$sex)

# Deceased versus sex and country:
tri_table = xtabs(~ sex + deceased + country, data=d.corona)
ftable(tri_table)

# b):
corona_logmod = glm(deceased ~ sex + age + country, family=binomial(link='logit'), data=d.corona)
summary(corona_logmod)
contrasts(d.corona$country)
contrasts(d.corona$sex)

# Multiple choice questions:
# i: False, difference between France and Japan and Korea is significant.
# ii: False, only means that the difference between France and Indonesia is not significant. 
# iii: True, odds increase by exp(10*beta_age) = 1.974
# iv: False, the odds increase by 3.12, not the probability. 

# c): 
# Make a "prediction plot" for the corona_logmod, with different countries and sexes:
# ggplot-fun!

pred_df = data.frame(expand.grid(sex = c("male","female"), age = seq(20, 100, by=0.5), country=c("France", "indonesia", "japan", "Korea")))
pred_df$prob_deceased = 1/(1 + exp(-predict.glm(corona_logmod, newdata=pred_df)))
# View(pred_df)
# summary(pred_df)

deceased_prob_plot = ggplot(data=pred_df, aes(x=age, y=prob_deceased, color=country, type=sex)) + 
  geom_line(aes(linetype=sex, color=country), size=1.2) + theme(legend.position="right") + 
  labs(title="Logistic model of death due to Covid-19", y="Predicted prob. of dying of Covid-19", x="Age") + 
  theme_grey(base_size = 14)
deceased_prob_plot


# d): Answer four questions:

# i: Have males generally a higher probability to die of coronavirus than females?
c.sex_logmod = glm(deceased ~ sex, family=binomial(link='logit'), data=d.corona)
# print(corona_sex_logmod)
c.sex_logmod_sum = summary(c.sex_logmod) # Significant coefficient, with increase in odds-ration for males of 2.675.
coef(c.sex_logmod_sum)
# str(c.sex_logmod_sum)
c.sex_logmod_sum

# ii: Is age a greater risk factor for males than for females?
c.sex_age_logmod = glm(deceased ~ sex + age, family=binomial(link='logit'), data=d.corona)
summary(c.sex_age_logmod)
vif(c.sex_age_logmod)

c.sex_age_interaction_logmod = glm(deceased ~ sex * age, family=binomial(link='logit'), data=d.corona)
summary(c.sex_age_interaction_logmod)
vif(c.sex_age_interaction_logmod)
# The sex-difference and interaction term sex:age both have very high variance-inflation-factors, indicating a high level of colinearity.
# As neither the sex-difference or sex-difference-age-interaction term are significant in the model including interactions between
# age and sex, we cannot say that age is a greater risk factor for males than for females based on this data set.

# iii: Is age a greater risk factor for the French population than for the Korean population?
c.country_age_logmod = glm(deceased ~ country + age, family=binomial(link='logit'), data=d.corona)
summary(c.country_age_logmod)
vif(c.country_age_logmod)

c.country_age_interaction_logmod = glm(deceased ~ country * age, family=binomial(link='logit'), data=d.corona)
summary(c.country_age_interaction_logmod)
vif(c.country_age_interaction_logmod)
# With a p-value of countryjapan:age of 0.1097, the coefficient measuring the change in probability-slope for age given 
# a case from France or Japan, we cannot reject the null hypothesis that there is no change in age as a risk factor between
# the French and Japanese population. 

# e): Why could it be that it looks like the mortality rate is greater in France than in the other countries:
# Could have gathered data from a different population than the other countries. In the sense that the French may have
# only tested patients admitted to the hospital, whilst the practice in other countries might have been to 
# test more broadly. This could influence the proportion of people registered as a deceased in the data set, 
# as those already admitted to hospital in France might be at greater risk of dying than a more representative
# subset of the population.

# f): 
# i:   True
# ii:  True
# iii: False (Could "want" more false positive, classify "healthy"-people into the "deceased"-category for extra checks.)
summary(as.factor(d.corona$deceased))
# iv:  False (Could lower the probability for getting flagged as "deceased"-category.)
