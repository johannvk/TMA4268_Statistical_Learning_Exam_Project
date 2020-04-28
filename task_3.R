library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

# Task 3:

id <- "1heRtzi8vBoBGMaM2-ivBQI5Ki3HgJTmO"  # google file ID
d.support <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                              id), header = T)
# We only look at complete cases
d.support <- d.support[complete.cases(d.support), ]
d.support <- d.support[d.support$totcst > 0, ]

# summary(d.support)

# a)
# Plot histogram of the 'totcst' variable:
ggplot(hist_df) + geom_histogram(aes(log(totcst)), bins=40)

# Plot histogram of all the numeric variables in d.support:
hist_df = select_if(d.support, is.numeric)
str(hist_df)

hp = list()
hist_bin_nums = c(30, 2L*length(unique(hist_df$num.co)), 1L*length(unique(hist_df$edu)), 30, 40, 40, 40, 30, 30, 30)
hist_names = names(hist_df); hist_y_labs = c(1, 4, 7, 8)
for(i in 1:length(hist_df)){
  hp[[i]] = ggplot(hist_df) + geom_histogram(aes_string(x=hist_names[[i]]), bins=hist_bin_nums[[i]]) +
    labs(y = "", cex.labs = 2.5)
  if(i %in% hist_y_labs){hp[[i]] = hp[[i]] + labs(y = "Count")}
}
grid_layout= rbind(c(1, 2, 3), c(4, 6, 6), c(7, 7, 5), c(8, 9, 10))
grid.arrange(hp[[1]], hp[[2]], hp[[3]], hp[[4]], hp[[5]], hp[[6]], hp[[7]], hp[[8]], hp[[9]], hp[[10]],
             layout_matrix=grid_layout, top="Support Histograms")

# Suggest to log-transform the 'totcst' variable. It then looks closer to a normal distribution which might be modelled by linreg.
ggplot(hist_df, aes(log(totcst))) + geom_histogram()
d.support$log_totcst = log(d.support$totcst)
# View(d.support)

# b)
# i: Fit a linear model with log_totcst as the response:
lm_log_totcst = lm(log_totcst ~ age + temp + edu + resp + num.co + dzgroup, data=d.support)
summary(lm_log_totcst)

coef(lm_log_totcst)[2]
lm_log_totcst_age_coeff = coef(lm_log_totcst)[2]
ten_year_cost_increase = exp(lm_log_totcst_age_coeff*10)  # In units of [Money]. 

# ii: Tukey-Anscombe plot (Residuals VS. Fitted) and Normal Q-Q plot:
par(mfrow=c(1, 2))  
plot(lm_log_totcst, which=c(1), main="Tukey-Anscombe")
plot(lm_log_totcst, which=c(2), main="Quantile-Quantile")
# Answer questions about the linear-regression assumptions.
# Linear-response: Residuals versus fitted has no clear trend, indicates that the linear response assumptio is Good!
# Homoscedastic error terms: QQ-plot shows good normality of error variances, and zero mean. Slight issue with the 
#                            distribution of residuals vs. fitted, as there are fewer fitted data points with intermediate values. 
#                            The variance still appears reasonably constant over the range of fitted values.
par(mfrow=c(1, 1))

# iii: Perform formal test of whether or not the effect of age depends on disease group (dzgroup)
# Effectively testing the restriction that beta_{age:dzgroup_j} = 0 for j = 1,.., 7 vs. Any of them being non-zero.
anova(lm(log_totcst ~ age + dzgroup, data=d.support), lm(log_totcst ~ age * dzgroup, data=d.support))
# Exact F-test:
# We find a p-value for the F-test under the distribution F_{7, 4944=(n - (1 + 1 + 7 + 7))} of 1.47e-4. 
# Significant below the alpha=1.0e-3 level. Reject the null hypothesis that the effect on cost of age does 
# not depend on the disease group.


# c):





