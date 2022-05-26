# the data 
auto_mpg <- read.table(paste('http://archive.ics.uci.edu/ml/',
                             'machine-learning-databases/auto-mpg/auto-mpg.data',
                             sep =''))

colnames(auto_mpg) = c("mpg", "cyl", "disp", "hp", "wt", "acc",
                       "year", "origin", "name")

head(auto_mpg)

# use the str() function to check structure of the data.
str(auto_mpg)

# remove the second column and last three columns. only concerned with continuous predictors for a continuous response.
auto_mpg <- auto_mpg[,-c(2, 7, 8, 9)]
# remove observations where hp=¡°?¡±
auto_mpg <- subset(auto_mpg, auto_mpg$hp != "?")
# change hp from a character to a numeric variable
auto_mpg$hp <- as.numeric(auto_mpg$hp)

# visualize the data with a scatter plot matrix using the pairs() function
pairs(auto_mpg)

# fit the model using the lm() function
mdl <- lm(mpg ~ wt + hp, data = auto_mpg)
(mdl_sum <- summary(mdl))

# compute the value of the coefficients directly from the data using the least squares equation
X <- cbind(rep(1, nrow(auto_mpg)), auto_mpg$wt, auto_mpg$hp)
# use the functions solve() and t() to find the inverse and transpose of the design matrix X.
# (X'X)^-1X'y.
solve(t(X) %*% X) %*% t(X) %*% auto_mpg$mpg

# Hypothesis Tests
# Hypothesis Test for a Single 
mdl_sum$coefficients
# Hypothesis Test for Significance of Regression
# ANOVA F-statistic
(fstat <- mdl_sum$fstatistic)
# p-value
pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

# Another way
# fit the null model using lm() and compare it to the full model using the anova() function.
null_mdl <- lm(mpg ~ 1, data = auto_mpg)
anova(null_mdl, mdl)

# We can compare other subsets of the full model (aka nested models).
wt_mdl <- lm(mpg ~ wt, data = auto_mpg)
anova(wt_mdl, mdl)

# Confidence Intervals
# Confiden Intervals for a single
# 90% confidence interval
confint(mdl, level = 0.9)

# Prediction
# avoid extrapolation
x_new <- data.frame(wt = 2500, hp = 150)
predict(mdl, newdata = x_new, interval = 'prediction', level = 0.9)

# Coefficient of Determination
c(mdl_sum$r.squared, mdl_sum$adj.r.squared)
