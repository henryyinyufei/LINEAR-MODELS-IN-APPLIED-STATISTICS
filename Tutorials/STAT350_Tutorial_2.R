# continue with the example from last tutorial
forbes_data <- read.csv(file = "http://users.stat.umn.edu/~sandy/alr4ed/data/Forbes.csv",
                        header = TRUE,
                        sep = ',',
                        row.names = 1)

my_mdl <- lm(formula = lpres ~ bp,
             data = forbes_data)

# save the model summary object to my_sum.
my_sum <- summary(my_mdl)
my_sum

# Hypothesis Test
# all values we need can be found in the summary:
my_sum $ coefficients

# F-statistic
my_sum $ fstatistic

# F-statistic is just the squared t-value for the estimate of the slope.
my_sum $ coefficients[2,3]^2

# intervals(beta)
# we assign the estimated coefficients and their standard errors to their own objects.
beta_hat <- my_sum $ coefficients[,1]
beta_ses <- my_sum $ coefficients[,2]

# compute the critical t-value
(tval <- qt(p = 0.975, df = my_mdl $ df))

# compute the CI
(CIs <- data.frame(estimate = beta_hat,
                  lower = beta_hat - tval*beta_ses,
                  upper = beta_hat + tval*beta_ses))
# using confint function
confint(my_mdl,level=0.95)

# intervals(sigma)
# residual sum of squares:
(SS_res <- sum(my_mdl $ residuals^2))

# divide that by the degrees of freedom
(sigma2_hat <- SS_res/my_mdl$df.residual)

# 
(chi_up <- qchisq(0.975, my_mdl$df.residual))
(chi_low <- qchisq(0.025, my_mdl$df.residual))

# CI
(CI_sigma <- data.frame(estimate = sigma2_hat,
                        lower = my_mdl$df.residual*sigma2_hat/chi_low,
                        upper = my_mdl$df.residual*sigma2_hat/chi_up))

# prediction
# avoid extrapolation, check the range
range(forbes_data$bp)

# prediction at bp = 207
predict(my_mdl, newdata = data.frame(bp = 207))

# If we were to omit the newdata argument the function returns the fitted values
predict(my_mdl)

# The predict function can also give us intervals
# CI
(conf_int <- predict(my_mdl,
                     newdata = data.frame(bp = 207),
                     interval = 'confidence'))

# PI
(pred_int <- predict(my_mdl,
                     newdata = data.frame(bp = 207),
                     interval = 'prediction'))

# Coefficient of Determination
my_sum $ r.squared

# is just the squared correlation between two variables
cor(forbes_data$bp,forbes_data$lpres)^2

# adjusted R2
my_sum$adj.r.squared
