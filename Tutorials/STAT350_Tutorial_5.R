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

#model where disp is treated as a factor ¨C this estimates a separate coefficient for each unique value of disp.
full_mdl <- lm(mpg ~ factor(disp), data= auto_mpg)
# create a SLR with just disp as a predictor
reduced_mdl <- lm(mpg ~ disp, data= auto_mpg)
# ANOVA
anova(full_mdl,reduced_mdl)

# Transforming the data
# Transformations on the Response
mdl_t <- lm(log(mpg) ~ wt+hp+disp, data=auto_mpg)
(mdl_t_sum <- summary(mdl_t))
# residual analysis plots
plot(mdl_t)


# Transformations on the Predictors
# a quick example(a data set whose response is some nonlinear function of a single regressor.)
set.seed(pi)
n = 25
x <- runif(n, -2, 2)
y <- 3 + 1.5*x^2 + 2*x^3 + rnorm(n, 0, 1)
plot(x, y)

# still use a linear regression model as the ¡°linear¡± part of the model is with respect to the regression coefficients
# function to I() tell R to treat x2 and x3 as is and not as operators in the formula for lm().
poly_mdl <- lm(y ~ x + I(x^2) + I(x^3))
summary(poly_mdl)
# Specifying the model in this way clearly leads to high correlation between predictor variables

# use the poly() function, specifying the degree polynomial we want, then passing that object to lm().
lm(y~poly(x, degree=3)) 
# return orthogonal variables resulting in different coefficients.

# There¡¯s a trade-off that you have to consider; can you accept multicollinearity (using I())or should you trade it at the
#loss of interpretability(using poly())? 
 

