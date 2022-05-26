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
mdl <- lm(mpg ~ wt + hp + disp, data = auto_mpg)
(mdl_sum <- summary(mdl))

# Hidden Extrapolation
# regressor variable hull(RVH)
X <- cbind(rep(1,nrow(auto_mpg)),auto_mpg$wt,auto_mpg$hp,auto_mpg$disp)
H <- X %*% solve(t(X) %*% X) %*% t(X)
(h_max <- max(diag(H)))

# Now we will define a new point x0 and check it¡¯s location relative to the RVH.
x_0 <- data.frame(wt=2500,hp=150,disp=300)
x_0 <- as.matrix(cbind(1,x_0),nrow=1)
(h_00 <- x_0 %*% solve(t(X) %*% X) %*% t(x_0))

# Standardized Regression Coefficients
# compute the means and standard deviations all variables in one line.
auto_means <- apply(auto_mpg, 2, FUN = mean)
auto_sds <- apply(auto_mpg, 2, FUN = sd)

# using scale()
auto_scaled <- as.data.frame(scale(auto_mpg, center=TRUE, scale=TRUE))
# As a check, we can make sure the means of each column in the data are zero
apply(auto_scaled, 2,FUN=mean)
# their standard deviations are one
apply(auto_scaled, 2,FUN=sd)

# fit a new linear regression model on this data
mdl_scaled <- lm(mpg ~ -1 + wt + hp + disp, data=auto_scaled)
(mdl_scaled_sum <- summary(mdl_scaled))

# Multicollinearity
# calculate the VIF for each predictor in our initial model
#
wt_mdl <- lm(wt ~ hp+disp,data=auto_mpg)
wt_sum <- summary(wt_mdl)
wt_vif <- 1/(1-wt_sum$r.squared)
#
hp_mdl <- lm(hp ~ wt+disp,data=auto_mpg)
hp_sum <- summary(hp_mdl)
hp_vif <- 1/(1-hp_sum$r.squared)
#
disp_mdl <- lm(disp ~ wt+hp,data=auto_mpg)
disp_sum <- summary(disp_mdl)
disp_vif <- 1/(1-disp_sum$r.squared)
#
c(wt_vif,hp_vif,disp_vif)

# or using vif function in faraway package
library(faraway)
vif(mdl)

# Model Assessment
# Residual Analysis
# residual plot
par(mfrow=c(2,2))
plot(mdl)

# Residuals vs. Fitted
plot(mdl,which=1)
# Normal Q-Q
plot(mdl, which=2)
# Scale-Location
plot(mdl,which=3)
# Residuals vs. Leverage
plot(mdl,which=5)
# Residual vs. Index
plot(mdl$residuals)
# plot succesive pairs of residuals
n <- nrow(auto_mpg)
plot(mdl$residuals[1:(n-1)], mdl$residuals[2:n])
