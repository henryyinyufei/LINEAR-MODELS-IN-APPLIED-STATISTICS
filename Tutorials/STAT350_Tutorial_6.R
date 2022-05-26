library(faraway)
data(savings)
head(savings)

# using . to the right of the ~ tells R to use all other variables in the model.
mdl <- lm(sr ~ ., data=savings)
(mdl_sum <- summary(mdl))

# check assumptions
plot(mdl)
plot(mdl$residuals,ylab="Residuals")

# Leverage
2*ncol(savings)/nrow(savings)

# A quick way to get the influences of points in R is with the influence() function, using the model object
#as it¡¯s only argument. The hat component of the resulting object gives us the diagonal of the hat matrix
mdl_inf <- influence(mdl)
sort(mdl_inf$hat, decreasing = TRUE)

# The halfnorm() function of the faraway package provides a useful way to visualize these leverages.
halfnorm(mdl_inf$hat, labs = names(mdl_inf$hat), ylab='Leverage')

# Influence

# Cook's Distance
# calculate the cook¡¯s distance for our model using the cooks.distance() function
mdl_cook <- cooks.distance(mdl)
sort(mdl_cook, decreasing = TRUE)

# How does the model change if we delete Libya from the data set?
mdl_lib <- lm(sr ~ ., savings, subset = row.names(savings) != 'Libya')
summary(mdl_lib)
