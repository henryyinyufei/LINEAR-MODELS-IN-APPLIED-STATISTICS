library(faraway)
library(tidyverse)
library(caret)
data(sexab)
str(sexab)

# boxplots for ptsd given for the levels of csa
ggplot(sexab, aes(x = csa, y = ptsd, fill = csa)) + geom_boxplot(show.legend = FALSE)

# scatter plot for ptsd given cpa where we have set the colour to indicate the level of csa
ggplot(sexab, aes(x = cpa, y = ptsd, color = csa)) + geom_point()

# code dummy variables
# dummyVars() from caret package for large number of dummy variables
dum1 <- ifelse(sexab$csa == "Abused", yes = 1, no = 0)
dum2 <- ifelse(sexab$csa == "NotAbused", yes =1, no = 0)

#
a <- model.matrix(~csa-1, sexab)
a
cbind(sexab,a)

# dummyVars()
b <- dummyVars(~csa, sexab)
data.frame(predict(b, newdata = sexab))

# x = 0 for the dummy variable is known as the reference level.
# make sure that R uses this a the reference level through the relevel() function
sexab$csa <- relevel(sexab$csa, ref = "NotAbused") 
levels(sexab$csa)

# fit the full model with interaction.
mdl_full <- lm(ptsd ~ cpa*csa, data = sexab)
summary(mdl_full)

# The first thing to notice in the model summary is that the interaction term is insignificant, i.e. we reject the
#hypo. So, we¡¯ll fit a model without it.
mdl_add <- lm(ptsd ~ cpa + csa, data = sexab)
summary(mdl_add)

# use an ANOVA to test the null hypothesis if that there is no difference between the above models.
anova(mdl_full, mdl_add)

# stratified regression lines
ggplot() +
  theme_bw() +
  theme(legend.position = c(0.1, 0.85)) +
  geom_point(aes(x = cpa, y = ptsd, col = csa), sexab, size = 2) +
  geom_abline(aes(slope = mdl_add$coefficients[2],
                  intercept = mdl_add$coefficients[1],
                  col = "NotAbused"),
              size = 1.5) +
  geom_abline(aes(slope = mdl_add$coefficients[2],
                  intercept = mdl_add$coefficients[1] + mdl_add$coefficients[3],
                  col = "Abused"),
              size = 1.5)

#
ggplot(aes(x = cpa, y = ptsd, color = csa), data = sexab) +
  geom_point(size = 2) +
  geom_abline(aes(slope = mdl_add$coefficients[2],
                  intercept = mdl_add$coefficients[1], 
                  color = "NotAbused"), 
              size = 1.5) +
  geom_abline(aes(slope = mdl_add$coefficients[2],
                  intercept = mdl_add$coefficients[1] + mdl_add$coefficients[3],
                  col = "Abused"),
              size = 1.5)





