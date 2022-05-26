library(faraway)
data(state)
state_data <- data.frame(state.x77, row.names = state.abb)
head(state_data)

# Stepwise Regression using AIC
state_mdl <- lm(Life.Exp ~ ., data = state_data)

step(state_mdl, direction = "both")
# update() function
update(state_mdl, . ~ . - Area)

state_mdl
