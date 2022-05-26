# Data Entry
forbes_data <- read.csv(file = "http://users.stat.umn.edu/~sandy/alr4ed/data/Forbes.csv",
                        header = TRUE,
                        sep = ',',
                        row.names = 1)

# Looking at the data 
# head() look at the first few rows of the data 
head(forbes_data)

# summary() get univariable numerical summaries
summary(forbes_data)

# boxplot() create a boxplot for each variable
boxplot(forbes_data,
        main = 'Boxplot of forbes_data')

# Simple Linear Regression
# bp will be the response and lpres will be the predictor.
# plot() scatter plot
plot(formula = lpres ~ bp,
     data = forbes_data,
     main = "lpres VS. bp",
     xlab = "bp",
     ylab = "lpres")

# lm() to fit linear regression models
my_mdl <- lm(formula = lpres ~ bp,
             data = forbes_data)

# brief summary of the model, call
my_mdl

# extract the coefficient values, we use $ followed by name.
my_mdl $ coefficients

# get a single value(intercept & slope)
my_mdl $ coefficients[1]
my_mdl $ coefficients[2]

# get a more elaborate summary
summary(my_mdl)

# abline() add the regression line to the scatter plot of the data 
# argument col to specify the color of the line
plot(formula = lpres ~ bp,
     data = forbes_data,
     main = 'lpres VS. bp',
     xlab = 'bp',
     ylab = 'lpres')

abline(reg = my_mdl,
       col = 'red')

# manually set the regression line values using the input a for the intercept and b for the slope
abline(a = my_mdl $ coefficients[1],
       b = my_mdl $ coefficients[2],
       col = 'red')


