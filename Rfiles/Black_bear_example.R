library(tidyverse)

black_bear <- read_csv("D:/Simon Fraser University/2020 fall/STAT 350/Rfile/black_bear_2019.csv")

data1 <- filter(black_bear, WMU != "Total")

ggplot(data1, aes(x = `Active Hunters`, y = Harvest)) +
  geom_point() 

ggplot(data1, aes(x = `Active Hunters`, y = Harvest, color = WMU)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Hunters and Harvest in different WMUs")

ggplot(data1, aes(x= sqrt(`Active Hunters`), y = sqrt(Harvest), color = WMU)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Hunters and Harvest in different WMUs")

result <- aov(Harvest ~ `Active Hunters` + `Active Hunters`:WMU -1, data = data1)
summary(result)

mdl <- lm(Harvest ~ `Active Hunters` + `Active Hunters`:WMU -1, data = data1)
summary(mdl)

par(mfrow = c(2,2))
plot(mdl)

result_transformation <- aov(sqrt(Harvest) ~ sqrt(`Active Hunters`) + sqrt(`Active Hunters`):WMU -1, data = data1)
summary(result_transformation)
mdl_transformation <- lm(sqrt(Harvest) ~ sqrt(`Active Hunters`) + sqrt(`Active Hunters`):WMU -1, data = data1)
plot(mdl_transformation)
