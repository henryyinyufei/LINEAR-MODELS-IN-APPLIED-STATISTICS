library(latticeExtra)
library(tidyverse) 

# Download the csv file AND change the data format of the year
data_download <- read_csv("https://files.ontario.ca/opendata/black_bear_2019.csv")


total <- data_download %>% rename_all(make.names)%>% filter(WMU =="Total")
data1 <- data_download %>% rename_all(make.names)%>% filter(WMU !="Total")

length(unique(data1$WMU))

ggplot(data1, aes(x = Active.Hunters, y = Harvest))+
  geom_point(show.legend = FALSE)+
  ggtitle("Hunters and Harvest in different WMUs")

ggplot(data1, aes(x = Active.Hunters, y = Harvest, colour = WMU))+
  geom_point(show.legend = FALSE)+
  ggtitle("Hunters and Harvest in different WMUs")

ggplot(data1, aes(x = sqrt(Active.Hunters), y = sqrt(Harvest), colour = WMU))+
  geom_point(show.legend = FALSE)+
  ggtitle("Hunters and Harvest in different WMUs")

result <- aov(Harvest~Active.Hunters+Active.Hunters:WMU-1,data = data1)
print(summary(result))

result <- lm(Harvest~Active.Hunters+Active.Hunters:WMU-1,data = data1)
print(summary(result))
plot(result)


ggplot(data1, aes(x = sqrt(Active.Hunters), y = sqrt(Harvest), colour = WMU))+
  geom_point(show.legend = FALSE)+
  ggtitle("Hunters and Harvest in different WMUs")

result <- aov(sqrt(Harvest)~sqrt(Active.Hunters)+sqrt(Active.Hunters):WMU-1,data = data1)
print(summary(result))
plot(result)



# some junk code
plot(data1$Active.Hunters,data1$Harvest)

for(i in 2:length(result$coefficients)){
  sl=result$coefficients[1]+result$coefficients[i]
  abline(c(0,))
}



library("lattice")
xyplot(data1$Harvest ~ data1$Active.Hunters | WMU, data = data1,
       panel = function(x, y, ...)
       {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       }
)
