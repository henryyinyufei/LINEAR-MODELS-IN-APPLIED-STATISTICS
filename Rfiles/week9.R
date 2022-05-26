xy=read.csv("table8.1.csv")

x=cbind(xy$x1,xy$x2)
y=xy$y

# Linear model (note, x2 is a quantitative variable)
tool.lm=lm(y~x[,1]*x[,2])
summary(tool.lm)


#model with just x1
tool.x1.lm= lm(y~x[,1])

#interaction model
tool.interaction.lm=lm(y~x[,1]*x[,2])

#compare models to see if tool type is in the model (H0:B2=B3=0)
print(anova(tool.x1.lm,tool.interaction.lm))

#See how they are significant... first test for interact, then different slope
anova(tool.interaction.lm)

