example8.1 <- read.csv("D:/Simon Fraser University/2020 fall/STAT 350/Rfile/indicator variable example 8.1.csv")
attach(example8.1)

tool.x1.lm <- lm(y ~ x1)

tool.interaction.lm <- lm(y ~ x1 * x2) # same as lm(y ~ x1 + x2 + x1:x2)

# anova(reduced model, full model)
print(anova(tool.x1.lm,tool.interaction.lm))
# anova(full model)
anova(tool.interaction.lm)
