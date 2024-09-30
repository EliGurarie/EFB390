SeaOtters <- read.csv("SeaOtters.csv")
SeaLions <- read.csv("SeaLions.csv")

head(SeaOtters)
head(SeaLions)

Length <- SeaLions$Length
Weight <- SeaLions$Weight
plot(Length, Weight)

plot(Weight ~ Length, data = SeaLions, col = "purple")

library(ggplot2)

require(ggplot2)
ggplot(SeaLions, aes(x = Length, y = Weight)) + geom_point()

ggplot(SeaLions, aes(x = Length, y = Weight, color = Sex)) + geom_point()
ggplot(SeaLions, aes(x = Length, y = Weight, color = Island)) + geom_point()

plot(Weight ~ Length, data = SeaLions)
lm(Weight ~ Length, data = SeaLions)

sealion.fit <- lm(Weight ~ Length, data = SeaLions)
summary(sealion.fit)

(CI.low <- 0.84675 - 2*0.02024)

(CI.high <- 0.84675 + 2*0.02024)

summary(sealion.fit)$coef[2,1] + c(-2,2) * summary(sealion.fit)$coef[2,2]

plot(Weight ~ Length, data = SeaLions)
abline(sealion.fit, col = "red")

plot(Weight ~ Length, data = SeaLions, pch = 19, col = rgb(0,0,0,.2))
abline(sealion.fit, col = "red", lwd = 2)

SeaLion.males <- subset(SeaLions, Sex == "M")
SeaLion.females <- subset(SeaLions, Sex == "F")

## Males ----
plot(Weight ~ Length, data = SeaLion.males)
lm(Weight ~ Length, data = SeaLion.males)

male.fit <- lm(Weight ~ Length, data = SeaLion.males)
summary(male.fit)

summary(male.fit)$coef[2,1] + c(-2,2) * summary(male.fit)$coef[2,2]

plot(Weight ~ Length, data = SeaLion.males)
abline(sealion.fit, col = "orange")

plot(Weight ~ Length, data = SeaLion.males, pch = 19, col = rgb(0,0,0,.2))
abline(sealion.fit, col = "orange", lwd = 2)

## Females ----
plot(Weight ~ Length, data = SeaLion.females)
lm(Weight ~ Length, data = SeaLion.females)

female.fit <- lm(Weight ~ Length, data = SeaLion.females)
summary(female.fit)

summary(female.fit)$coef[2,1] + c(-2,2) * summary(female.fit)$coef[2,2]

plot(Weight ~ Length, data = SeaLion.females)
abline(sealion.fit, col = "purple")

plot(Weight ~ Length, data = SeaLion.females, pch = 19, col = rgb(0,0,0,.2))
abline(sealion.fit, col = "purple", lwd = 2)

# Part 2 ----

plot(count ~ year, data = SeaOtters)
plot(count ~ year, data = SeaOtters, log = "y")

otter.fit <- lm(count ~ year, data = SeaOtters)
summary(otter.fit)
plot(otter.fit)

plot(count ~ year, data = SeaOtters)
abline(otter.fit)

logotter.fit <- lm(log(count) ~ year, data = SeaOtters)
summary(logotter.fit)

plot(log(count) ~ year, data = SeaOtters)
abline(logotter.fit)

# population growth w CIs
r.hat <- summary(logotter.fit)$coefficients[2,1]
r.sd <- summary(logotter.fit)$coefficients[2,2]
exp(c(r.hat, r.hat-2*r.sd, r.hat+2*r.sd))

plot(log(count) ~ year, main = 'Sea Otter Population', 
     col = 'brown', pch = 4, data = SeaOtters, lwd = 2)
abline(logotter.fit, col = "blue", lwd = 2)

SeaOtters.bc <- read.csv("BC_SeaOtters.csv")
head(SeaOtters.bc)

plot(Count ~ Year, data = SeaOtters.bc)
SeaOtters.bc

logbc.fit <- lm(log(Count) ~ Year, data = SeaOtters.bc)
summary(logbc.fit)

# population growth w CIs
r.hat <- summary(logbc.fit)$coefficients[2,1]
r.sd <- summary(logbc.fit)$coefficients[2,2]
exp(c(r.hat, r.hat-2*r.sd, r.hat+2*r.sd))

ggplot(SeaOtters.bc, aes(Year, Count)) + geom_point() + 
  stat_smooth(method = "glm", 
              method.args = list(family = gaussian(link = "log")))
