# Step 1: Install the packages ----

install.packages("Lahman")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")

library(Lahman)
library(dplyr)
library(ggplot2)
library(GGally)


# Step 2: Load the data ----

LahmanData

Teams


###  https://rdrr.io/cran/Lahman/man/Teams.html 


View(Teams)

str(Teams)
dim(Teams)
head(Teams)
tail(Teams)

# Step 3: Data Wrangling ----

# package "dplyr": pipe operator %>%

a <- c(1,2,3)

mean(a)

log(sqrt(mean(a)))

a %>% 
  mean() %>%
  sqrt() %>%
  log()

# select
# filter
# arrange
# mutate
# summarize
# group_by







Teams %>%
  select(yearID, lgID,  W, L, R, RA) %>%
  filter(yearID == 2014) %>%
  mutate(wpct = R^1.83 / (R^1.83 + RA^1.83), 
         expwin = round(wpct * (W+L)), diff = W - expwin,RD = R-RA) %>%
  arrange(lgID) %>%
  group_by(lgID) %>%
  summarize(mean.wpct = mean(wpct)) 


# Create new variable wpct, RD

mydata <- Teams %>%
  select(yearID, lgID, teamID, W, L, R, RA) %>%
  filter(yearID == 2014) %>%
  mutate(Wpct = W/(W +L),  RD = R - RA)

head(mydata)

mydata


# Step 4: Data Visualization ----

# histogram : show the distribution

hist(mydata$Wpct)


ggplot(mydata) + 
  geom_histogram(aes(x = Wpct), binwidth = 5, 
                 color = "white", fill = "grey") +
  ggtitle("Histogram") + xlab("Win") + ylab("Frequency")

# use different color for different league


ggplot(mydata) + 
  geom_histogram(aes(x = Wpct, fill = lgID), binwidth = 5, 
                 color = "white" , position = "identity", alpha = 0.5) +
  ggtitle("Histogram") + xlab("Win") + ylab("Frequency")


ggplot(mydata) + 
  geom_histogram(aes(x = Wpct, fill = lgID), binwidth = 5, 
                 color = "white" ) +
  ggtitle("Histogram") + xlab("Win") + ylab("Frequency") +
  facet_wrap(~lgID)


# Scatter Plot


plot(mydata$RD,mydata$Wpct)

ggplot(mydata) + geom_point( aes(x = RD, y = Wpct, color = lgID)) + 
  ggtitle("Histogram of RD vs. Wpct") + xlab("") + ylab("")


ggplot(mydata) + geom_point( aes(x = RD, y = Wpct, color = lgID)) + 
  ggtitle("Histogram of RD vs. Wpct") + xlab("Run Differential") + ylab("Win Percentage") +
  facet_wrap(~lgID)


## Step 5 : Linear Regression



mod1 <- lm(Wpct ~ RD, data = mydata)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)
par(mfrow = c(1,1))



ggplot(mydata, aes(x = RD, y = Wpct)) + 
  geom_point() + stat_smooth(method = "lm")


## Pythagorean Formula by Bill James


mydata <- mydata %>%
  mutate(Wpct_p = R^2/(R^2 + RA^2), expwin = Wpct_p * (W + L))
         
         
plot(mydata$expwin,mydata$W)


identify(mydata$expwin,mydata$W,mydata$teamID,n=1)

## Compare the Mean Square Error(MSE)

## residuals
res1 <- mod1$residuals
res2 <- mydata$Wpct-mydata$Wpct_p

par(mfrow = c(1,2))
plot(res1,main = "Residuals: Linear Regression")
plot(res2, main = "Residuals: Pythagorean Formula")
par(mfrow =c(1,1))

MSE1 <- mean(res1^2)
MSE2 <- mean(res2^2)
