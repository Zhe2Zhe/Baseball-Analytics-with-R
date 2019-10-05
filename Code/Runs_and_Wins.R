#################################################
# The Relation Between Runs and Wins
# 
# Needs teams.csv files from the Lahman's database

#
#################################################


library(ggplot2)
library(dplyr)


setwd("~/Desktop/Projects/Baseball/Data")
teams <- read.csv("teams.csv")
tail(teams)

# the description of every column is provided in 
# the "readme" file accompanying the Lahman's database


# Select a subset of the data

myteams <- subset(teams, yearID > 2000)[ , c("teamID", "yearID",
                                             "lgID", "G", "W", "L", "R", "RA")]


# yearID         Year
# lgID           League
# teamID         Team
# G              Games played
# W              Wins
# L              Losses
# R              Runs scored
# RA             Opponents runs scored

tail(myteams)

myteams$RD <- with(myteams, R - RA)  # Run Differential
myteams$Wpct <- with(myteams, W / (W + L))  # Winning Percentage

plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab="winning percentage")

x <- myteams$RD
y <- myteams$Wpct

# polynomial
plot(x,y^2)

# Linear (negative)
plot(x,-y)

# reciprocal (negative) 
plot(x,1/y)

# No clear relation
plot(rnorm(100))


plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab="winning percentage")
# Simple Linear Regression
# Use Run Differential to predict winning Percentage

# Check normality
par(mfrow=c(1,2))
qqnorm(myteams$Wpct, main = "Winning Percentage")
qqnorm(myteams$RD, main = "Run Differential")

linfit <- lm(Wpct ~ RD, data=myteams)

summary(linfit)
anova(linfit)

# Add regression line on the graph

par(mfrow=c(1,1))
plot(myteams$RD, myteams$Wpct,
     xlab="run differential",
     ylab="winning percentage")
abline(a=coef(linfit)[1], b=coef(linfit)[2], lwd=2)

# Predict the winning percentage

myteams$linWpct <- predict(linfit)

# Predict the wins
myteams$linW <- with(myteams, round((W+L)* linWpct))

myteams$linResiduals <- residuals(linfit)

# Residual plot
plot(myteams$RD, myteams$linResiduals,
     xlab="run differential",
     ylab="residual",main="Residuals vs. Predicted")
abline(h=0, lty=3)
identify(myteams$RD, myteams$linResiduals,myteams$teamID,n=2)

# or

points(c(68, 88), c(.0749, -.0733), pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)


# generate all diagnostic graphs at once
par(mfrow=c(2,2))
plot(linfit)





#  The Pythagorean Formula for Winning Percentage

# k=1
myteams$pytW_1 <- with(myteams, round(R/(R+RA)*(W + L)))
# Nominal residuals
myteams$pytResiduals_1 <- myteams$W - myteams$pytW_1


# k=2
myteams$pytW_2 <- with(myteams, round(R ^ 2 / (R ^ 2 + RA ^ 2)* (W+L)))

# Nominal residuals
myteams$pytResiduals_2 <- myteams$W - myteams$pytW_2

dev.off()
ggplot(myteams, aes(myteams$W, myteams$pytW_1)) + geom_point() + stat_smooth(method = "lm")

ggplot(myteams, aes(myteams$W, myteams$pytW_2)) + geom_point() + stat_smooth(method = "lm")


# it turns out 1.83 gives the best fit 

myteams$pytW_3 <- with(myteams, round(R ^ 1.83 / (R ^ 1.83 + RA ^ 1.83) * (W+L)))
myteams$pytResiduals_3 <- myteams$W - myteams$pytW_3

ggplot(myteams, aes(myteams$W, myteams$pytW_3)) + geom_point() + stat_smooth(method = "lm")


# It's hard to compare the percentage
# Only look at the year 2012

data2011 <-subset(myteams, myteams$yearID==2011)

data2011[,c("teamID","G","W","linW","pytW_1","pytW_2","pytW_3")]
sqrt(mean(myteams$pytResiduals ^ 2))

#  The Exponent in the Pythagorean Formula

# myteams$logWratio <- log(myteams$W / myteams$L)
# myteams$logRratio <- log(myteams$R / myteams$RA)
# pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams)
# pytFit




############################################


#  Good and Bad Predictions by the Pythagorean Formula

gl2011 <- read.table("gl2011.txt", sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)
BOS2011 <- subset(gl2011, HomeTeam=="BOS" | VisitingTeam=="BOS")[
  , c("VisitingTeam", "HomeTeam", "VisitorRunsScored",
      "HomeRunsScore")]
head(BOS2011)

BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam == "BOS",
                                          HomeRunsScore - VisitorRunsScored,
                                          VisitorRunsScored - HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0

aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)

results <- gl2011[,c("VisitingTeam", "HomeTeam",
                     "VisitorRunsScored", "HomeRunsScore")]
results$winner <- ifelse(results$HomeRunsScore >
                           results$VisitorRunsScored, as.character(results$HomeTeam),
                         as.character(results$VisitingTeam))
results$diff <- abs(results$VisitorRunsScored -
                      results$HomeRunsScore)

onerungames <- subset(results, diff == 1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

teams2011 <- subset(myteams, yearID == 2011)
teams2011[teams2011$teamID == "LAA", "teamID"] <- "ANA"
teams2011 <- merge(teams2011, onerunwins)
plot(teams2011$onerunW, teams2011$pytResiduals,
     xlab="one run wins",
     ylab="Pythagorean residuals")

identify(teams2011$onerunW, teams2011$pytResiduals,
         labels=teams2011$teamID)
#...identify data points by mouse-clickin on the plot
#...then press ESC to finish

pit <- read.csv("pitching.csv")
top_closers <- subset(pit, GF > 50 & ERA < 2.5)[ ,c("playerID",
                                                    "yearID", "teamID")]

teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)

# Section 4.7  How Many Runs for a Win?

D(expression(G * R ^ 2 / (R ^ 2 + RA ^ 2)), "R")

IR <- function(RS=5, RA=5){
  round((RS ^ 2 + RA ^ 2)^2 / (2 * RS * RA ^ 2), 1)
}

IRtable <- expand.grid(RS=seq(3, 6, .5), RA=seq(3, 6, .5))
rbind(head(IRtable), tail(IRtable))

IRtable$IRW <- IR(IRtable$RS, IRtable$RA)
xtabs(IRW ~ RS + RA, data=IRtable)
