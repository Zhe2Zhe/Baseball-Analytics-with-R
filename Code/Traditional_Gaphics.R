install.packages("graphics")
library("graphics")

# Read the data frame
##############################################
# Traditional Graphics
# Needs package "graphics"
# Needs datafiles hofbatting.csv, all1998.csv,
#                 fields.csv, retrosheetIDs.csv
#   plus .csv files from the Lahman's database
#         (placed in the "lahman" subfolder)
# 
##############################################

# Section 3.1 Introduction
# change the path
setwd("~/Desktop/Projects/Baseball/data")
hof <- read.csv("hofbatting.csv")

head(hof)
# Create a new factor "era" of baseball: 
# "19th century": up to the 1900 season
# "Dead Ball": 1901 - 1919
# "Lively Ball": 1920 to 1941
# "Integration": 1942 to 1960
# "Expansion": 1961 to 1976
# "Free Agency": 1977 to 1993
# "Long Ball" after 1993

# Create MidCareer
hof$MidCareer <- with(hof, (From + To) / 2)
# Creat Era
hof$Era <- cut(hof$MidCareer,
               breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
               labels = c("19th Century", "Lively Ball", "Dead Ball",
                          "Integration", "Expansion", "Free Agency", 
                          "Long Ball"))
head(hof)
str(hof)

table(hof$Era)
T.Era <- table(hof$Era)

barplot(T.Era)
# or add axis
barplot(T.Era, xlab="Era", ylab="Frequency", 
        main="Era of the Nonpitching Hall of Famers")
# bar chart or pie chart
plot(T.Era)
pie(T.Era)


dotchart(as.numeric(T.Era), labels=names(T.Era), xlab="Frequency",main="Dotplot of Era of the Nonpitching Hall of Famers")

# HR: home run
# OPS: On-base plus slugging (OPS) 
# is a sabermetric baseball statistic calculated as 
# the sum of a player's on-base percentage and slugging average.

# Only focus on those whose hr>=500
hof.500 <- subset(hof, HR >= 500)

# OPS in increasing order
hof.500 <- hof.500[order(hof.500$OPS), ]
dotchart(hof.500$OPS, labels=hof.500$X, xlab="OPS")

####################################
# Numeric Variable: Scatterplot, 
# Stripchart (one-dimensional scatterplot) and 
# Histogram
####################################



## Scatter plot



plot(hof$MidCareer,main="Scatterplot of MidCareer")

# "method = jitter" means points are randomly placed in a band over their value
#               this method is helpful when you have multiple plotting symbol 

par(mfrow=c(1,2))
stripchart(hof$MidCareer, xlab="Mid Career", main = "Stripchart of MidCareer")

stripchart(hof$MidCareer, method="jitter", pch=1, 
           xlab="Mid Career", main = "Stripchart of MidCareer")
par(mfrow=c(1,1))

hist(hof$MidCareer, xlab="Mid Career", main="Histogram of MidCareer")


# customize the break
hist(hof$MidCareer, xlab="Mid Career",main="Histogram of MidCareer",
     breaks=seq(1880, 2000, by=10))

hist(hof$MidCareer, xlab="Mid Career", main="Histogram of MidCareer",
     breaks=seq(1880, 2000, by=20))

# Density Curve

d <- density(hof$MidCareer)
plot(d,main = "Density plot of MidCareer")


# Two Numeric Variables

with(hof, plot(MidCareer, OPS,main="OPS vs. MidCareer"))
with(hof, lines(MidCareer, OPS))

# get a smoothing curve to show the general association pattern
with(hof, plot(MidCareer, OPS,main="Scatterplot of OPS vs. MidCareer"))
with(hof, lines(lowess(MidCareer, OPS, f=0.3)))

# f is the smoother span. T
with(hof, lines(lowess(MidCareer, OPS, f=1)))
with(hof, lines(lowess(MidCareer, OPS, f=0.1)))


# use identify to find the extreme value, n= number of points we want to identify
with(hof, identify(MidCareer, OPS, X, n=4))
#... identify points on the plot by mouse-clicking 
#... then press ESC

# or 

plot(hof$MidCareer, hof$OPS,xlab="MidCareer", ylab="OPS",main="OPS vs MidCareer")
lines(lowess(hof$MidCareer, hof$OPS, f=0.3))
identify(hof$MidCareer, hof$OPS, hof$X, n=4)


# generate a plot


with(hof, plot(OBP, SLG))

# Change the range, label and size of points

with(hof, plot(OBP, SLG, xlim=c(0.25, 0.50), 
               ylim=c(0.28, 0.75), pch=19,
               xlab="On Base Percentage",
               ylab="Slugging Percentage"))

# OPS = OBP + SLG

curve(.7 - x, add = T)
curve(.8 - x, add = T)
curve(.9 - x, add = T)
curve(1 - x, add = T)

text(.27,.42, "OPS=0.7")
text(.27,.52, "OPS = 0.8")
text(.27,.62,"OPS=0.9")
text(.27,.72,"OPS = 1.0")






# A Numeric Variable and a Factor Variable

# calculate home run rate
hof$HR.Rate <- with(hof, HR / AB)

stripchart(HR.Rate ~ Era, data=hof, main="Era vs. Home Run Rate ")


# Side-by-side box plot
par()
boxplot(HR.Rate ~ Era, data=hof, 
        horizontal=FALSE, xlab="HR Rate", main = "Era vs. Home Run Rate")


# Save the graph
png("../output/bargraph.png")
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=FALSE, xlab="HR Rate")
dev.off()

pdf("../output/graphs.pdf")
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=FALSE, xlab="HR Rate")
dev.off()

