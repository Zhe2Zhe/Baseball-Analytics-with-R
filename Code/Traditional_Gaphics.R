# install.packages("graphics")
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
setwd("~/Desktop/Projects/Baseball/data")
hof <- read.csv("hofbatting.csv")


# Create a new factor "era" of baseball: 
# "19th century": up to the 1900 season
# "Dead Ball": 1901 - 1919
# "Lively Ball": 1920 to 1941
# "Integration": 1942 to 1960
# "Expansion": 1961 to 1976
# "Free Agency": 1977 to 1993
# "Long Ball" after 1993
head(hof)
# Create MidCareer
hof$MidCareer <- with(hof, (From + To) / 2)
# Creat Era
hof$Era <- cut(hof$MidCareer,
               breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
               labels = c("19th Century", "Lively Ball", "Dead Ball",
                          "Integration", "Expansion", "Free Agency", 
                          "Long Ball"))
head(hof)
T.Era <- table(hof$Era)
T.Era

barplot(T.Era)
# or add axis
barplot(table(hof$Era), xlab="Era", ylab="Frequency", 
        main="Era of the Nonpitching Hall of Famers")
# bar chart or pie chart
plot(table(hof$Era))
pie(table(hof$Era))

# Saving Graphs use png function

#or png("bargraph.png") in the current folder

# "../" the topper folder




# Dot plots

T.Era <- table(hof$Era)
dotchart(as.numeric(T.Era), labels=names(T.Era), xlab="Frequency")

hof.500 <- subset(hof, HR >= 500)
hof.500 <- hof.500[order(hof.500$OPS), ]
dotchart(hof.500$OPS, labels=hof.500$X, xlab="OPS")

# Numeric Variable: Scatterplot, Stripchart (one-dimensional scatterplot) and Histogram

#windows(width=7, height=3.5)


## Scatter plot

plot(hof$MidCareer,main="Scatterplot of MidCareer")

# "method = jitter" means points are randomly placed in a band over their value
#               this method is helpful when you have multiple plotting symbol 
stripchart(hof$MidCareer, xlab="Mid Career", main = "Stripchart of MidCareer")
stripchart(hof$MidCareer, method="jitter", pch=1, 
           xlab="Mid Career","Stripchart of MidCareer")

hist(hof$MidCareer, xlab="Mid Career", main="")
d <- density(hof$MidCareer)
plot(d)



# customize the break
hist(hof$MidCareer, xlab="Mid Career", main="",
     breaks=seq(1880, 2000, by=20))

# Two Numeric Variables

with(hof, plot(MidCareer, OPS,main="OPS vs. MidCareer"))
with(hof, lines(MidCareer, OPS))
# get a smoothing curve to show the general association pattern
with(hof, plot(MidCareer, OPS,main="Scatterplot of OPS vs. MidCareer"))
with(hof, lines(lowess(MidCareer, OPS, f=0.3)))

# use identify to find the extreme value, n= number of points we want to identify
with(hof, identify(MidCareer, OPS, X, n=4))
#... identify points on the plot by mouse-clicking 
#... then press ESC

# or 

plot(hof$MidCareer, hof$OPS,xlab="MidCareer", ylab="OPS",main="OPS vs MidCareer")
lines(lowess(hof$MidCareer, hof$OPS, f=0.3))
identify(hof$MidCareer, hof$OPS, hof$X, n=4)


with(hof, plot(OBP, SLG))

with(hof, plot(OBP, SLG, xlim=c(0.25, 0.50), 
               ylim=c(0.28, 0.75), pch=19,
               xlab="On Base Percentage",
               ylab="Slugging Percentage"))


#... identify points on the plot by mouse-clicking 
#... then press ESC


# A Numeric Variable and a Factor Variable

# calculate home run rate
hof$HR.Rate <- with(hof, HR / AB)

stripchart(HR.Rate ~ Era, data=hof, main="Era vs. Home Run Rate ")

#par(plt = c(.2,.94,.145,.883))
par()
boxplot(HR.Rate ~ Era, data=hof, 
        horizontal=TRUE, xlab="HR Rate", main = "Era vs. Home Run Rate")


# Save the graph
png("../output/bargraph.png")
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=TRUE, xlab="HR Rate")
dev.off()

pdf("../output/graphs.pdf")
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=TRUE, xlab="HR Rate")
dev.off()

