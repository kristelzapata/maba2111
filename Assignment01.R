# Kristel Joyce Zapata
# 184138

# Assignment01 answers

# 1. WHO dataset
# a. load and run Session1.R

WHO <- read.csv("WHO.csv")
WHO

# b. country with the biggest population

WHO$Country[which.max(WHO$Population)]

# c. population of Malaysia

malaysia <- subset(WHO, Country == "Malaysia")
malaysia$Population

# d. country with the lowest literacy

min.LitRate <- WHO$Country[which.min(WHO$LiteracyRate)]
print(min.LitRate)

# e. Richest country in Europe based on GNI

WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe
Max.Europe.GNI <- WHO.Europe$Country[which.max(WHO.Europe$GNI)]
print(Max.Europe.GNI)

# f. Mean Life expectancy of countries in Africa

WHO.Africa = subset(WHO, Region == "Africa")
WHO.Africa
mean(WHO.Africa$LifeExpectancy)

# g. Number of countries with population greater than 10,000

Over.10M <- subset(WHO, Population > 10000)
Over.10M
dim(Over.10M)[1]

# h. Top 5 countries in the Americas with the highest child mortality

WHO.Americas <- subset(WHO, Region == "Americas")
WHO.Americas
index.americas <- order(WHO.Americas$ChildMortality, decreasing = TRUE)
Americas.ChildMort <- WHO.Americas[index.americas,]
Top.Americas.ChildMort <- head(Americas.ChildMort,5)
Top.Americas.ChildMort$Country

# 2. NBA dataset (Historical NBA Performance.xlsx) answers - I have saved file first as csv.
# a. The year Bulls has the highest winning percentage

NBA <- read.csv("Historical NBA Performance.csv")
NBA.Bulls = subset(NBA, Team == "Bulls")
NBA.Bulls
WinningPercentageMax <- which.max(NBA.Bulls$Winning.Percentage)
WinningPercentageMax
NBA.Bulls$Year[WinningPercentageMax]

# b. Teams with an even win-loss record in a year

NBA.evenWL <- subset(NBA, Winning.Percentage == "0.5")
NBA.evenWL

# 3. Seasons_Stats.csv answers

# a. bonus
# b. bonus

# c. What year/season does Lebron James scored the highest?

season <- read.csv("Seasons_Stats.csv")
Lebron <- subset(season, Player == "LeBron James")
Lebron.Max <- which.max(Lebron$PTS)
Lebron.Max
Lebron$Year[Lebron.Max]

# d. What year/season does Michael Jordan scored the highest?

Jordan <- subset(season, Player == "Michael Jordan*")
Jordan.Max <- which.max(Jordan$PTS)
Jordan.Max
Jordan$Year[Jordan.Max]

# e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?

Kobe <- subset(season, Player == "Kobe Bryant")
Kobe.MP.Min <- which.min(Kobe$MP)
Kobe.MP.Min
Kobe$PER[Kobe.MP.Min]

# 4. National Universities Rankings.csv

NUR <- read.csv("National Universities Rankings.csv")
NUR

# a. University with the most number of undergrads

NUR$Undergrad.Enrollment <- gsub(pattern = ",",replacement = "", NUR$Undergrad.Enrollment)
NUR$Undergrad.Enrollment
Max.Undergrad <- NUR$Name[which.max(NUR$Undergrad.Enrollment)]
Max.Undergrad

# b. Average Tuition in the Top 10 University

top.10 <- NUR[order(NUR$Rank),][1:10,]
top.10$Tuition.and.fees <- gsub(pattern = "\\$|\\,",replacement = "", top.10$Tuition.and.fees)
top.10$Tuition.and.fees
mean(as.numeric(top.10$Tuition.and.fees))

