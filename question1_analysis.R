#First I want to read in the data

#Next I will install the following packages
install.packages("plyr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(plyr)
netflix_data = read.csv("/Users/mielafoster/Desktop/netflixUS_final.csv")
View(netflix_data)

#create the variables that I care about in the diversity data
gender = netflix_data$gender
race = netflix_data$Race
type = netflix_data$type
release_yr = netflix_data$release_year
date_added = netflix_data$date_added

#I want to now convert the non-numeric data to numeric data for race and gender

netflix_data$gender_num <-ifelse(netflix_data$gender=="Female", 1,
                                   ifelse(netflix_data$gender=="Male", 0,
                                   ifelse(netflix_data$gender=="Male Female", 2, 3)))
View(netflix_data)

netflix_data$race_num <-ifelse(netflix_data$Race=="Black", 1,
                             ifelse(netflix_data$Race=="White", 0,
                             ifelse(netflix_data$Race=="Asian", 2, 
                             ifelse(netflix_data$Race=="Latino", 3,
                             ifelse(netflix_data$Race=="Middle East", 4,
                             ifelse (netflix_data$Race=="Ethnic & White", 5, 6))))))
View(netflix_data)

netflix_data$type_num <-ifelse(netflix_data$type=="Movie", 1,
                                 ifelse(netflix_data$type=="TV Show", 2, 0))
View(netflix_data)

#Now I want to export this to use for the question 2 stuff.
num_netflix_1 = write.csv( netflix_data,"/Users/mielafoster/Desktop/netflix.csv", row.names = FALSE)


#Now I will subset by year!

sub_2020 <- netflix_data[netflix_data$date_added == 2020,]
View(sub_2020)

sub_2019 <- netflix_data[netflix_data$date_added == 2019,]
View(sub_2019)

sub_2018 <- netflix_data[netflix_data$date_added == 2018,]
View(sub_2018)

sub_2017 <- netflix_data[netflix_data$date_added == 2017,]
View(sub_2017)

sub_2016 <- netflix_data[netflix_data$date_added == 2016,]
View(sub_2016)

sub_2015 <- netflix_data[netflix_data$date_added == 2015,]
View(sub_2015)

sub_2014 <- netflix_data[netflix_data$date_added == 2014,]
View(sub_2014)

sub_2013 <- netflix_data[netflix_data$date_added == 2013,]
View(sub_2013)

sub_2012 <- netflix_data[netflix_data$date_added == 2012,]
View(sub_2012)

sub_2011 <- netflix_data[netflix_data$date_added == 2011,]
View(sub_2011)

sub_2010 <- netflix_data[netflix_data$date_added == 2010,]
View(sub_2010)

#I want to first subset the data by each ethic group and compare to white directors, and investigate if diversity real
sub_black <- netflix_data[netflix_data$race_num == 1,]
View(sub_black)

sub_latino <- netflix_data[netflix_data$race_num == 3,]
View(sub_latino)

sub_asian <- netflix_data[netflix_data$race_num == 2,]
View(sub_asian)

sub_white <- netflix_data[netflix_data$race_num == 0,]
View(sub_white)

sub_middleeast <- netflix_data[netflix_data$race_num == 4,]
View(sub_middleeast)

sub_ethnic_mix <- netflix_data[netflix_data$race_num == 5,]
View(sub_ethnic_mix)

#Now we are going to observe the longitudinal data for each ethnic type of person
#Recall the color code as we add histograms!
hist(sub_white$date_added, col = "lightblue", breaks = 5, main = "Frequency of White Directors over Time", xlab = "Time")
hist(sub_ethnic_mix$date_added, col = "brown", breaks = 5, main = "Frequency of Ethnic & White Directors over Time", xlab = "Time")
hist(sub_latino$date_added, col = "red", breaks = 1,  add = T) 
hist(sub_asian$date_added, col = "gold", breaks = 6 , add = T)
hist(sub_middleeast$date_added, col = "blue", breaks = 10, add = T)
hist(sub_black$date_added, col = "green", breaks = 1, add = T)

#bar proportion plots of race
ggplot(netflix_data ) +
  aes(x = date_added, fill = factor(race)) +
  geom_bar(position = "fill")

#bar proportion plots by gender
ggplot(netflix_data) +
  aes(x = date_added, fill = factor(gender)) +
  geom_bar(position = "fill")

#Now we're going to subset the data by each gender ethnic group, or the ones that are most important!
sub_white_female <- netflix_data[netflix_data$gender_num == 1 & netflix_data$race_num == 0, ]
View(sub_white_female)

sub_black_female <- netflix_data[netflix_data$gender_num == 1 & netflix_data$race_num == 1, ]
View(sub_black_female)

sub_black_male <- netflix_data[netflix_data$gender_num == 0 & netflix_data$race_num == 1, ]
View(sub_black_male)

sub_white_male <- netflix_data[netflix_data$gender_num == 0 & netflix_data$race_num == 0, ]
View(sub_white_male)

sub_asian_male <- netflix_data[netflix_data$gender_num == 0 & netflix_data$race_num == 2, ]
View(sub_asian_male)

#Now we are going to observe the longitudinal data for each type of person
#Recall the color code as we add histograms!
hist(sub_white_female$date_added, col = "lightblue", breaks = 5, main = "Frequency of Ethnic-Gender Groups over Time", xlab = "Time")
hist(sub_black_female$date_added, col = "green", breaks = 1, add = T)
hist(sub_black_male$date_added, col = "red", breaks = 1,  add = T) 
hist(sub_asian_male$date_added, col = "gold", breaks = 6 , main = "Frequency of Ethnic-Gender Groups over Time", xlim = "Time", add = T)
hist(sub_white_male$date_added, col = "blue", breaks = 10, main = "Frequency of Ethnic-Gender Groups over Time", add = T)





hist(sub_2020$race_num)
hist(sub_2019$race_num)
hist(sub_2018$race_num)
hist(sub_2017$race_num)
hist(sub_2016$race_num)
hist(sub_2015$race_num)
hist(sub_2014$race_num)
hist(sub_2013$race_num)
hist(sub_2012$race_num)
hist(sub_2011$race_num)

hist(sub_2020$gender_num)
hist(sub_2019$gender_num)
hist(sub_2018$gender_num)
hist(sub_2017$gender_num)
hist(sub_2016$gender_num)
hist(sub_2015$gender_num)
hist(sub_2014$gender_num)
hist(sub_2013$gender_num)
hist(sub_2012$gender_num)
hist(sub_2011$gender_num)

ggplot(sub_2020, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2019, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2018, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2017, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2016, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2015, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2014, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2013, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2012, aes(race)) +
  geom_bar(fill = "#0073C2FF")

ggplot(sub_2011, aes(race)) +
  geom_bar(fill = "#0073C2FF")










