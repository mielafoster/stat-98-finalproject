#Now this part of my r-code will explore correlations between predictors for outcomes
install.packages("corrplot")
install.packages("displayr")
install.packages("Hmisc")
library("Hmisc")
library(corrplot)


#First let's import my dataset
netflix_data_2 = read.csv("/Users/mielafoster/Desktop/netflix.csv")
View(netflix_data_2)

#Now in order to do our correlation matrix we have to remove the nonnumeric data
netflix_num = subset(netflix_data_2, select = -c(show_id,type, director, cast, country, rating, duration, listed_in, title, description, gender,Race) )
View(netflix_num)


#Now we're going to make our first correlation matrix
netflix.cor = cor(netflix_num, use = "pairwise.complete.obs")
print(netflix.cor)
corrplot(netflix.cor)

#Let's see if we can extract correlation between each racial group and the other predictors we have


sub_blacknum <- netflix_num[netflix_num$race_num == 1,]
View(sub_blacknum)

sub_latinonum <- netflix_num[netflix_num$race_num == 3,]
View(sub_latinonum)

sub_asiannum<- netflix_num[netflix_num$race_num == 2,]
View(sub_whitenum)

sub_whitenum <- netflix_num[netflix_num$race_num == 0,]
View(sub_whitenum)

sub_middleeastnum <- netflix_num[netflix_num$race_num == 4,]
View(sub_middleeastnum)

sub_ethnic_mixnum <- netflix_num[netflix_num$race_num == 5,]
View(sub_ethnic_mixnum)

#Now we need to find each correlation matrix for each racial group
black.cor = cor(sub_blacknum, use = "pairwise.complete.obs")
print(black.cor)
corrplot(black.cor)

latino.cor = cor(sub_latinonum, use = "pairwise.complete.obs")
print(latino.cor)
corrplot(latino.cor)

asian.cor = cor(sub_whitenum, use = "pairwise.complete.obs")
print(asian.cor)
corrplot(asian.cor)

white.cor = cor(sub_whitenum, use = "pairwise.complete.obs")
print(white.cor)
corrplot(white.cor)

middle.cor = cor(sub_middleeastnum, use = "pairwise.complete.obs")
print(middle.cor)
corrplot(middle.cor)

ethnicmix.cor = cor(sub_ethnic_mixnum, use = "pairwise.complete.obs")
print(ethnicmix.cor)
corrplot(ethnicmix.cor)

#Now I want to subset my data for gender
sub_femalenum <- netflix_num[netflix_num$gender_num == 1,]
sub_malenum <- netflix_num[netflix_num$gender_num == 0,]
sub_mix_gen_num <- netflix_num[netflix_num$gender_num == 2,]
View(sub_mix_gen_num)

#Now create Correlation matricies 
female.cor = cor(sub_femalenum, use = "pairwise.complete.obs")
print(female.cor)

male.cor = cor(sub_malenum, use = "pairwise.complete.obs")
print(male.cor)
corrplot(male.cor)

mix_gen.cor = cor(sub_mix_gen_num , use = "pairwise.complete.obs")
print(mix_gen.cor)
#However there is no data is this one!

#Now we will begin the linear modeling part of the analysis
#Now this part of my r-code will explore correlations between predictors for outcomes
install.packages("corrplot")
install.packages("displayr")
install.packages("Hmisc")
library("Hmisc")
library(corrplot)


#First let's import my dataset
netflix_data_2 = read.csv("/Users/mielafoster/Desktop/netflix.csv")
View(netflix_data_2)

#Now in order to do our correlation matrix we have to remove the nonnumeric data
netflix_num = subset(netflix_data_2, select = -c(show_id,type, director, cast, country, rating, duration, listed_in, title, description, gender,Race) )
View(netflix_num)


#Now we're going to make our first correlation matrix
netflix.cor = cor(netflix_num, use = "pairwise.complete.obs")
print(netflix.cor)
corrplot(netflix.cor)

#Let's see if we can extract correlation between each racial group and the other predictors we have


sub_blacknum <- netflix_num[netflix_num$race_num == 1,]
View(sub_blacknum)

sub_latinonum <- netflix_num[netflix_num$race_num == 3,]
View(sub_latinonum)

sub_asiannum<- netflix_num[netflix_num$race_num == 2,]
View(sub_whitenum)

sub_whitenum <- netflix_num[netflix_num$race_num == 0,]
View(sub_whitenum)

sub_middleeastnum <- netflix_num[netflix_num$race_num == 4,]
View(sub_middleeastnum)

sub_ethnic_mixnum <- netflix_num[netflix_num$race_num == 5,]
View(sub_ethnic_mixnum)

#Now we need to find each correlation matrix for each racial group
black.cor = cor(sub_blacknum, use = "pairwise.complete.obs")
print(black.cor)
corrplot(black.cor)

latino.cor = cor(sub_latinonum, use = "pairwise.complete.obs")
print(latino.cor)
corrplot(latino.cor)

asian.cor = cor(sub_whitenum, use = "pairwise.complete.obs")
print(asian.cor)
corrplot(asian.cor)

white.cor = cor(sub_whitenum, use = "pairwise.complete.obs")
print(white.cor)
corrplot(white.cor)

middle.cor = cor(sub_middleeastnum, use = "pairwise.complete.obs")
print(middle.cor)
corrplot(middle.cor)

ethnicmix.cor = cor(sub_ethnic_mixnum, use = "pairwise.complete.obs")
print(ethnicmix.cor)
corrplot(ethnicmix.cor)

#Now I want to subset my data for gender
sub_femalenum <- netflix_num[netflix_num$gender_num == 1,]
sub_malenum <- netflix_num[netflix_num$gender_num == 0,]
sub_mix_gen_num <- netflix_num[netflix_num$gender_num == 2,]
View(sub_mix_gen_num)

#Now create Correlation matricies 
female.cor = cor(sub_femalenum, use = "pairwise.complete.obs")
print(female.cor)

male.cor = cor(sub_malenum, use = "pairwise.complete.obs")
print(male.cor)
corrplot(male.cor)

mix_gen.cor = cor(sub_mix_gen_num , use = "pairwise.complete.obs")
print(mix_gen.cor)


install.packages("ggplot2")
library(ggplot2)
install.packages("moonBook")
library(moonBook)
install.packages("ggeffects")
library(ggeffects)
install.packages("ggiraphExtra")
library(ggiraphExtra)

#These models are for the IMDB ratings

#Model 1: This has interaction terms of gender date_added
imdb_model1 = lm(IMDB ~ gender_num + race_num + type_num + votes + (gender_num*date_added), data = netflix_num)
summary(imdb_model1)
model1 <- ggpredict(imdb_model1,se=TRUE, interactive=TRUE)
plot(model1)
ggplot(imdb_model1,aes(y=IMDB,x=date_added,color=factor(gender_num)))+geom_point()+stat_smooth(method="lm",se=FALSE)


#Model 2: This has interaction terms of  race and date_added
imdb_model2 = lm(IMDB ~ gender_num + race_num + type_num + votes + (race_num*date_added), data = netflix_num)
summary(imdb_model2)
model2 <- ggpredict(imdb_model2,se=TRUE, interactive=TRUE)
plot(model2)
ggplot(imdb_model2,aes(y=IMDB,x=date_added,color=factor(race_num)))+geom_point()+stat_smooth(method="lm",se=FALSE)

#Now let's compare these models
anova(imdb_model1, imdb_model2)

#These models are for Rotten Tomato ratings

#Model 1: This has interaction terms of gender and race and gender and date_added
tomato_model1 = lm(tomato ~ gender_num +race_num + type_num +(gender_num*date_added), data = netflix_num)
summary(tomato_model1)
model3 <- ggpredict(tomato_model1,se=TRUE, interactive=TRUE)
plot(model3)
ggplot(tomato_model1,aes(y=tomato,x=date_added,color=factor(gender_num)))+geom_point()+stat_smooth(method="lm",se=FALSE)


#Model 2: This has interaction terms of gender and race  and race and date_added
tomato_model2 = lm(tomato ~ gender_num + race_num + type_num +(race_num*date_added), data = netflix_num)
summary(tomato_model2)
model4 <- ggpredict(tomato_model2,se=TRUE, interactive=TRUE)
plot(model4)
ggplot(tomato_model2,aes(y=tomato,x=date_added,color=factor(race_num)))+geom_point()+stat_smooth(method="lm",se=FALSE)


#Now let's compare these models
anova(tomato_model1, tomato_model2)




