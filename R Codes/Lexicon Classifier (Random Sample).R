library("e1071")
library("RTextTools")
library(stringr)

educationwords <- scan("~/Desktop/BT Assignment 2 (Store Data)/lexicons/education.txt", what="character", comment.char=";")
financewords <- scan("~/Desktop/BT Assignment 2 (Store Data)/lexicons/finance.txt", what="character", comment.char=";") 
gamewords <- scan("~/Desktop/BT Assignment 2 (Store Data)/lexicons/game.txt", what="character", comment.char=";") 
weatherwords <- scan("~/Desktop/BT Assignment 2 (Store Data)/lexicons/weather.txt", what="character", comment.char=";") 
socialwords <- scan("~/Desktop/BT Assignment 2 (Store Data)/lexicons/social_iteration_2.txt", what="character", comment.char=";") 

score.sentiment = function(sentence, edu.words, fin.words, game.words, weather.words , social.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # clean up sentences with R's regex-driven global substitute, gsub():
  # http://www.regular-expressions.info/posixbrackets.html
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  # and convert to lower case:
  sentence = tolower(sentence)
  
  # split into words. str_split is in the stringr package
  word.list = str_split(sentence, '\\s+')
  # sometimes a list() is one level of hierarchy 
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  edu.matches = match(words, edu.words)
  fin.matches = match(words, fin.words)
  game.matches = match(words, game.words)
  weather.matches = match(words, weather.words)
  social.matches = match(words, social.words)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
  edu.matches = !is.na(edu.matches)
  fin.matches = !is.na(fin.matches)
  game.matches = !is.na(game.matches)
  weather.matches = !is.na(weather.matches)
  social.matches = !is.na(social.matches)
  
  #  print(sum(edu.matches))
  #  print(sum(fin.matches))
  #  print(sum(game.matches))
  #  print(sum(weather.matches))
  #  print(sum(social.matches))
  
  real = c(sum(edu.matches),sum(fin.matches),sum(game.matches),sum(weather.matches),sum(social.matches))
  n = max(real)
  if (n == 0){
    return(NA)
  }
  if (n == sum(edu.matches)){
    return("education")
  }
  if (n == sum(fin.matches)){
    return("finance")
  }
  if (n == sum(game.matches)){
    return("game")
  }
  if (n == sum(weather.matches)){
    return("weather")
  }
  if (n == sum(social.matches)){
    return("social")
  }
}

################################################################
################################################################

library("textcat") #to filter out english reviews
library("e1071")
library("RTextTools")
library("stringr")
library("tm")
set.seed(1)

#compile 1000 reviews for weather apps and 2000+ for finance apps, weather more than 5 words, and finance more than 7:

randomsampleprocessing <- function(app, wordnum, correctclass){
  app = csvtodf(app)
  app = english(app)
  app = wordlength(app, wordnum)
  app = textprocess(app)
  app = addclass(app, correctclass)
}

#function to put text reviews into data frame
csvtodf = function(csv){
  app = data.frame(text = csv$review)
}

#function to filter out english reviews

english <- function(app){
  
  num = nrow(app)
  vector = rep("", num)
  for (i in 1:num){
    vector[i] = textcat(app$text[i])
  }
  app = cbind(app, vector)
  app = app[!(app$vector != "english" ),]
  app = na.omit(app)
  app = app$text
  app = data.frame(app)
  
}

wordlength = function(app, number){
  app = data.frame(app)
  num = nrow(app)
  vector = rep(0, num)
  for (i in 1:num){
    vector[i] = length(strsplit(as.character(app[i,1])," ")[[1]])
  }
  app = cbind(app, vector)
  app = app[!(app$vector <= number),]
  app = app[,1]
}


#function for text processing 
#input is not dataframe form, that's why I added last line to convert it back.

textprocess <- function(df){
  
  df = tolower(df)
  df = removeNumbers(df)
  df = stripWhitespace(df)
  df = removePunctuation(df)
  df = stemDocument(df)
  df = data.frame(df)
}

#function to add classifier class

addclass = function(app, nameclass){
  vector = rep(nameclass,nrow(app))
  app = data.frame(text = app, class = vector)
}

#reviewclass is a data frame column instead of a string eg "finance" or "weather"

samplesize = function(data, size){
  n = nrow(data)
  data = data[sample(n),]
  data = data[1:size,]
}

################################################################
################################################################


################################################################
################################################################

education_weather_data = rbind(education_data, weather_data)
education_weather_finance_data = rbind(education_weather_data, finance_data)
education_weather_finance_social_data = rbind(education_weather_finance_data, social_data)
all_data = rbind(education_weather_finance_social_data, game_data)

education_weather_test_data = rbind(education_test_data, weather_test_data)
education_weather_finance_test_data = rbind(education_weather_test_data, finance_test_data)
education_weather_finance_social_test_data = rbind(education_weather_finance_test_data, social_test_data)
all_test_data = rbind(education_weather_finance_social_test_data, game_test_data)

################################################################
################################################################

vector = rep(0, 10000)
for (i in 1: 10000){
  vector[i] = score.sentiment(all_data[i,1], educationwords, financewords, gamewords, weatherwords, socialwords)
}
vector
real = all_data[1:10000,2]
real = matrix(real,nrow = 10000, ncol = 1)
com = matrix(vector, nrow = 10000, ncol = 1)
table=table(com, real )

colnames(table) = c("weather", "finance", "game", "social", "education")
rownames(table) = c("weather", "finance", "game", "social", "education")
table

sum(table)
sum(diag(table))
sum(diag(table))/ sum(table)


################################################################
################################################################

total_education = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_education.csv")
total_social = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_social.csv")
total_finance = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_finance.csv")
total_game = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_game.csv")
total_weather = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_weather.csv")

sample_education = samplesize(total_education, 10000)
sample_social = samplesize(total_social, 10000)
sample_finance = samplesize(total_finance, 10000)
sample_game = samplesize(total_game, 10000)
sample_weather = samplesize(total_weather, 10000)

sample_education_process = randomsampleprocessing(sample_education, 5, "education")
sample_social_process = randomsampleprocessing(sample_social, 5, "social")
sample_finance_process = randomsampleprocessing(sample_finance, 5, "finance")
sample_game_process = randomsampleprocessing(sample_game, 5, "game")
sample_weather_process = randomsampleprocessing(sample_weather, 5, "weather")

sample_education_train = sample_education_process[1:1000,]
sample_education_test = sample_education_process[1001:2000,]

sample_social_train = sample_social_process[1:1000,]
sample_social_test = sample_social_process[1001:2000,]

sample_finance_train = sample_finance_process[1:1000,]
sample_finance_test = sample_finance_process[1001:2000,]

sample_game_train = sample_game_process[1:1000,]
sample_game_test = sample_game_process[1001:2000,]

sample_weather_train = sample_weather_process[1:1000,]
sample_weather_test = sample_weather_process[1001:2000,]

################################################################
################################################################

education_weather_data = rbind(sample_education_train, sample_weather_train)
education_weather_finance_data = rbind(education_weather_data, sample_finance_train)
education_weather_finance_social_data = rbind(education_weather_finance_data, sample_social_train)
all_data = rbind(education_weather_finance_social_data, sample_game_train)

education_weather_test_data = rbind(sample_education_test, sample_weather_test)
education_weather_finance_test_data = rbind(education_weather_test_data, sample_finance_test)
education_weather_finance_social_test_data = rbind(education_weather_finance_test_data, sample_social_test)
all_test_data = rbind(education_weather_finance_social_test_data, sample_game_test)

################################################################
################################################################

lexicon_vector = rep(0, 500)
for (i in 1: 500){
  lexicon_vector[i] = score.sentiment(all_test_data[i,1], educationwords, financewords, gamewords, weatherwords, socialwords)
}
#vector
real = all_test_data[1:500,2]
real = matrix(real,nrow = 500, ncol = 1)
lexicon_com = matrix(lexicon_vector, nrow = 500, ncol = 1)
lexicon_table = table(lexicon_com, real)

sum(lexicon_table)
sum(diag(lexicon_table))
sum(diag(lexicon_table))/ sum(lexicon_table)

################################################################
################################################################

score.sentiment("study exam grade money play friend chat social dating", educationwords, financewords, gamewords, weatherwords, socialwords)

# at 20 words, it is 0.73 accuracy
#

#test 2 that social is "weakened" cos it seems to be categorising wrong categories