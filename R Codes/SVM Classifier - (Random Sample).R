library("textcat") #to filter out english reviews
library("e1071")
library("RTextTools")
library("stringr")
library("tm")
set.seed(100)

#compile 1000 reviews for weather apps and 2000+ for finance apps, weather more than 5 words, and finance more than 7:

randomsampleprocessing <- function(app, wordnum, correctclass){
  app = csvtodf(app)
  app = english(app)
  app = wordlength(app, wordnum)
  app = textprocess(app)
  app = addclass(app, correctclass)
}

#function to put text reviews into data frame
csvtodf = function(app){
  app = data.frame(text = app$review)
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

#function to restrict word length of reviews

matrify = function(matrix){
  matrix = create_matrix(matrix[,1], language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=T, removePunctuation=T, removeSparseTerms=0.998)
  matrix = as.matrix(matrix) 
  #is.matrix(fin_matrix) 
}

predictTest <- function(test_text, mat, classifier){
  
  train_mat = mat[1:2,]
  #Note: for demo purpose, if we use 1 row here, R will treat it as a vector, instead of data.frame, so everything goes wrong
  train_mat[,1:ncol(train_mat)] = 0
  
  #reset the counts of each term (make everything 0)
  
  test_matrix = create_matrix(test_text, language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=T, removePunctuation=T)
  test_mat <- as.matrix(test_matrix)
  
  #for testing DocumentTermMatrix, only select those terms appeared in training data
  for(col in colnames(test_mat)){
    if(col %in% colnames(train_mat))
    {
      train_mat[2,col] = test_mat[1,col]; # fill up the 2nd row of training data using the counts of terms in testing data
    }
  }
  
  #test_mat = as.matrix((test_mat))
  row.names(train_mat)[1] = ""
  row.names(train_mat)[2] = test_text #actually, now the 2nd row becomes our focal
  p <- predict(classifier, train_mat[1:2,])
  # print(as.character(p[2]))
  as.character(p[2])
}

#reviewclass is a data frame column instead of a string eg "finance" or "weather"

samplesize = function(data, size){
  n = nrow(data)
  data = data[sample(n),]
  data = data[1:size,]
}

################################################################
################################################################

total_education = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_education.csv")
total_social = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_social.csv")
total_finance = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_finance.csv")
total_game = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_game.csv")
total_weather = read.csv("~/Desktop/BT Assignment 2 (Store Data)/combined_weather.csv")

sample_education = samplesize(total_education, 20000)
sample_social = samplesize(total_social, 25000)
sample_finance = samplesize(total_finance, 20000)
sample_game = samplesize(total_game, 35000)
sample_weather = samplesize(total_weather, 20000)

sample_education_process = randomsampleprocessing(sample_education, 20, "education")
sample_social_process = randomsampleprocessing(sample_social, 20, "social")
sample_finance_process = randomsampleprocessing(sample_finance, 20, "finance")
sample_game_process = randomsampleprocessing(sample_game, 20, "game")
sample_weather_process = randomsampleprocessing(sample_weather, 20, "weather")

sample_education_train = sample_education_process[1:3000,]

sample_social_train = sample_social_process[1:3000,]

sample_finance_train = sample_finance_process[1:3000,]

sample_game_train = sample_game_process[1:3000,]

sample_weather_train = sample_weather_process[1:3000,]

################################################################
################################################################

sample_education_train_svm = sample_education_process[1:1500,]

sample_social_train_svm = sample_social_process[1:1500,]

sample_finance_train_svm = sample_finance_process[1:1500,]

sample_game_train_svm = sample_game_process[1:1500,]

sample_weather_train_svm = sample_weather_process[1:1500,]

education_weather_data_svm = rbind(sample_education_train_svm, sample_weather_train_svm)
education_weather_finance_svm_data = rbind(education_weather_data_svm, sample_finance_train_svm)
education_weather_finance_social_svm_data = rbind(education_weather_finance_svm_data, sample_social_train_svm)
all_data_svm = rbind(education_weather_finance_social_svm_data, sample_game_train_svm)
matrix_svm = matrify(all_data_svm) #set for svm at 1500 per category

################################################################
################################################################

sample_education_train = sample_education_process[1:3000,]

sample_social_train = sample_social_process[1:3000,]

sample_finance_train = sample_finance_process[1:3000,]

sample_game_train = sample_game_process[1:3000,]

sample_weather_train = sample_weather_process[1:3000,]

education_weather_data = rbind(sample_education_train, sample_weather_train)
education_weather_finance_data = rbind(education_weather_data, sample_finance_train)
education_weather_finance_social_data = rbind(education_weather_finance_data, sample_social_train)
all_data_nb = rbind(education_weather_finance_social_data, sample_game_train)
matrix_nb = matrify(all_data_nb) #set for nb at 3000 per category

################################################################
################################################################

sample_education_test = randomsampleprocessing(samplesize(total_education, 500), 1, "education")
sample_social_test = randomsampleprocessing(samplesize(total_social, 500), 1, "social")
sample_finance_test = randomsampleprocessing(samplesize(total_finance, 500), 1, "finance")
sample_game_test = randomsampleprocessing(samplesize(total_game, 500), 1, "game")
sample_weather_test = randomsampleprocessing(samplesize(total_weather, 500), 1, "weather")

sample_education_test = sample_education_test[1:100,]
sample_social_test = sample_social_test[1:100,]
sample_finance_test = sample_finance_test[1:100,]
sample_game_test = sample_game_test[1:100,]
sample_weather_test = sample_weather_test[1:100,]

education_weather_test_data = rbind(sample_education_test, sample_weather_test)
education_weather_finance_test_data = rbind(education_weather_test_data, sample_finance_test)
education_weather_finance_social_test_data = rbind(education_weather_finance_test_data, sample_social_test)
all_test_data = rbind(education_weather_finance_social_test_data, sample_game_test)

################################################################
################################################################

nb_classifier = naiveBayes(matrix_nb, as.factor(all_data_nb[,2]))

predictTest("study and grades and grade", matrix_nb, nb_classifier)
predictTest("weather is hot and sunny", matrix_nb, nb_classifier)

svm_classifier = svm(matrix_svm, as.factor(all_data_svm[,2]))

predictTest("money wealth savings save", matrix, svm_classifier)
predictTest("fun addictive game to play", matrix, svm_classifier)
predictTest("lots of sunny cloudy ", matrix, svm_classifier)

################################################################
################################################################

nb_vector = rep(0,500)
for (i in 1:500){
  nb_vector[i] = predictTest(all_test_data[i,1], matrix_nb, nb_classifier)
}
real = all_test_data[1:500,2]
real = matrix(real,nrow = 500, ncol = 1)
nb_com = matrix(nb_vector, nrow = 500, ncol = 1)
nb_table = table(nb_com,real)
nb_table

sum(nb_table)
sum(diag(nb_table))
sum(diag(nb_table))/ sum(nb_table)

################################################################

svm_vector = rep(0,500)
for (i in 1:500){
  svm_vector[i] = predictTest(all_test_data[i,1], matrix_svm, svm_classifier)
}
real = all_test_data[1:500,2]
real = matrix(real,nrow = 500, ncol = 1)
svm_com = matrix(svm_vector, nrow = 500, ncol = 1)
svm_table=table(svm_com, real)
svm_table

sum(svm_table)
sum(diag(svm_table))
sum(diag(svm_table))/ sum(svm_table)

################################################################
################################################################

#ensemble method

#creation of 3 col dataframe

combined_table = cbind(svm_vector, lexicon_vector)
combined_table = cbind(combined_table, nb_vector)
combined_table = cbind(combined_table, data.frame(all_test_data[1:500,2]))
print(combined_table)

ensemble = function(svm, lexicon, nb){
  classes = c("education", "finance", "social", "game", "weather")
  score = c(0,0,0,0,0)
  for (i in 1:5){
    if (svm == classes[i]){
      score[i] = score[i] + 2
    }
  }
  if (is.na(lexicon) == FALSE){
    for (p in 1:5){
      if (lexicon == classes[p]){
        score[p] = score[p] + 2
      }
    }
  }
  for (s in 1:5){
    if (nb == classes[s]){
      score[s] = score[s] + 1
    }
  }
  
  tab = cbind(classes, score)
  
  if (is.na(lexicon) == TRUE){
    return(svm)
  }
  
  if (svm != lexicon & lexicon != nb & svm != nb){
    return (lexicon)
  }

  for (i in 1:5){
  #  if (nrow(which()) if svm and lexicon and nb all diff, I want to choose lexi
    if (tab[i,2] == max(score)){
      return(as.String(tab[i,1]))
    }
  }
}

################################################################
################################################################

combined_vector = rep(0,500)
for (i in 1:500){
  combined_vector[i] = ensemble(as.String(combined_table[i,1]),as.String(combined_table[i,2]),as.String(combined_table[i,3]))
}

#final combined classifier
response = all_test_data[1:500,2]
response = matrix(real,nrow = 500, ncol = 1)
combined_vector = matrix(combined_vector, nrow = 500, ncol = 1)
combined_table=table(combined_vector, response)
combined_table

sum(combined_table)
sum(diag(combined_table))
sum(diag(combined_table))/ sum(combined_table)

ensemble("weather", "social" , "weather")
    
real = all_test_data[1:500,2]
real = matrix(real,nrow = 500, ncol = 1)

