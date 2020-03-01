# Titanic Project
# Tahseen Ahmed
# Feb 24th, 2020
# A learning project as an introduction to data science using the Titanic dataset from Kaggle.
# This project was made by following David Langer's YouTube series on introduction to data science.
# https://youtu.be/fuB7s19g3nQ     
# ===========================================================================================

# Loading csvs into R.
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Adding a "survived" variable. It is used to combine the datasets.
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combining the datasets.
data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

# Gross survival rates
table(data.combined$survived)


# Display a table of combined survival.
table(data.combined$pclass)


# Load ggplot2
library(ggplot2)

# Hypothesis -High class passengers a higher survival rate.
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Passenger Class") +
  ylab("Total") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$name))

# The number of unique names across the testing and training file.
length(unique(as.character(data.combined$name)))


# Store duplicate names as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# Load up stringr
library(stringr)

# Identifying possible correlation with sibsp
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]


# Hypothesis - Titles correspond with age of person (Mr refers to adult man etc.)
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(data.combined$sex == "male"), ]
males[1:5,]

# Add "Title" variable to dataset to explore potential 3D relationship
# Utility function which helps with title extraction.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)

# Plot from the training set
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Distribution of sexes across training/testing dataset.
table(data.combined$sex)

# 3D plot of sex, pclass (passenger class) and "survived" label
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Age and sex seem like strong determining factors for survival. Now to look at distrubtions of age over the dataset.
summary(data.combined$age)
summary(data.combined[1:891,"age"])

# Split up survival rates broken down into age, passenger class and gender.
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is good proxy for male children.
# If the ages match up with children, the hypothesis is correct.
# Oldest boy turns out to be 14.50, so this is correct.
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)


# "Miss." includes girls and possibly single women. Examine data to check:
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None" & !is.na(misses$age),], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# Female children appear to have a different survival rate.
# Save this as a potiential feature to engineer later.
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# Moving onto sibsp, print out summary of table.
summary(data.combined$sibsp)

# If categories are small enough we can use it as a factor.
length(unique(data.combined$sibsp))


data.combined$sibsp <- as.factor(data.combined$sibsp)


# Title is possibly a good predictor of survival. Plot out titles to check
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat parch as viable factor, plot. Parch is # of parent-children relationships.
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Creating a family size feature out of parch.
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Taking look at ticket variable.
str(data.combined$ticket)

# Ticket has too many different values to be a factor. It's better represented as a string.
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

# No apparent structure at first. Look at first letters of unique tickets.
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)


# Not many uniques, can create a factor.
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# General plot of ticket data.
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by first character of ticket") +
  xlab("First letter of ticket") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like a viable predictor. Split up and plot to see if patterns arise.
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("First Char. of Ticket") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Combine with pclass, title and ticket to see if there's a pattern.
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("First Char. of Ticket") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


# Next, look at fare prices of passengers
summary(data.combined$fare)
length(unique(data.combined$fare))

# Checking if fare price is a good predictor of survival.
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")


# Taking a look at "cabin" variable.
str(data.combined$cabin)

# Too many different factors for cabin, so we'll represent it as a string.
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

# Replacing all empty cabins with "U".
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]


# Looks like first letter can be viable factor.
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by first letter of 'cabin'") +
  xlab("First letter of cabin.") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Possible predictor. Provide a detailed plot to chec
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Checking if cabin improves patterns among pclass and title.
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$embarked)
levels(data.combined$embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Exploratory Modelling Time

library(randomForest)

rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 2000)
rf.1
varImpPlot(rf.1)


rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 2000)
rf.2
varImpPlot(rf.2)


rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 2000)
rf.3
varImpPlot(rf.3)


rf.train.4 <- data.combined[1:891, c("pclass", "title", "sibsp", "parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 2000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 2000)
rf.5
varImpPlot(rf.5)



# Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("pclass", "title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 2000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("pclass", "title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 2000)
rf.7
varImpPlot(rf.7)


# Video 5: Cross Validation

# Submit test records and features
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

# Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160215_1.csv", row.names = FALSE)

# Our submission scores 0.79426, OOB predicts 0.8159
library(caret)
library(doSNOW)

# Research shows 10-fold CV is a good place to start.
# Stratify CV: Create folds and ensure survived/dead ratio matches training set better.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549

table(rf.label[cv.10.folds[[33]]])
308 / 494


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 2000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# Not close enough to OOB prediction. Bring down k folds.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 2000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3


# Video 6: Exploratory Modeling 2

# Use single decision tree to understand what's happening

# Install and load packages
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Following from video 5: Using 3 CV 10 times.

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Points of Interest:
# Mr. and Otherr. predicted to perish correctly 83.2%
# Master. and Miss. and Mrs. in 1st\2nd class predicted to survive 94.9% of time correctly.
# Master, Miss and Mrs. in 3rd glass predicted to perish with 100% accuracy (ouch)
# Master, Miss and Mrs in 3rd class with family sizes not being 5,6,8 or 11 survive with 59.6% accuracy


# rpart and rf reinforce importance of title.
table(data.combined$title)

# Parse out last name and title
data.combined[1:25, "name"]

name.splits <- str_split(data.combined$name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# Investigating the 'the' title.
data.combined[which(titles == "the"),]

# Turn other titles into aliases.
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)

# Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) + 
  ggtitle("Surival Rates for new.title by pclass")

# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." | 
                 data.combined$new.title == "Rev." |
                 data.combined$new.title == "Sir." |
                 data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

# Visualize 
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() +
  facet_wrap(~ pclass) +
  ggtitle("Surival Rates for Collapsed new.title by pclass")


# Grab features
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive in on 1st class Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female in first class Mr.?
first.mr.df[first.mr.df$sex == "female",]

# Update new.title feature
indexes <- which(data.combined$new.title == "Mr." & 
                 data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

# Checking other gender and title discrepancies.
length(which(data.combined$sex == "female" & 
             (data.combined$new.title == "Master." |
              data.combined$new.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Look at 1st class misters.
summary(first.mr.df[first.mr.df$survived == "1",])
View(first.mr.df[first.mr.df$survived == "1",])

# Investigating passengers of high fare prices.
indexes <- which(data.combined$ticket == "PC 17755" |
                 data.combined$ticket == "PC 17611" |
                 data.combined$ticket == "113760")
View(data.combined[indexes,])

# Survival rate of 1st class misters by fare price.
ggplot(first.mr.df, aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")


# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)


# Visualize new features
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = avg.fare, fill = survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")


# Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# One missing value, investigate
data.combined[is.na(data.combined$avg.fare), ]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == 1 &
                                     ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# Use caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all of the data.
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# Focus on just 1st class all-up?
indexes <- which(data.combined$pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again


# See if feature engineering made a difference.
features <- c("pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Video 7: Submitting, scoring, and analysis.



# Rpart: 0.80383
# Submit test records and features.
test.submit.df <- data.combined[892:1309, features]

# Make predictions using model.
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20160619_1.csv", row.names = FALSE)


# RF Scores: 0.80861
features <- c("pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp


test.submit.df <- data.combined[892:1309, features]

# Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20160619_1.csv", row.names = FALSE)

# Tip: To improve on model, focus on where it gets predictions wrong.

# Explore collections of features using mutual information to gain insight.
# By intuition, the plot of the tree should align to the definition of mutual information.
#install.packages("infotheo")
library(infotheo)

mutinformation(rf.label, data.combined$pclass[1:891])
mutinformation(rf.label, data.combined$sex[1:891])
mutinformation(rf.label, data.combined$sibsp[1:891])
mutinformation(rf.label, data.combined$parch[1:891])
mutinformation(rf.label, discretize(data.combined$fare[1:891]))
mutinformation(rf.label, data.combined$embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))

# Leverage tsne algorithim to create 2D representation of data.
# Passengers with titeles other than "Mr."
#install.packages("Rtsne")
library(Rtsne)
most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$survived != "None")

set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")


# Getting a baseline: use conditional mutual info on tsne x and y features for females
# and boys in first and second class. Intuitively: the combination of these features should be higher than others.
condinformation(most.correct$survived[indexes], discretize(tsne.1$Y[indexes,]))

# Use CMI on most used features: new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "pclass")])


# Look at adult males as this is biggest room for improvement. Visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$survived[indexes], discretize(tsne.2$Y[indexes,]))


# Create tsne features for all training data and using them in model.
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]
