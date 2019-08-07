# Load raw data 
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE) 

# add survived variable to test set to allow for combined data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# combine data sets
data.combined <- rbind(train, test.survived) 

# a bit about R data type (e.g., factors)
  str(data.combined)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

#  Take a look at Gross Survival rates
table(data.combined$Survived)

# Distribution accross Classes
table(data.combined$Pclass)

# load up ggplot2 package to use for visualization
library(ggplot2)

# Hypothesis rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("TotalCount") +
  labs(fill = "Survived")  

# examine the first few names in the dataset 
head(as.character(train$Name)) 

# how many unique elements are there in both train and test?
length(unique(data.combined$Name)) 

# Two duplicated names, take a closer look
# First, get duplicated names and store them as vectors
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Nest take a look at the record in the combined data set
data.combined[which(data.combined$Name %in% dup.names),] 

# what is up with the "miss" and "mr" thing?
  library(stringr) 
# any correlation with other variables (e.g., sibsp)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

#Hypothesis: name titles collerate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,] 

#check out males to see if pattern continues 
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# expand upon the relationhip between "Survived and "Pclass" by adding a new variable "title"
# create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$title <- as.factor(titles)

# since we only have Survived labels on the train set, only
# use the 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 


ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")+
  geom_label(stat='count',aes(label=..count..), position = "stack") 

# what is the distribution of female to male across train and test set
table(data.combined$Sex) 

# visualize the 3 way relationship of Sex, Pclass and Survival
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# look at the distribution of age over the data set
summary(data.combined$Age) 

# Just to be thorough, take a look at Survival rate broken down into Pclass, Sex, and Age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~ Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")