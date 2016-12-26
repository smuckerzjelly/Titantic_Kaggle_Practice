train <- read.csv("train.csv", header = TRUE)  ##Reads files and keeps column headers
test <- read.csv("test.csv", header = TRUE)

test.survived <- data.frame(Survived = rep("NONE", nrow(test)), test[,]) #Adds the variable "survived" to the test data frame

data.combined <- rbind(train, test.survived)  #Combines the two data frames 

data.combined$Pclass <- as.factor(data.combined$Pclass) #Change class from int to factor
data.combined$Survived <- as.factor(data.combined$Survived)

table(data.combined$Survived)  #Looks at survial rate in table 

table(data.combined$Pclass)

table(data.combined$Sex)

library(ggplot2)


train$Pclass <- as.factor(train$Pclass)    #Plots a bar graph of survival 
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("PClass") +
  ylab("Total Count") +
  labs(fill = "Survived")

head(as.character(train$Name)) #Takes a look at 1st six rows of data

length(unique(as.character(data.combined$Name)))  #How many unique names are there?

dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.Names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses [1:5,]


extractTitle <- function(Name) {
  
  Name <- as.character(Name)
  
    if(length(grep("Miss." , Name)) > 0) {
    return("Miss.")
      }else if(length(grep("Master." , Name)) > 0) {
        return("Master.")
        
      }else if(length(grep("Mrs." , Name)) > 0) {
        return("Mrs.") 
        
      } else if(length(grep("Mr." , Name)) > 0) {
        return("Mr.")
        
      }else {
        return("Other")
}
}

titles <- NULL 
for (i in 1:nrow(data.combined)) {
  titles <-  c(titles, extractTitle(data.combined[i, "name"]))
}

data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,] , mapping = aes(x = Title, fill = Survived)) +
  geom_bar(0.5, stat = "identity") + 
  facet_wrap(~Pclass) +
  ggtitle("PClass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

