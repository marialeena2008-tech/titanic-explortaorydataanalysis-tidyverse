######### Reading and writing files ######
# get the working directory
getwd()

# Set the working directory
setwd("C:/Users/Admin/Documents/R Program/files")
getwd()
library(tidyverse)

## Reading CSV files
titanic <- read.csv("titanic_data.csv",header=T)

##Strcture of the data
str(titanic)

#First six  records
head(titanic)

#Last six records
tail(titanic)

#Summary of the data
summary(titanic)

#Selection the specific column
titanic$Survived ## selection a single column
titanic$Gender

titanic[,c("Name","Gender","Fare")] # Selection multiple columns

titanic[,5:8] #select column by Index

### FILTER Operations (Row Selection)
#Filter and select passengers above the age of 35
titanic[titanic$Age > 35,c("PassengerId","Gender","Age")]
titanic

#Selecting using SELECT-Select specific Columns
sel_set_1 <- titanic %>% select(Pclass,Age,Fare,Survived)

#Females only
female_passengers <- titanic %>%
  filter(Gender=="female")%>% select(Pclass ,Age , Fare, Survived)

# Create
# ifelse(condition,Truth result,False result)
titanic$Survived_Status <- ifelse(titanic$Survived > 0,"Survived","Not Survived")
ifelse(titanic$Survived==1,"Survived","Not Survived")

# create a family count using matate()
titanic %>% mutate (FamilyMember = titanic$SibSp+titanic$Parch)%>% head()

# create an Adult / Child column using age
titanic %>% mutate(AgeGroup= ifelse(titanic$Age>18,"Adult","Child")) %>% head()

titanic$AgeGroup <- ifelse(titanic$Age>18,"Adult","Child")
##  what is feature engineering

##### SORTING #######
# Sort by ascending fare
fares_asc <- titanic %>% arrange(Fare)

#Sort by desecending frequency
fares_dsc <- titanic %>% arrange(desc(Fare))

#Sort by class then gender then age
class_gender_age <- titanic %>% arrange(Pclass,Gender,Age)

#update the Age of passenger id 1 to 23
# what is the current age og passenger ig 1
titanic[titanic$PassengerId==1,"Age"] #22
titanic$Age[titanic$PassengerId==1]

#update it to 23
titanic$Age[titanic$PassengerId==1] <- 23
titanic$Age[titanic$PassengerId==1]

#Group by 
#Find the number of male/female passenger
titanic %>% group_by(Gender)%>% summarise(Count=n())
titanic %>% group_by(Pclass)%>% summarise(Count=n())

titanic %>% group_by(Gender)%>% summarise(AvgAge=mean(Age))# Na bcz few value are NA, So maen is not possible
titanic %>% group_by(Gender)%>% summarise(AvgAge=mean(Fare))

#Count survivors by gender
titanic %>% group_by(Gender)%>% summarise(Survived=sum(Survived))

#Count passengers by class 
titanic %>% group_by(Pclass)%>% summarise(Count=n())

#count survivors by class 
titanic %>% group_by(Pclass)%>% summarise(Survived=sum(Survived))%>% arrange(Survived)

# Writing data to a file
write.csv(titanic,"Titanic_Modified.csv")

#########################################################################################
# get the working directory
getwd()

# Set the working directory
setwd("C:/Users/Admin/Desktop/R Program/files")
getwd()
# Reading the text
text_data <- readLines("Text_data.txt")
text_data

students <- read.table("student_marks.txt",header=TRUE)
students

# Writing to a text files
student_data <- data.frame(
  Name = c("Abhishek", " Mayuri", "Arun Peter","Risha"),
  Age = c(20,22,19,21),
  Marks = c(85,90,78,88)
)

write.table(
  student_data,
  "StudentDetails.txt",
  sep = "\t",
  row.names = FALSE
)
