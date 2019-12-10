#loading librarys
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#import the data from working directory
crimedata=read.csv("crime.csv") #data can be found from https://www.kaggle.com/AnalyzeBoston/crimes-in-boston

#analyzing content of data
names(crimedata)
head(crimedata)
unique(crimedata$OFFENSE_CODE)
unique(crimedata$OFFENSE_CODE_GROUP)
unique(crimedata$DISTRICT)

#Predict the crime given all other information
#crime is given by a code/code_offense_group

#This is catagorical data, so this will be a catagorical problem
#first we need a way to catagorize the data

#cleaning unneeded parts from crimedata
selecteddata <- crimedata %>% select(OFFENSE_CODE, DISTRICT, MONTH, DAY_OF_WEEK, HOUR, OFFENSE_CODE_GROUP)
names(selecteddata)

#Eliminating all entries with no entered values
selecteddata <- selecteddata[! selecteddata$DISTRICT %in% "",]
selecteddata <- selecteddata[! selecteddata$OFFENSE_CODE %in% "",]
selecteddata <- selecteddata[! selecteddata$MONTH %in% "",]
selecteddata <- selecteddata[! selecteddata$DAY_OF_WEEK %in% "",]
selecteddata <- selecteddata[! selecteddata$HOUR %in% "",]
selecteddata <- selecteddata[! selecteddata$OFFENSE_CODE_GROUP %in% "",]
cleandata <- selecteddata
head(cleandata)
dim(cleandata)


#creating the datasets
set.seed(1, sample.kind="Rounding")
index <- sample(nrow(cleandata), 100000)
x <-  cleandata[index,]
y <-  factor(cleandata$OFFENSE_CODE[index])

index1 <- sample(nrow(cleandata), 1000)
x_test <-  cleandata[index1,]
y_test <-  factor(cleandata$OFFENSE_CODE[index1])


#visualizing data
#district b2
#given a specific district, guess the most common crime group
districtB2 <- x %>% filter(DISTRICT %in% "B2")
plot(districtB2$OFFENSE_CODE_GROUP)
B2 <- table(districtB2$OFFENSE_CODE_GROUP)
guessB2 <- names(which(max(B2)==B2))
guessB2

#district e18
districtE18 <- x %>% filter(DISTRICT %in% "E18")
plot(districtE18$OFFENSE_CODE_GROUP)
E18 <- table(districtE18$OFFENSE_CODE_GROUP)
guessE18 <- names(which(max(E18)==E18))
guessE18

#Does Time of day effect the crime type/rate?
B2hour21 <- districtB2 %>% filter(HOUR %in% "21")
plot(B2hour21$OFFENSE_CODE_GROUP)
B2h21 <- table(B2hour21$OFFENSE_CODE_GROUP)
guessB2h21 <- names(which(max(B2h21)==B2h21))
guessB2h21 #so specifying the hour does change the crime rate

#Does Day effect the crime type/rate?
B2dayF <- districtB2 %>% filter(DAY_OF_WEEK %in% "Friday")
plot(B2dayF$OFFENSE_CODE_GROUP)
B2dF <- table(B2dayF$OFFENSE_CODE_GROUP)
guessB2dayF <- names(which(max(B2dF)==B2dF))
guessB2dayF
B2dayM <- districtB2 %>% filter(DAY_OF_WEEK %in% "Monday")
plot(B2dayM$OFFENSE_CODE_GROUP)
B2dM <- table(B2dayM$OFFENSE_CODE_GROUP)
guessB2dayM <- names(which(max(B2dM)==B2dM))
guessB2dayM #So day also effects the crime

#Putting it all together in cleandata
B3M9DFH22 <- cleandata %>% filter(DISTRICT %in% "B3") %>% filter(MONTH %in% "9") %>% filter(DAY_OF_WEEK %in% "Monday") %>% filter(HOUR %in% "21")
plot(B3M9DFH22$OFFENSE_CODE_GROUP)
B3M9DFH22Table <- table(B3M9DFH22$OFFENSE_CODE_GROUP)
B3M9DFH22Guess  <- names(which(max(B3M9DFH22Table)==B3M9DFH22Table))
B3M9DFH22Guess 

#Start putting the predictions all together
GuessList <- function(CrimeList){
  tempList <- list()
  for(i in 1:nrow(CrimeList)){
    tempDI <- CrimeList[i,2] #district of the indexed row
    tempM <- CrimeList[i,3] #Month of the indexed row
    tempDAY <- CrimeList[i,4] #Day of the indexed row
    tempH <- CrimeList[i,5] #Hour of the indexed row
    tempG <- x %>% filter(DISTRICT %in% tempDI) %>% filter(MONTH %in% tempM) %>% filter(DAY_OF_WEEK %in% tempDAY) %>% filter(HOUR %in% tempH) #using x as training set
    tempTABLE  <- table(tempG$OFFENSE_CODE_GROUP)
    tempGuess  <- names(which(max(tempTABLE)==tempTABLE))
    tempList[i] <- tempGuess[1] #Adding the most common crime to a List. If there are more than 1 most common, it will take the first value
  }
  tempList
}
GuessList(x) #this takes some time to run

#Accuracy Test
#lets use x_test to guage accuracy of a small set first
accuracy_test <- function(test,true){
  p <- 0
  for(i in 1:length(test)){
    j <- ifelse(test[i] == true[i,6],1,0)
    p <- p+j
  }
  l = p/length(test)
  print(l)
}#test and true must have same length
accuracy_test(x_test,x_test2)

test <- GuessList(x_test)
accuracy_test(test,x_test)

test2 <- GuessList(x)
accuracy_test(test2,x)

test3 <- GuessList(cleandata) #Takes a REAL long time to run
accuracy_test(test3,cleandata)
