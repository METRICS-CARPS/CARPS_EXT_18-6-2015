# State Narcissism Data Scrubbing & Analysis August 7, 2017
# Contact Adam Putnam with questions: adamlputnam@gmail.com
# Link to project on Open Science Framework: osf.io/tnjqs

# This analysis script takes the raw Qualtrics output and organizes it for later analysis.
# It generates several .CSV files that will be needed to run the analysis script.
# It also cuts subjects from being analyzed who do not meet criteria specified in our pre-registration

# * * * * * * * * LOAD PACKAGES * * * * * * * * 
library(reshape)
library(plyr)
library(psych)

# * * * * * * * * IMPORT DATA AND LIST OF STATES * * * * * * * * 
# pull in list of states
stateList = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
       "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
       "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
       "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
       "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
       "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
       "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming") 

# import data
noHeaders = read.csv("State Narcissism Raw Data.csv", header = F, skip = 3) # import data without headers 
headers = read.csv("State Narcissism Raw Data.csv", header = F, colClasses = "character") # import the data for header rows
headers = headers[1:3,  ] # omit non-header rows
names(noHeaders) = iconv(headers[2,  ], "latin1", "ASCII", sub = "") # pull row 2 and remove multi-byte characters; save as headers
data = noHeaders # start main DF
data[data == ""] = NA # convert blank cells to NA

# * * * * * * * * CLEAN UP DATA SET (DELETE UNUSED COLUMNS AND RENAME OTHERS) * * * * * * * *
# build an array with column names to delete
columnsToDelete = c("ResponseID",  "ResponseSet",  "ExternalDataReference",  "Finished",  "Status", "Score-weightedAvg", "Score-weightedStdDev")
columnsToDelete = append(columnsToDelete, grep("Recipient", names(data), value = TRUE))
columnsToDelete = append(columnsToDelete, grep("Thank", names(data), value = TRUE))
columnsToDelete = append(columnsToDelete, grep("Welcome!", names(data), value = TRUE))
columnsToDelete = append(columnsToDelete, grep("IMPORTANT", names(data), value = TRUE, ignore.case = TRUE))
columnsToDelete = append(columnsToDelete, grep("Please answer", names(data), value = TRUE))
columnsToDelete = append(columnsToDelete, grep("You will now be", names(data), value = TRUE))

columnsToDelete2 = names(data) %in% columnsToDelete # create TF vector based on the names

# then exclude the columns from the above array, so that we are left with only the columns we want
trimmedData = data[!columnsToDelete2]

# Rename column names
colnames(trimmedData)[3] = "historyScore"
colnames(trimmedData)[5] = "consent"
colnames(trimmedData)[8] = "citizenship"
colnames(trimmedData)[9] = "education"
colnames(trimmedData)[10] = "fluentEnglish"
colnames(trimmedData)[11] = "birthState"
colnames(trimmedData)[12] = "homeState"
colnames(trimmedData)[13] = "homeState.Years"
colnames(trimmedData)[14] = "currentState"
colnames(trimmedData)[15:24] = paste("Important Event", as.character(1:10))
colnames(trimmedData)[40] = "criticalQuestion"
colnames(trimmedData)[41:90] = paste(as.character(stateList), " NR Rating")
colnames(trimmedData)[91] = "contributedMostState"
colnames(trimmedData)[92] = "contributed2ndMostState"
colnames(trimmedData)[93] = "contributed3rdMostState"
colnames(trimmedData)[94] = "contMostStateRating"
colnames(trimmedData)[95] = "cont2ndMostStateRating"
colnames(trimmedData)[96] = "cont3rdMostStateRating"
colnames(trimmedData)[97] = "contributedLeastMost"
colnames(trimmedData)[98] = "mostSway"
colnames(trimmedData)[99] = "homeState100Percent"
colnames(trimmedData)[100] = "most100Percent"
colnames(trimmedData)[101] = "2ndMost100Percent"
colnames(trimmedData)[102] = "3rdMost100Percent"
colnames(trimmedData)[103] = "allOther100Percent"
colnames(trimmedData)[104] = "egocentricPrediction"
colnames(trimmedData)[105] = "Plus 5 - No State"
colnames(trimmedData)[106:152] = paste("Plus 5 - ", as.character(stateList[1:47]))
colnames(trimmedData)[153] = "Plus 5 - Washington D.C."
colnames(trimmedData)[154:156] = paste("Plus 5 - ", as.character(stateList[48:50]))
colnames(trimmedData)[157] = "Cheat"
colnames(trimmedData)[158] = "Look up?"
colnames(trimmedData)[159] = "surveyDifficulty"
colnames(trimmedData)[160] = "generalComments"

# add subject number and determine sample size
trimmedData$subjectNumber = 1:nrow(trimmedData)
n = max(trimmedData$subjectNumber)
n

# * * * * * * * * CUT DATA BASED ON A PRIORI CRITERIA * * * * * * * 
# check for cuts based on manual inspection of 10 critical events (+ subjects who had duplicate MTurk IDs)
cutListDF = read.csv("historyCuts.csv", header = T) # read in the list of subject numbers to cut from manual inspection.

cutList = cutListDF$subNum # list of subjects to cut

 # start a DF to store data that we are cutting
 cutData = trimmedData[cutList, ] 
 tenEventDrops = nrow(cutData) # calculate # of subjects cut from 10 events failures & mturk ID duplicates
 
 # save the data that we want
 trimmedDataTen = trimmedData[-cutList, ] 

# cut subjects based low history score, cheating, not living in state for at least five years,  not speaking english, or if there is an NA for the critical question
trimmedDataHistory = trimmedDataTen[trimmedDataTen$historyScore > 4, ] 
trimmedDataNoCheat = trimmedDataHistory[trimmedDataHistory$Cheat == "No", ]
trimmedDataPlusFive = trimmedDataNoCheat[trimmedDataNoCheat$homeState.Years != "1 to 5", ]
trimmedDataEnglish = trimmedDataPlusFive[trimmedDataPlusFive$fluentEnglish == "Yes", ]
trimmedDataOMiss = trimmedDataEnglish[!is.na(trimmedDataEnglish$criticalQuestion), ] # use is.na() to pull a TF vector for critical question being NA. Grab opposite.

 # save the cut data for each criteria to the cutData dataframe
 cutData = rbind(cutData, trimmedDataTen[trimmedDataTen$historyScore < 5, ]) # X
 cutData = rbind(cutData, trimmedDataHistory[trimmedDataHistory$Cheat == "Yes", ]) 
 cutData = rbind(cutData, trimmedDataNoCheat[trimmedDataNoCheat$homeState.Years == "1 to 5", ])
 cutData = rbind(cutData, trimmedDataPlusFive[trimmedDataPlusFive$fluentEnglish == "No", ])
 cutData = rbind(cutData, trimmedDataEnglish[is.na(trimmedDataEnglish$criticalQuestion), ])

# save final data
finalData = trimmedDataOMiss

# renumber subject numbers & count subjects
finalData$subjectNumber = 1:nrow(finalData)
finalN = nrow(finalData)
finalN

# check to see the count for each state (before and after cuts)
table(trimmedData$homeState)
table(finalData$homeState)

# calculate the number of dropped subjects for each ommission criteria
historyDrops = nrow(trimmedDataTen) - nrow(trimmedDataHistory)
cheatDrops = nrow(trimmedDataHistory) - nrow(trimmedDataNoCheat)
shortStayDrops = nrow(trimmedDataNoCheat) - nrow(trimmedDataPlusFive)
nonfluentEnglishDrops = nrow(trimmedDataPlusFive) - nrow(trimmedDataEnglish)
missingCQDataDrops = nrow(trimmedDataEnglish) - nrow(trimmedDataOMiss)

# report # of drops for each criteria
tenEventDrops - 19 # 19 duplicate mturk ids included in the ten events drop 
historyDrops
cheatDrops
shortStayDrops
nonfluentEnglishDrops
missingCQDataDrops

# * * * * * * * * GENERATE A SUBJECT LEVEL SUMMARY * * * * * * * *
subjectData = finalData[ , c(1:24, 40, 91:104, 157:161)] # grab everything but history quiz responses, random state ratings, +5 years, MTurkID

# add each subject's average rating for random state
subjectData$Random.State.Ratings = rowMeans(finalData[, 41:90], na.rm = T)

# save Subject Level Data to a text file 
write.csv(subjectData, file = "subjectLevelData.csv")

# OPTIONAL save cut data
 # write.csv(cutData, file = "cutData.csv")

# * * * * * * * * STATE LEVEL SUMMARY * * * * * * * *

# calculate the average non-resident rating for each state
rStateMean = colMeans(finalData[, 41:90], na.rm = TRUE)
rStateSD = apply(finalData[, 41:90], 2, sd, na.rm = TRUE)

# combine the random state ratings w/their names (and add labels)
stateData = data.frame(as.character(stateList), rStateMean, rStateSD)
colnames(stateData) = c("state", "NR.State.Mean", "NR.State.SD")
rownames(stateData) = as.character(stateList)

# add the resident ratings to the state data summary
stateData$Rrating = tapply(finalData$criticalQuestion, finalData$homeState, mean)
stateData$RratingSD = tapply(finalData$criticalQuestion, finalData$homeState, sd)

# reorder columns to put resident ratings first.
stateData = stateData[, c(1, 4, 5, 2, 3)]

# add the resident rating split by condition (P and NP) 
# first create two new DFs based on the prime condition
primeData = subset(finalData, Condition == "P")
noPrimeData = subset(finalData, Condition == "NP")

# then calculate means for the resident states
stateData$Prime.Rrating = tapply(primeData$criticalQuestion, primeData$homeState, mean)
stateData$Prime.RratingSD = tapply(primeData$criticalQuestion, primeData$homeState, sd)
stateData$NoPrime.Rrating = tapply(noPrimeData$criticalQuestion, noPrimeData$homeState, mean)
stateData$NoPrime.RratingSD = tapply(noPrimeData$criticalQuestion, noPrimeData$homeState, sd)

# calculate means for 100% restriction version of question
 # first replace any NAs with 0 for homeState 100 and ContributedMost 100 [subjects may have left field blank]
 finalData$homeState100Percent[is.na(finalData$homeState100Percent)] = 0
 finalData$most100Percent[is.na(finalData$most100Percent)] = 0 
 finalData$`2ndMost100Percent`[is.na(finalData$`2ndMost100Percent`)] = 0 
 finalData$`3rdMost100Percent`[is.na(finalData$`3rdMost100Percent`)] = 0 

  # save results to state data frame 
  stateData$State100PercentMean.ALL = tapply(finalData$homeState100Percent, finalData$homeState, mean, na.rm = T)
  stateData$State100PercentSD.ALL = tapply(finalData$homeState100Percent, finalData$homeState, sd, na.rm = T)

# add the 100% question to the state dataframe but cutting where subjects did not follow directions (e.g., they listed
  # their home state as one of the most important states and rated it twice in the question.
    sd100.1 = subset(finalData, as.character(homeState) != as.character(contributedMostState))
    sd100.2 = subset(sd100.1, as.character(homeState) != as.character(contributed2ndMostState))
    sd100.3 = subset(sd100.2, as.character(homeState) != as.character(contributed3rdMostState))
 
  # calculate the 100% homeState question for each state and save to the state dataframe
    stateData$State100PercentMean = tapply(sd100.3$homeState100Percent, sd100.3$homeState, mean, na.rm = T)
    stateData$State100PercentSD = tapply(sd100.3$homeState100Percent, sd100.3$homeState, sd, na.rm = T)
 
# add 100% version of question, but only cut subjects who failed to follow directions (they not only listed homestate twice,
  # but rated it twice as well)  

  oneHundredData = finalData[ , c(12, 91, 92, 93, 99:103, 161)] # grab just relevant info
  
  oneHundred1Double = subset(oneHundredData, as.character(oneHundredData$homeState) == as.character(oneHundredData$contributedMostState)) 
  oneHundred2Double = subset(oneHundredData, as.character(oneHundredData$homeState) == as.character(oneHundredData$contributed2ndMostState))
  oneHundred3Double = subset(oneHundredData, as.character(oneHundredData$homeState) == as.character(oneHundredData$contributed3rdMostState))
  
  # pull and calculate the number of subjects who did not follow directions for Most Important
  oneHundred1DoubleFollow = subset(oneHundred1Double, oneHundred1Double$homeState100Percent == 0 | oneHundred1Double$most100Percent == 0)
  nrow(oneHundred1Double) # how many people listed home state as most important
  nrow(oneHundred1DoubleFollow) # how many people " and followed directions
  mostFail = nrow(oneHundred1Double) - nrow(oneHundred1DoubleFollow) # how many people " and did NOT follow directions
  
  # pull and calculate the number of subjects who did not follow directions for 2nd Most Important
  oneHundred2DoubleFollow = subset(oneHundred2Double, oneHundred2Double$homeState100Percent == 0 | oneHundred2Double$`2ndMost100Percent` == 0)
  nrow(oneHundred2Double) # how many people listed home state as 2nd most important
  nrow(oneHundred2DoubleFollow) # how many people " and followed directions
  mostFail2 = nrow(oneHundred2Double) - nrow(oneHundred2DoubleFollow) # how many people " and did NOT follow directions
  mostFail2
  
  # pull and calculate the number of subjects who did not follow directions for 3rd Most Important
  oneHundred3DoubleFollow = subset(oneHundred3Double, oneHundred3Double$homeState100Percent == 0 | oneHundred3Double$`3rdMost100Percent` == 0)
  nrow(oneHundred3Double) # how many people listed home state as 2nd most important
  nrow(oneHundred3DoubleFollow) # how many people " and followed directions
  mostFail3 = nrow(oneHundred3Double) - nrow(oneHundred3DoubleFollow) # how many people " and did NOT follow directions
  mostFail3
  
  # identify subjects who failed to follow instructions
  fail1 = subset(oneHundred1Double, oneHundred1Double$homeState100Percent != 0 & oneHundred1Double$most100Percent != 0)
  fail2 = subset(oneHundred2Double, oneHundred2Double$homeState100Percent != 0 & oneHundred2Double$`2ndMost100Percent` != 0)
  fail3 = subset(oneHundred3Double, oneHundred3Double$homeState100Percent != 0 & oneHundred3Double$`3rdMost100Percent` != 0)
  
  failSubjectList = c(fail1$subjectNumber, fail2$subjectNumber, fail3$subjectNumber)
  table(failSubjectList) # note that subjects 310 and 2835 rated their homestate 3 times! so they are counted twice here
  length(unique(failSubjectList)) # how many subjects did not follow instructions
  
  # exclude all of the subjects who failed to follow instructions 
  cut100Data = finalData[-failSubjectList, ]
  
  stateData$noFail100Mean = tapply(cut100Data$homeState100Percent, cut100Data$homeState, mean)
  stateData$noFail100SD = tapply(cut100Data$homeState100Percent, cut100Data$homeState, sd)

  
## * * * * * * * * * * * * * * * * * * * * * * * *     
  
# calculate the narcissistic index for each state
stateData$Narc.Index = stateData$Rrating - stateData$NR.State.Mean
 
# tally the number of residents who rated a state
stateData$RCount = tapply(finalData$homeState, finalData$homeState, length)
 
# tally the number of non-residents who rated a state
stateData$NRCount = apply(finalData[, 41:90], 2, function(x) length(which(!is.na(x))))
 
# count the n for each state reported in 100% version of the question when we eliminated the subjects who rated 
#their home state in the top 3 (including them would be same as resident rating).
stateData$State100PercentCount = tapply(sd100.3$homeState, sd100.3$homeState, length)

# * * * * * * * * SAVE STATE LEVEL SUMMARY AND FINAL DATA* * * * * * * * 
write.csv(stateData, "stateLevelData.csv")
write.csv(finalData, "State Narcissism Scrubbed Data.csv")


# * * * * * * * * CREATE DATA FOR T-TEST FRIENDLY FORMAT (LONG FORMAT)* * * * * * * * 
# save home state, critical question, and subject number
ttestDF = finalData[, c(12, 40, 161)] 

# add resident tag to all the data
ttestDF$Residency = as.factor("Resident") 
names(ttestDF)[1] = "state" # rename the first column to "state"

# save non-resident data to the same data frame
 # first create a mini-DF with just the things you need
 nrRDF = finalData[ , c(41:90, 161)] # save the state that people are rating plus their subject number
 names(nrRDF) = stateList # rename the columns to have just the state name (you'll copy them later)
 names(nrRDF)[51] = "subjectNumber" 

 #first build a dummy dataframe, then create function to grab all non NA data from a column
 tempDF = data.frame(state = factor(), criticalQuestion = integer(), 
                     subjectNumber = integer(), Residency = character()) 
 
 #this function will grab the non-NA entries and other relevant info for the non resident ratings
 nrRatingScraper = function(x){
  temp = !is.na(nrRDF[ , x])
  tempDF = cbind.data.frame(names(nrRDF[x]), nrRDF[temp, x], nrRDF[temp, 51], "NonResident")
  names(tempDF) = names(ttestDF)
  return(tempDF)
 }

 # then use a loop to run the function on each column 
 nrData = data.frame()
 for(i in 1:50){
  nrData = rbind.data.frame(nrData, nrRatingScraper(i))
 }
 
# finally combine the resident and the non-resident data and save as a .csv file
finalTtestDF = rbind(ttestDF, nrData)
write.csv(finalTtestDF, "ttestReadyData.csv")

# * * * * * * * * THE END * * * * * * * * 
# See the analysis script to run the statistical analyses. Make sure that the following .csv files have been generated:
# State Narcissism Scrubbed Data.csv
# stateLevelData.csv
# subjectLevelData.csv
# ttestReadyData.csv


