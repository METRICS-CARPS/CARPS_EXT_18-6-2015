# State Narcissism Data Scrubbing & Analysis, August 7, 2017
# Contact Adam Putnam with questions: adamlputnam@gmail.com
# Link to project on Open Science Framework: osf.io/tnjqs

# LOAD PACKAGES
library(reshape)
library(plyr)
library(psych)
library(pastecs)
library(lsr)
library(car) 
library(ggplot2)
library(fiftystater)
library(dplyr)
library(mapproj)

# load list of state names
stateList = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
              "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
              "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
              "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
              "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
              "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", 
              "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming") 

# pull data files generated from data scrubbing script
finalData = read.csv(file = "State Narcissism Scrubbed Data.csv", header = T)
names(finalData)[1] = "oldRowName"

stateData = read.csv(file = "stateLevelData.csv", header = T)
names(stateData)[1] = "oldRowName"

subData = read.csv(file = "subjectLevelData.csv", header = T)
names(subData)[1] = "oldRowName"

ttestData = read.csv(file = "ttestReadyData.csv")

# * * * * * * * * EGOCENTRIC BIAS * * * * * * * *
# 1 What is the average critical question rating? 
  averageCQ = round(mean(finalData$criticalQuestion), 2)
  averageCQ
  cqStat = round(stat.desc(finalData$criticalQuestion), 2) # detailed statistics
  cqStat
  averageCQ - cqStat[[11]] # 95% CI lower limit
  averageCQ + cqStat[[11]] # 95% CI upper limit
  
  # sum percentage aka What is the "total" percentage for all states?
  totalCQ = sum(stateData$Rrating)
  round(totalCQ, 2)
  
# * * * * * * * * 2 How do resident ratings compare to non-resident ratings? * * * * * * * * 
  # what is the overall resident vs. non-resident rating?
  tapply(ttestData$criticalQuestion, ttestData$Residency, mean)
  nonResCQ = tapply(ttestData$criticalQuestion, ttestData$Residency,mean)[[1]]
  nonResCQ
  nonResStats = tapply(ttestData$criticalQuestion, ttestData$Residency, function(x)round(stat.desc(x), 2))
  nonResStats
  nonResCQ - nonResStats$NonResident[[11]] # lower limit 95% CI
  nonResCQ + nonResStats$NonResident[[11]] # lower limit 95% CI
  
  # run a t-test
  t.test(ttestData$criticalQuestion ~ ttestData$Residency, paired = FALSE, alternative = "less") # alternative predicts that resident > non-resident
  cohensD(ttestData$criticalQuestion ~ ttestData$Residency)
  
  # examine resident vs. non-resident statistics but summarize at state level first
  # sum percentage for non resident ratings (average by state first)
  totalNRCQ = sum(stateData$NR.State.Mean)
  round(totalNRCQ, 2)
  
  round(stat.desc(stateData$Rrating, norm = T), 2) # note min & max: Iowa and Virginia
  round(stat.desc(stateData$NR.State.Mean, norm = T), 2) # note min & max: Wyoming and Virginia
  round(stat.desc(stateData$Narc.Index, norm = T), 2) # note min & max: Washington (negative) & Delaware (Virginia is 2nd highest). Colorado is most accurate
  
  # generate Table 1 (res vs. non-res w/ t-test results & effect sizes) w/ state level data
  table1 = data.frame(stateData$state,stateData$Rrating, stateData$RratingSD, stateData$NR.State.Mean, stateData$NR.State.SD, stateData$Narc.Index)
  
  # calculate ttest for each state
  tempT = ddply(ttestData, "state",function(df)c(
    t.test(df$criticalQuestion ~ df$Residency,alternative = "less")$statistic,
    t.test(df$criticalQuestion ~ df$Residency,alternative = "less")$parameter,
    t.test(df$criticalQuestion ~ df$Residency,alternative = "less")$p.value)
    )
    # note: Bonferroni correction used to determine significance of state comparisokns: 05/50 comparisons = .001
  
  # save results of t-tests
  table1$t = tempT$t *-1
  table1$df = tempT$df
  table1$p = tempT$V1
  
  # calculate cohen's d for each state
  tempES = ddply(ttestData, "state", function(df)cohensD(criticalQuestion~Residency,data=df))
  table1$effect.size = tempES$V1

  # clean up 
  table1[,c(2:8, 10)]=round(table1[,c(2:8, 10)], 2) # round all values to 2 decimal places
  table1$p = round(table1$p, 3) # round p value to 3 decimal places
  
  # mark significant values after bonferonni correction
  table1$sig = "not significant"
  table1[which(table1$p <= .001), 11]= "sig"
  
  # save table 1
  write.csv(table1, "table1.csv")
  
  # calculate how many non-resident observations we have
  describe(stateData$NRCount)
  
# * * * * * * * * # 3 How do the prime vs. no-prime conditions compare to one another? * * * * * * * * 
  # how many subjects in each condition?
  table(subData$Condition)
  
  # calculate overall prime vs. no-prime at individual level
  primeNoPrimeStats = tapply(finalData$criticalQuestion, finalData$Condition, stat.desc)
  primeNoPrimeStats
  
  primeMean = primeNoPrimeStats$P[["mean"]]
  noPrimeMean = primeNoPrimeStats$NP[["mean"]]
  primeMean
  noPrimeMean
     # calculate 95% CI
      18.10 + 1.01 # no prime
      18.10 - 1.01 # no prime
      18.38 + 1.03 # prime
      18.38 - 1.03 # prime
      
  # individual as unit of analysis
  t.test(criticalQuestion ~ Condition,  data = finalData,  alternative = "greater") # one tailed test with prime < not prime
  cohensD(criticalQuestion ~ Condition, data = finalData)

# then calculate difference by summing at state level
  # calculate the total rating for prime and no prime condition
  sum(stateData$Prime.Rrating)
  sum(stateData$NoPrime.Rrating)
  
  # create dataframe to hold stats for prime vs. no prime at the state level (including t-tests)
  table2 = data.frame(
    stateData$state, 
    stateData$Prime.Rrating, 
    stateData$Prime.RratingSD, 
    stateData$NoPrime.Rrating, 
    stateData$NoPrime.RratingSD)
  
  # calculate n,  ttest,  and cohens D for each state for prime vs. no prime
    # first count how many subjects are in each condition for each state
    tempN = ddply(subData,  "homeState",  function(df) table(df$Condition))
    table2$primeN = tempN$P
    table2$noPrimeN = tempN$NP
    # note that we will use Bonferroni correction. .05/50 comparisons = .001
    
    # write  custom function to grab t,  df,  and p from comparing the two conditions
    tStats = function(df){
      tResults = t.test(df$criticalQuestion ~ df$Condition, alternative = "greater")
      result = c(tResults$statistic, tResults$parameter, tResults$p.value)
      return(result)
    }
    
    # then calculate t test results
    tempT = ddply(subData,  "homeState",  tStats)
    names(tempT)[4] = "p"
    
    # save results to table2
    table2 = cbind(table2, tempT[, 2:4]) # grab relevant rows from tempT
    
    # finally,  calculate cohensD
    tempES = ddply(subData,  "homeState",  function(df)cohensD(criticalQuestion~Condition, data=df))
    names(tempES)[2] = "CohensD"
    table2$effect.size = tempES$CohensD
    
    # clean up numbers
    table2[, c(2:9, 11)] = round(table2[, c(2:9, 11)], 2)
    table2[, 10] = round(table2[, 10], 3)
    
    # note significant values
    table2$sig = "not significant"
    table2[which(table2$p <= .001), "sig"]= "sig"
    
    # save table
    write.csv(table2,  "table2.csv")
    
#  * * * * * * * *  # 4 Calculate the 100% Response and compare to standard version of the critical question * * * * * * * * 
    # first do descriptives
    averageCQ
    
    # look at data with all responses included (but note that some subjects may have listed their home state as one of the most important states)
    state100All = mean(finalData$homeState100Percent)
    state100All
    state100All.Stats = round(stat.desc(finalData$homeState100Percent,  norm = TRUE), 2)
    state100All.Stats
    state100All - state100All.Stats[["CI.mean.0.95"]]
    state100All + state100All.Stats[["CI.mean.0.95"]]
    sum(stateData$State100PercentMean.ALL) # total of the states when using the 100% version of the question
    
    # and run a t-test
    t.test(finalData$criticalQuestion, finalData$homeState100Percent, paired = TRUE)
    cohensD(finalData$criticalQuestion, finalData$homeState100Percent, method = "paired")
  
    # now run same analysis but after excluding subjects who listed their homestate as one of the three most important states
    state100.NoDupes = mean(stateData$State100PercentMean)
    state100.NoDupes
    state100.NoDupes.Stats = round(stat.desc(stateData$State100PercentMean, norm = TRUE), 2)
    state100.NoDupes.Stats 
    state100.NoDupes - state100.NoDupes.Stats[["CI.mean.0.95"]]
    state100.NoDupes + state100.NoDupes.Stats[["CI.mean.0.95"]]
    sum(stateData$State100PercentMean)
    
      
    # and finally run same analysis but only excluding subjects who did not follow instructions (i.e. they listed homestate 2 or more times
      # AND they rated it more than once)
    state100.NoFail = mean(stateData$noFail100Mean)
    state100.NoFail
    state100.NoFail.Stats = round(stat.desc(stateData$noFail100Mean, norm = TRUE), 2)
    state100.NoFail.Stats
    state100.NoFail - state100.NoFail.Stats[["CI.mean.0.95"]]
    state100.NoFail + state100.NoFail.Stats[["CI.mean.0.95"]]
    sum(stateData$noFail100Mean)

#  * * * * * * * *  Narcisissistic Index Analyses & Specific states * * * * * * * * 
# we predicted that Texas,  New York,  Virginia,  Massachusetts,  California and South Carolina would be the most narcissiticc.
# we also predicted that any state that is a 13 colony will have a high rating
# we'll do three sets of predictions (first just our target states, then 13 colonies, then all predicted states). Only the last is reported in the manuscript.

# Note that the narcissistic index is equivalent to Mdiff for Resident & Non Resident
    
# First set of predictions
predictedStates = c("Texas", "New York",  "Virginia",  "Massachusetts",  "California",  "South Carolina")
nIndex = table1[, c(1, 6, 2, 4)]
names(nIndex) = c("state", "narIndex", "Rrating", "NRrating")

  # tag relevant states
  nIndex[nIndex$state %in% predictedStates, "predicted"] = "predicted" # tag states that you predicted
  nIndex[is.na(nIndex$predicted), "predicted"] = "control" # tag the other states with the opposite

  # calculate means for resident ratings for predicted and not-predicted states
  tapply(nIndex$Rrating, nIndex$predicted, mean)
  tapply(nIndex$Rrating, nIndex$predicted, sd)
  tapply(nIndex$Rrating, nIndex$predicted, stat.desc)
  
  # test to see if groups are different on CQ
  t.test(Rrating ~ predicted,  data = nIndex,  alternative = "less") # predicted states > non predicted
  cohensD(Rrating ~ predicted,  data = nIndex)
  
  # calculate means for each group for narcisissic index
  tapply(nIndex$narIndex, nIndex$predicted, mean)
  tapply(nIndex$narIndex, nIndex$predicted, sd)
  tapply(nIndex$narIndex, nIndex$predicted, stat.desc)
  
  # conduct a t-test to see if they are different
  t.test(narIndex ~ predicted,  data = nIndex,  alternative = "less") # no significant difference
  cohensD(narIndex ~ predicted,  data = nIndex)
      
# just 13 colony predictions
colonies = c("Connecticut", "Delaware", "Georgia", "Maryland", "Massachusetts", "New Jersey", "New York", "New Hampshire", "North Carolina", "Pennsylvania", "Rhode Island", "South Carolina", "Virginia")

# tag colonies
nIndex[nIndex$state %in% colonies,  "colony"] = "colony"
nIndex[is.na(nIndex$colony), "colony"] = "new state"

  # calculate means for resident ratings
  tapply(nIndex$Rrating, nIndex$colony, mean)
  tapply(nIndex$Rrating, nIndex$colony, sd)
  tapply(nIndex$Rrating, nIndex$colony, stat.desc)

  # test to see if groups are different on CQ
  t.test(Rrating ~ colony,  data = nIndex,  alternative = "greater") # colonies > new states
  cohensD(Rrating ~ colony,  data = nIndex)

  # calculate narIndex means for each group
  tapply(nIndex$narIndex, nIndex$colony, mean)
  tapply(nIndex$narIndex, nIndex$colony, sd)
  tapply(nIndex$narIndex, nIndex$colony, stat.desc)

  # conduct a t-test to see if they are different on narIndex
  t.test(narIndex ~ colony,  data = nIndex,  alternative = "greater") # colonies > new states
  cohensD(narIndex ~ colony,  data = nIndex)


# 3 all predicted states (as noted in preregistration)
allPredicted = c(colonies,  "Texas",  "California")

  # tag all predicted
  nIndex[nIndex$state %in% allPredicted,  "allPredict"] = "narcissist"
  nIndex[is.na(nIndex$allPredict), "allPredict"] = "modest"

  # calculate means for resident ratings
  tapply(nIndex$Rrating, nIndex$allPredict, mean)
  tapply(nIndex$Rrating, nIndex$allPredict, sd)
  tapply(nIndex$Rrating, nIndex$allPredict, stat.desc)

    # calculate 95% CIs for values
    14.73 + 1.20 # modest
    14.73 - 1.20 # modest
    26.13 + 3.66 # narc
    26.13 - 3.66 # narc
  
  # test to see if groups are different on CQ
  t.test(Rrating ~ allPredict,  data = nIndex,  alternative = "less") # narc > non-narc
  cohensD(Rrating ~ allPredict,  data = nIndex)

  # calculate means for each group for narc Index
  tapply(nIndex$narIndex, nIndex$allPredict, mean)
  tapply(nIndex$narIndex, nIndex$allPredict, sd)
  tapply(nIndex$narIndex, nIndex$allPredict, stat.desc)
  
    # calculate 95% CIs for values
    5.38 + 1.11 # modest 
    5.38 - 1.11 # modest
    9.49 + 2.55 # narc
    9.49 - 2.55 # narc

  
  # conduct a t-test to see if they are different on narIndex
  t.test(narIndex ~ allPredict,  data = nIndex,  alternative = "less") # Narc > non-narc
  cohensD(narIndex ~ allPredict,  data = nIndex)

# * * * * * * * *  SUBJECT COUNTS OF STATES FOR EXPLORATORY QUESTIONS  * * * * * * * * 
stateList2 = data.frame(stateData$state) # save list of states
names(stateList2)[1] = "state"

# for each question,  save table as DF and rename
ContMost = as.data.frame(table(finalData$contributedMostState))
names(ContMost) = c("state", "contMostCount")

Cont2 = as.data.frame(table(finalData$contributed2ndMostState))
names(Cont2) = c("state", "cont2Count")

Cont3 = as.data.frame(table(finalData$contributed3rdMostState))
names(Cont3) = c("state", "cont3Count")

ContLeast = as.data.frame(table(finalData$contributedLeastMost))
names(ContLeast) = c('state', 'contLeastCount')

MostSway = as.data.frame(table(finalData$mostSway))
names(MostSway) = c('state', 'mostSwayCount')

MostNarc = as.data.frame(table(finalData$egocentricPrediction))
names(MostNarc) = c('state', 'mostNarcCount')

# combine dataframes
countDF = join_all(list(stateList2, ContMost, Cont2, Cont3, ContLeast, MostSway, MostNarc),  by = "state")

# replace NA's with 0
countDF[is.na(countDF)] = 0

# reorder in alpha order
countDF = countDF[order(countDF$state), ]

# add total counts for Contributed Most
countDF$totalInfluentialCounts = countDF$contMostCount + countDF$cont2Count + countDF$cont3Count

# save count table (votes on which states contributed most, least, etc.)
write.csv(countDF,  "countTable.csv")

# * * * * * * * * CALCULATE 95% CI Interval for Narc.Index Differences for each state* * * * * * * * 
# grab data necessary to calculate 95% CI
diffData = ddply(ttestData, "state", function(x){ c(
  tapply(x$criticalQuestion, x$Residency, mean), 
  tapply(x$criticalQuestion, x$Residency, length), 
  tapply(x$criticalQuestion, x$Residency, var)
  )})

#calculate descriptive statistics on the number of ratings for the non-residents
round(stat.desc(diffData$NonResident, basic = TRUE),2)

diffData$Narc.Index = diffData$Resident - diffData$NonResident
diffData
names(diffData) = c("state", "NonResident.Rating", "Resident.Rating", 
                    "NonResident.N", "Resident.N", "NonResident.Var", 
                    "Resident.Var", "Narc.Index")

# calculate pooled standard deviation (formula from Cummings, 2012)
diffData$pooledS = sqrt(((diffData$Resident.N-1)*diffData$Resident.Var+(diffData$NonResident.N-1)*diffData$NonResident.Var)/(diffData$Resident.N + diffData$NonResident.N - 2))

# caculated MOE
diffData$MOEdiff = qt(.975, diffData$Resident.N + diffData$NonResident.N - 2) * diffData$pooledS*sqrt((1/diffData$Resident.N)+(1/diffData$NonResident.N))
diffData$ll = diffData$Narc.Index - diffData$MOEdiff
diffData$ul = diffData$Narc.Index + diffData$MOEdiff

# save 95% CI 
write.csv(diffData,  "narcIndex.csv")

# * * * * * * * *  GENERATE HEAT MAPS  * * * * * * * * 
# load data
SN.data = diffData[,c(1:3,8,11,12)]
data("fifty_states")

# lowercase states
SN.data$state = tolower(SN.data$state)
head(SN.data)

# load central coordinate data
centralcoords <- read.csv("centralcoords.csv")
centralcoords <- centralcoords[centralcoords$State!="District of Columbia",]
centralcoords$state = tolower(centralcoords$State)
centralcoords <- centralcoords[,-1] #take out upper case state column
centralfifty <- merge(SN.data, centralcoords, all.x=T) #merge rating data with coordinate data

# create rounded resident rating column with percents
centralfifty$Rrat.perc <- round(centralfifty$Resident.Rating, 0) #round resident ratings
centralfifty$Rrat.perc <- as.numeric(centralfifty$Rrat.perc)
centralfifty$Rrat.perc <- paste(centralfifty$Rrat.perc, "%", sep="") #add percent after numbers

# create rounded narcisstic index column with percents
centralfifty$NarcIndex.perc <- round(centralfifty$Narc.Index, 0)
centralfifty$NarcIndex.perc <- as.numeric(centralfifty$NarcIndex.perc)
centralfifty$NarcIndex.perc <- paste(centralfifty$NarcIndex.perc, "%", sep="")

# create dataframe for line segments on east coast
linepoints.df = data.frame(x=c(-75.607910,-71.619873, -74.443359,-72.169189, -76.827393, -71.477051, -72.729492, -72.652588), y=c(39.276119,41.682400, 40.140565,42.427332, 38.669696, 43.890051, 44.298954, 41.665987), xx=c(-71.607910,-68.619873, -70.443359,-70.169189, -73.527393, -71.477051, -72.729492, -71.542969), yy=c(39.276119,41.682400, 40.140565,42.427332, 38.669696, 45.790051, 46.498954, 41.014361))
# order: Delaware, Rhode Island, New Jersey, Massachusetts, Maryland, New Hampshire, Vermont, Connecticut

# **FINAL MAPS**

## MAP OF NARCISSISTIC INDEX (FILL AND NUMBERS)

statemapNI <- ggplot(SN.data, aes(map_id = state)) +
  geom_map(aes(fill = Narc.Index), map = fifty_states) + 
  scale_fill_distiller(palette = "Spectral")+ # full color spectrum from blue to red 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", fill = "Narcissistic Index", title = "Narcissistic Index by State") +
  theme(legend.position = "bottom", panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + # move title to center
  borders("state", colour = "white") + # adds white border - needs to go before text and lines
  geom_text(data = , aes(centralfifty$Longitude, centralfifty$Latitude, label = centralfifty$NarcIndex.perc), size = 3) + # adds NI numbers on top
  geom_segment(data = linepoints.df, mapping = aes(x = x, y = y, xend = xx, yend = yy), size = .3, color = "black", inherit.aes = FALSE) #adds line segments
statemapNI + fifty_states_inset_boxes()


## MAP OF RESIDENT RATINGS (FILL AND NUMBERS)

statemapRR <- ggplot(SN.data, aes(map_id = state)) +
  geom_map(aes(fill = Resident.Rating), map = fifty_states) + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide ="colorbar") + # red scale coloring
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", fill = "Resident Rating", title = "Resident Ratings by State" ) +
  theme(legend.position = "bottom", panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) + # center title
  borders("state", colour = "white") + # adds white border 
  geom_text(data = , aes(centralfifty$Longitude, centralfifty$Latitude, label = centralfifty$Rrat.perc), size = 3) + # adds resident ratings on top
  geom_segment(data = linepoints.df, mapping = aes(x = x, y = y, xend = xx, yend = yy), size = .3, color = "black", inherit.aes = FALSE) # adds line segments
statemapRR + fifty_states_inset_boxes()

## * * * * * * * *  SUPPLEMENTARY ANALYSES * * * * * * * * 


## Create Histograms Showing the Distribution of Ratings for Individual States
  histData = finalData[ , c("homeState","criticalQuestion")] # save just the data we need to create histograms

  # check histogram for all states
  ggplot(histData, aes(x = criticalQuestion)) + geom_histogram(binwidth = 10, fill = "white", color= "black", boundary = 0)

  # create histogram for each state and save multiple graphs on each page
  pdf("AllStatesFormatted.pdf", 8, 10.5) # This becomes figure S1 in the manuscript
  for(i in seq(1, length(unique(histData$homeState)), 10)) {
   print(ggplot(histData[histData$homeState %in% levels(histData$homeState)[i:(i+9)], ],
                aes(x = criticalQuestion)) +
                geom_histogram(binwidth = 10, boundary = 0, fill = "white", color = "black") +
                facet_wrap(~ homeState, ncol = 2) +
                xlab("Percentage Estimate of State's Contribution") + 
                ylab("Frequency") +
                ggtitle("Distribution of Responses to Critical Question for Each State") +
                coord_cartesian(xlim = c(0,100), ylim = c(0,50)) +
                theme(plot.title = element_text(hjust = 0.5))
          )
  }
  dev.off()

  # split by state and save as .PDF but just for the 15 states with the highest values
  orderedState = stateData[order(-stateData$Rrating), ]
  top15List = orderedState[1:15,"state"]
  top15List = droplevels(top15List)

  top15Data = subset(histData, homeState %in% top15List)
  
  # histogram of top 15 states
  ggplot(top15Data, aes(x = criticalQuestion)) + geom_histogram(binwidth = 10, fill = "white", color= "black", boundary = 0)
  
  # generate histogram for each state, but put multiple graphs on a page
  ggplot(top15Data, aes(x = criticalQuestion)) + 
    geom_histogram(binwidth = 10, fill = "white", color = "black", boundary = 0)  + 
    facet_wrap(~ homeState, ncol = 3) + 
    xlab("Percentage Estimate of State's Contribution") + 
    ylab("Frequency") +
    ggtitle("Distribution of Responses for the 15 States With the Highest Resident Ratings")

##  * * * * * * * * CHECK RELIABILITY OF DATA BY DOING SPLIT HALF ANALYSIS* * * * * * * * 
# first grab the relevant data
splitData = finalData[ , c("subjectNumber", "homeState", "criticalQuestion")]

# split half reliability for all subjects
splitData$oddEvenTag = c("odd","even") # add tag
tapply(splitData$criticalQuestion,splitData$oddEvenTag,mean)
oddData = subset(splitData, oddEvenTag == "odd")
evenData = subset(splitData, oddEvenTag == "even")

# # calculate the average for each state
# oddAverages = ddply(oddData, "homeState", function(a)c(mean(a$criticalQuestion), nrow(a)))
# evenAverages = ddply(evenData, "homeState", function(a)c(mean(a$criticalQuestion), nrow(a)))
# 
# # combine dataframes
# splitDF = cbind(oddAverages,evenAverages)
# splitDF = splitDF[ , -4]
# names(splitDF) = c("homeState", "oddAverage", "oddN", "evenAverage", "evenN")
# 
# # check correlation for all states
# cor(splitDF$oddAverage, splitDF$evenAverage)
# cor.test(splitDF$oddAverage, splitDF$evenAverage)
# # low correlation, but no reason to suspect that there would be a high correlation

## split each state into two groups and then calculate stats for each split half
splitResults = ddply(splitData, "homeState", function(x){
  half1 = x[seq(from = 1, to = nrow(x) -1, by = 2), ] # grab odd numbered subjects
  half2 = x[seq(from = 2, to = nrow(x), by = 2), ] # grab even numbered subjects
  half1Mean = mean(half1$criticalQuestion) 
  half2Mean = mean(half2$criticalQuestion)
  half1N = nrow(half1)
  half2N = nrow(half2)
  data.frame(half1Mean,half2Mean,half1N,half2N)
})
splitResults

#caculate pearson correlation
cor(splitResults$half1Mean, splitResults$half2Mean)

## calculate split half reliability for the non resident ratings
nonResSplitResults = data.frame(State = character(), half1Mean = double(), 
                             half2Mean = double(), half1N = integer(), half2N = integer()) # build a blank data frame to hold results

for (i in 42:91){ # grab the non resident rating columns
  temp = is.na(finalData[ ,i]) # identify which ones are NA
  temp2 = finalData[!temp, i] # grab the opposites
  half1 = temp2[seq(from = 1, to = length(temp2) -1, by = 2)] # grab odd numbers
  half2 = temp2[seq(from = 2, to = length(temp2), by =  2)] # grab even numbers
  half1Mean = mean(half1)
  half2Mean = mean(half2)
  half1N = length(half1)
  half2N = length(half2)
  nonResSplitResults = rbind.data.frame(nonResSplitResults, c(half1Mean, half2Mean, half1N, half2N))
}
nonResSplitResults

# organize data frame
nonResSplitResults$homeState = stateList
nonResSplitResults = nonResSplitResults[ ,c(5,1:4)]
names(nonResSplitResults) = names(splitResults)

cor(nonResSplitResults$half1Mean,nonResSplitResults$half2Mean)
# report the results of this correlation.

## * * * * * * * * Calculate the accuracy for each question on the history quiz * * * * * * * * 
# grab relevant data
quizData = finalData[ , c(26:40, 4, 162)]

#import questions
quizQs = c("Which ally during the Revolutionary War was most instrumental to United States victory?",
           "Which of the following tax acts was not passed during the time leading up to or during the Revolutionary War?", 
           "What were the first ten amendments to the constitution called?",
           "Which of the following politicians was not a US president?",
           "Which president was an enthusiastic supporter the National Park Service, doubling the number of national parks?",
           "President Thomas Jefferson doubled the size of the United States through the Louisiana Purchase which involved buying land from which country?",
           "Which Supreme Court case declared that states cannot ban slavery?",
           "When was the Civil War?",
           "The United States entered World War I in what year?",
           "Which amendment granted women suffrage (the right to vote)?",
           "The Great Depression began with the crash of the stock market in what year?",
           "Which foreign power was NOT opposed to the U.S. in World War II?",
           "The Vietnam War was fought to stop the spread of what form of government?",
           "What year did the United States win the space race, when Neil Armstrong stepped foot on the moon?",
           "In 1981, hostages were released after being held for 444 days from what Middle Eastern country?")

#import correct answers
quizAnswers = c("France",	"Howe Tariffs",	"The Bill of Rights",	"Alexander Hamilton",
                "Theodore Roosevelt",	"France", "Dred Scott v. Sanford",	"1861-1865",	
                "1917",	"19th",	"1929",	"Russia",	"Communism",	"1969",	"Iran")

# double check n (should be 2,898)
nrow(finalData)

# number quiz questions
qNums = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15")

# update names for the items
names(quizData)[1:15] = paste(qNums, names(quizData)[1:15], sep = ".")

# or better, just replace names with numbers
names(quizData)[1:15] = qNums

# build holder for accuracy
quizAccuracy = numeric()

# loop through quizData and calculate the average correct for each question (2898 is total sample size)
for (i in 1:15){
  quizAccuracy[i] = (sum(quizData[ ,i] == quizAnswers[i])) / 2898
  quizAccuracy[i] = round(quizAccuracy[i], 2)
}

# combine the questions, answers, and accuracy into a single DF
quizQA = as.data.frame(cbind(quizQs, quizAnswers, quizAccuracy))

# write table
write.csv(quizQA, "table4.csv")

