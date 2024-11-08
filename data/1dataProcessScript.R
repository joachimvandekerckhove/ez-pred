########################################### 
#       This is script to look at         
#       demographics data along with      
#       creating merged dataset with      
#       cognitive scores and covariates   
###########################################

########################################### 
# SET-UP. Prep R environment & data files   
###########################################

# *****************************************
# Fresh new working environment
graphics.off() # closing windows
rm(list = ls())  # clearing memory

# *****************************************
# Loading packages
library(dplyr)
library(psych)
library(ggplot2)
library(patchwork)
library(rstudioapi)
library(naniar)
library(summarytools)


# *****************************************
# Set working directory to this current file's location
current_path <- getSourceEditorContext()$path
setwd(dirname(current_path))

# *****************************************
# Read in 2 dataset: demographics and cognitive
demData <- read.csv("Demographics_n69_2023-11-04-T10-10-43.csv") #Demographics dataset
demData <- demData[,-1] #Removes first X column
demData <- filter(demData, participant_id != "4039") # Exclude 4039 who only had 9 trials all from version WD / ST- / RT+ / PC-

cogData <- read.csv("Color_Shapes_Trial_Level_Parsed_n69_2023-11-04-T10-10-42.csv", na.strings = "-999") #Cognitive dataset
cogData <- cogData[,-1] #Remove first X column
cogData <- filter(cogData, participant_id != "4039") # Exclude 4039 who only had 9 trials

# Create new folder to hold output & set as working directory 
dirName = sprintf("1DataProcess%s", Sys.Date())
dir.create(dirName)
setwd(dirName)

# *****************************************
# Identify and code multi-ethnic folks in demographics data

# Search for "+" in race variable, indicating more than 1 category selected
multiRaceList <- grep("+", demData$race, fixed = TRUE) # This prints row number, stored in vector
multiRaceList
# Glimpse of subset to check
multiRaceDat = demData[multiRaceList, ]

# Creating new race variable to recode
demData$raceNew <- demData$race

# Recode multiracial
demData2 = demData %>% 
  mutate(raceNew = ifelse(participant_id %in% multiRaceDat$participant_id, 6, race))

demData = demData2

# *****************************************
# Merging demData and cogData matched by PID                    

demDatOrdered <- arrange(demData, participant_id, .by_group = TRUE)
cogDatOrdered <- arrange(cogData, participant_id, task_version, .by_group = TRUE)

mergeDat <- merge(demDatOrdered, cogDatOrdered, by = "participant_id", all =TRUE)
mergeOrdered <- arrange(mergeDat, participant_id, task_version, .by_group = TRUE)

# *****************************************
# Data descriptives: Identify factors in data

# Factor race, ethnicity, sex, education as factors
demData$raceNew <- factor(demData$raceNew, levels = 0:6, labels = c("White", "Black/AA", "Asian", "American Native/Alaska Native", "Native Hawaiian/Pacific Islander",
                                                                    "Other", "Multi-ethnic"))

demData$ethnicity <- factor(demData$ethnicity, levels = 0:1, labels = c("Not Hispanic", "Hispanic"))

demData$sex <- factor(demData$sex, levels = 0:2, labels = c("Female", "Male", "Other"))

demData$education <- factor(demData$education, levels = 0:8, labels = c("Elementary", "Middle school", "High school", "Vocational training/Some college",
                                                                        "Associate's degree", "Bachelor's degree", "Post-bachelor schooling", "Master's degree",
                                                                        "Doctoral degree"))
describe(demData)
hist(demData$covid)


########################################### 
# PART 1. Demo data visualizations and summary
###########################################

# *****************************************
# Create frequency tables

# Race
table(demData[,"raceNew"], useNA = 'always')
# Ethnicity (Hispanic or non-Hispanic)
table(demData[,"ethnicity"], useNA = 'always')
# Sex
table(demData[, "sex"], useNA = 'always')
# Education
table(demData[, "education"], useNA = 'always')

# Proportions tabless
propRace <- with(demData, table(raceNew)) %>% prop.table(); round(propRace,2)
propEth <- with(demData, table(ethnicity)) %>% prop.table(); round(propEth,2)
propSex <- with(demData, table(sex)) %>% prop.table(); round(propSex,2)
propEdu <- with(demData, table(education)) %>% prop.table(); round(propEdu,2)

# *****************************************
# Demographics plots

# Race/Ethnic identity bar plot
pRace2 <- ggplot(demData, aes(x = raceNew)) +
  geom_bar(aes(fill = raceNew)) + 
  labs(title = "Race/Ethnicity frequency", 
       x = "Race/Ethnic identity", 
       y = "Count") + 
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
pRace2

# Ethnicity bar plot
pEthn2 <- ggplot(demData, aes(x = ethnicity)) +
  geom_bar(aes(fill = ethnicity)) + 
  labs(title = "Ethnicity frequency",
       x = "Ethnicity",
       y = "Count") + 
  theme_minimal() + 
  theme(legend.position = "none") 
pEthn2

# Sex/Gender bar plot
pSex <- ggplot(demData, aes(x = sex)) +
  geom_bar(aes(fill = sex)) + 
  labs(title = "Sex frequency",
       x = "Sex",
       y = "Count") +
  theme_minimal() + 
  theme(legend.position = "none")
pSex

# Education bar plot
pEdu2 <- ggplot(demData, aes(x = education)) +
  geom_bar(aes(fill = education)) + 
  labs(title = "Education frequency",
       x = "Education",
       y = "Count") + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
pEdu2

pAge <- ggplot(demData, aes(x = age)) +
  geom_density(fill="gray") + 
  labs(title = "Age",
       x = "Age (years)",
       y = "Density") + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  xlim(x = c(18,90))
pAge

# Export patchwork of all plots in 1 image as png
png(filename = "sampleDemoPlot.png", width = 1000, height = 700, units = "px")
(pAge + pSex + pEthn2) / (pRace2 + pEdu2)
graphics.off()

###################################################################
###################################################################
###################################################################

########################################### 
# PART 2. Prep merged data for analysis   #
###########################################

# Working with merged data from above
data = mergeDat

# Relabel 'task_version'
names(data)[11] <- 'version'

# Check for 16 conditions
sort(unique(data$version))

# Coding task_time as numeric
data$task_time <- as.numeric(data$task_time)

# Convert character version into numeric factor
data$versionF <- as.factor(data$version)

oldvals <- c("SP_ST-_RT-_PC-", "SP_ST-_RT-_PC+", 
             "SP_ST-_RT+_PC-", "SP_ST-_RT+_PC+", 
             "SP_ST+_RT-_PC-", "SP_ST+_RT-_PC+",
             "SP_ST+_RT+_PC-", "SP_ST+_RT+_PC+", 
             "WD_ST-_RT-_PC-", "WD_ST-_RT-_PC+", 
             "WD_ST-_RT+_PC-", "WD_ST-_RT+_PC+",
             "WD_ST+_RT-_PC-", "WD_ST+_RT-_PC+", 
             "WD_ST+_RT+_PC-", "WD_ST+_RT+_PC+")

newvals <- c(seq(1:16))
data$versionCode <- newvals[match(data$versionF, oldvals)]

head(data[,c("version", "versionCode")])

# Check for 16 condtions
sort(unique(data$versionCode))

# *****************************************
# Code conditions

##### Notes about conditions:
#Cond1 = Study Time -> drift (delta) manipulation, 0: sT- (500ms) vs 1: ST+ (2000ms)  
#Cond2 = Prob of Change -> initial bias (beta) manipulation, 0: PC- (50-50), 1: PC+ (20-80)  
#Cond3 = Reaction Time -> threshold (alpha) manipulation: 0: RT+ (500s), 1: RT- (3s)  
#Cond4 = Probe Type -> 0: whole display, 1: single probe - this is not an DDM manipulation  
#SameOrDifferent = 1: same, 2: different  

# Creating dummy variables for 4 feature manipulation
# Study time condition
data$StudyTime <- substring(data$version, 4, 6)
data$StudyTimeCoded <- dplyr::recode(data$StudyTime, "ST-" = 0, "ST+" = 1)

# Choice urgency condition (Relabeled from ReactionTime)
data$ChoiceUrgency <- substring(data$version, 8, 10)
data$ChoiceUrgencyCoded <- dplyr::recode(data$ChoiceUrgency, "RT+" = 0, "RT-" = 1)

# Probability of change condition
data$ProbOfChange <- substring(data$version, 12, 14)
data$ProbOfChangeCoded <- dplyr::recode(data$ProbOfChange, "PC-" = 0, "PC+" = 1)

# Probe Type Condition
data$ProbeType <- substring(data$version, 1, 2)
data$ProbeTypeCoded <- dplyr::recode(data$ProbeType, "WD" = 0, "SP" = 1)

# *****************************************
##### Number of trials per participant

pList <- sort(unique(data$participant_id)); pList
pNo <- length(pList); pNo
trialList <- array(NA, dim = c(length(pList), 2))
trialList[ ,1] <- pList
pIndex <- cbind(pList, seq(1:pNo)); pIndex

for (i in 1:pNo){
  trialList[i,2] <- length(data$participant_id[data$participant_id == paste(pList[i])])
  #print(paste0("Participant ", trialList[i,1], " has ", trialList[i,2], " trials."))
}

# Prints table with participant id and number of trials
trialList

# *****************************************
##### Exploring participants who completed less than 960 trials

# Minimum number of trials completed
min <- min(trialList[,2]); min

# Participant with lowest trial completed
minList <- which(trialList[,2] == min)
pIndex[minList, ] 

# List of participants with less than 960 completed trials
lessList <- which(trialList[,2] < 960); lessList 
length(lessList) # How many had less than 960 trials
lessTrials <- cbind(pIndex[lessList, ], trialList[lessList,2]);
lessTrials[order(lessTrials[ ,3]), ] # Summary table, sorted based on number of trials completed

length(lessList) # Number of people with less trials

# *****************************************
# Storing full data separately, just in case needed
mergedFullData <- data

# *****************************************
# Check for overall data missingness

# Check using naniar package (for response_time and button_press)
any_na(mergedFullData) # Any NA in data
n_miss(mergedFullData) # Number of NA in data
prop_miss(mergedFullData) # Proportion of NA
NAList <- mergedFullData %>% is.na() %>% colSums() # Prints NA in each variable
NAList # Shows all variables NA trial counts
NAList[NAList > 0] #Shows only variables with any NA

# Summary of NA in each variable
miss_var_summary(data) #summary
miss_var_table(data) #tabulated summary

# Summary of NA per participant
miss_case_summary(data) #summary
miss_case_table(data) #tabulated summary

# 3 different visualization of missingness in data
gg_miss_var(data)
vis_miss(data, warn_large_data = FALSE) + theme(axis.text.x = element_text(angle = 80))
gg_miss_upset(data)

# Check for data missingness in button_pressed and response_time variables
NAList["button_pressed"]
NAList["response_time"]

# *****************************************
# Explore trial data of potential extreme response times
# Extreme RTs are faster than 200ms and slower than 7000ms

##### Checking for low RT (too fast)

tooFast <- filter(mergedFullData, response_time < 200) #filters out RTs<200ms
tooFast
lowRTPNo <- length(unique(tooFast$participant_id)) #number of people with too fast trials
lowRTPNo
lowRTPID <- unique(tooFast$participant_id[]) #their participant_id number
lowRTPID

lowRTMatrix <- array(NA, dim = c(lowRTPNo, 2)) #create matrix to summarise info
lowRTMatrix[,1] <- lowRTPID

for (i in 1:lowRTPNo) {
  list <- filter(tooFast, participant_id == paste(lowRTPID[i]))
  lowRTMatrix[i,2] <- length(list[,12])
}

# 1st column shows ID number and 2nd column shows number of low RT trials
lowRTMatrix

##### Checking for long RT (too slow)

# Filter out too slow RT
tooSlow <- filter(mergedFullData, response_time > 7000) #filters out RTs>7000ms
tooSlow

highRTPNo <- length(unique(tooSlow$participant_id)) #number of people with too slow trials
highRTPNo
highRTPID <- unique(tooSlow$participant_id[]) #their participant_id number
highRTPID

highRTMatrix <- array(NA, dim = c(highRTPNo, 2)) #create matrix to summarise info
highRTMatrix[,1] <- highRTPID

for (i in 1:highRTPNo) {
  list <- filter(tooSlow, participant_id == paste(highRTPID[i]))
  highRTMatrix[i,2] <- length(list[,12])
}

# Shows summary of participants with too slow trials by 
# 1) participant_id order, 
# 2) number of too slow trials (descending order)
# 1st column shows ID number and 2nd column shows number of low RT trials
highRTMatrix[order(highRTMatrix[,1]),]
highRTMatrix[order(highRTMatrix[,2], decreasing = TRUE),]

# *****************************************
###### Deeper look at too fast and too slow RT trials

# Subset trials with too fast RT
tooFastOnly <- data %>%
  filter(response_time < 200)

# Subset trials with too slow RT
tooSlowOnly <- data %>%
  filter(response_time > 7000)

# Descriptives of both
describe(tooFastOnly)
describe(tooSlowOnly)

# Histogram
plotTooFastRT <- ggplot(tooFastOnly, aes(x = response_time)) + geom_histogram(bins = 10, fill = "gold", color = "black") + labs(title = "Too fast excluded trials (RTs<200ms)", x = "Response time (ms)", y = "Count (trials)") + ylim(0 , 25)

# Histogram
plotTooSlowRT <- ggplot(tooSlowOnly, aes(x = response_time)) + geom_histogram(fill = "lightblue", bins = 10, color = "black") + labs(title = "Too slow excluded trials (RTs>7000ms)", x = "Response time (ms)", y = "Count (trials)") + ylim(0 , 25)

# Exports patchwork plot of filtered trials as png
png(filename = "plot.filteredRTTrials.png", width = 1800, height = 1000, units = "px", res=200)
plotTooFastRT + plotTooSlowRT
dev.off()

# *****************************************
# Raw number of trials
nrow(data) 

# Filtered data, exclude trials greater than 7000 ms & less than 200 ms
fData1 <- data %>%
  filter(response_time < 7000)
fData2 <- fData1 %>%
  filter(response_time > 200)

nrow(fData2) # Number of trials in filtered data
nrow(data) - nrow(fData2) #number of trials were excluded from raw data

# *****************************************
##### Explore mean response time data using filtered data

#Using filtered data
data <- fData2

describe(data) #descriptives
describeBy(data, group = data$ProbeType) #descriptives grouped by probe type

WDgroup <- filter(data, ProbeTypeCoded == 0) #subset whole display trials
SPgroup <- filter(data, ProbeTypeCoded == 1) #subset single probe trials

plotWDRT = ggplot(WDgroup, aes(x = response_time)) + 
  geom_histogram(color = "black", fill = "white", bins = 15) +
  labs(title = "Response times in whole display", x = "Response times (ms)", y = "Count (trials)")


plotSPRT = ggplot(SPgroup, aes(x = response_time)) + 
  geom_histogram(color = "black", fill = "white", bins = 15) +
  labs(title = "Response times in single probe", x = "Response times (ms)", y = "Count (trials)")

mean(WDgroup$response_time) #mean RT in WD trials
mean(SPgroup$response_time) #mean RT in SP trials

# Density plot of trial RT between WD (red color) and SP (green color) trials
plotMeanRTbyProbe <- ggplot(data, aes(x = response_time)) + geom_density(data = WDgroup, fill = "#FF6699", color = "black", alpha = 0.4) + geom_density(data = SPgroup, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw response times between probe type", 
       subtitle = "Red = WD, Green = SP",
       x = "Response times (ms)",
       y = "Count (trials)") + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotMeanRTbyProbe

# Bar plot of trial accuracy between WD (red color) and SP (green color) trials
plotMeanAccbyProbe <- ggplot(data, aes(x = factor(accuracy, levels = 0:1, labels = c("Incorrect", "Correct")))) + geom_bar(data = WDgroup, fill = "#FF6699", color = "black", alpha = 0.4) + geom_bar(data = SPgroup, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw accuracy between probe type", 
       subtitle = "Red = WD, Green = SP",
       x = "Accuracy",
       y = "Count (trials)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotMeanAccbyProbe


##### Exports patchwork summary of mean RT and accuracy between probe types
png(filename = "plot.meanRTandAccuracyByProbe.png", width = 1800, height = 1000, units = "px", res = 200)
plotMeanRTbyProbe + plotMeanAccbyProbe
dev.off()


#####
# Creates cross tabulations for accuracy (whole display)
freq(WDgroup$accuracy,
     prop = "r", # row proportions
     method = "render")

# Simpler table
freq(WDgroup$accuracy, 
     report.nas = FALSE, 
     totals     = FALSE, 
     cumul      = FALSE, 
     headings   = FALSE)


# Creates cross tabulations for accuracy (single probe)
freq(SPgroup$accuracy,
     prop = "r", # row proportions
     method = "render")

# Simpler table
freq(SPgroup$accuracy, 
     report.nas = FALSE, 
     totals     = FALSE, 
     cumul      = FALSE, 
     headings   = FALSE)


# Computing proportion of accuracy in WD group
tab <- table(WDgroup$accuracy); tab
# Proportion of incorrect
tab[[1]]/(tab[[1]]+tab[[2]])
# Proportion of correct
tab[[2]]/(tab[[1]]+tab[[2]])

# Computing proportion of accuracy in SP group
tab <- table(SPgroup$accuracy); tab
# Proportion of incorrect
tab[[1]]/(tab[[1]]+tab[[2]])
# Proportion of correct
tab[[2]]/(tab[[1]]+tab[[2]])

###################################################################
# Explore response time and accuracy by conditions

describeBy(data, group = data$versionCode) #descriptives by each version
length(unique(data$versionCode)) #check for 16 versions
col1 <- as.numeric(unique(data$versionCode))
col2 <- unique(data$version)
versionList <- cbind(col1, col2)
versionList <- as.data.frame(versionList)
versionList$col1 <- as.numeric(versionList$col1)
versionList[order(versionList$col1),]

##### STUDY TIME
group0 <- filter(data, StudyTimeCoded == 0)
group1 <- filter(data, StudyTimeCoded == 1)

# Simpler table
freqWD <- freq(group0$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# Simpler table
freqSP <- freq(group1$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# For Study time (in WD vs SP)
freqWD
freqSP

plotSTRT <- ggplot(data, aes(x = response_time)) + 
  geom_density(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) +
  geom_density(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw response times between study time", 
       subtitle = "Red = Short ST, Green = Long ST",
       x = "Response time (ms)",
       y = "Density") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotSTRT


plotSTAcc <- ggplot(data, aes(x = factor(accuracy, levels = 0:1, labels = c("Incorrect", "Correct")))) + geom_bar(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) + geom_bar(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw accuracy between study time", 
       subtitle = "Red = Short ST, Green = Long ST",
       x = "Accuracy",
       y = "Count") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotSTAcc

# PATCHWORK PLOTS
png(filename = "plotST.RTAcc.png", width = 700, height = 500, units = "px")
plotSTRT + plotSTAcc
dev.off()


##### PROB OF CHANGE
group0 <- filter(data, ProbOfChangeCoded == 0)
group1 <- filter(data, ProbOfChangeCoded == 1)

# Simpler table
freqWD <- freq(group0$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# Simpler table
freqSP <- freq(group1$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# For Prob of Change (in WD vs SP)
freqWD
freqSP

plotPCRT <- ggplot(data, aes(x = response_time)) + geom_density(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) + geom_density(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw response times between probabability of change", 
       subtitle = "Red = Low Change, Green = High Change",
       x = "Response time (ms)",
       y = "Count") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotPCRT


plotPCAcc <- ggplot(data, aes(x = factor(accuracy, levels = 0:1, labels = c("Incorrect", "Correct")))) + 
  geom_bar(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) + 
  geom_bar(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw accuracy between probability of change", 
       subtitle = "Red = Low Change, Green = High Change",
       x = "Accuracy",
       y = "Count") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotPCAcc

png(filename = "plotPC.RTAcc.png", width = 700, height = 500, units = "px")
plotPCRT + plotPCAcc
graphics.off()

##### CHOICE URGENCY

group0 <- filter(data, ChoiceUrgencyCoded == 0)
group1 <- filter(data, ChoiceUrgencyCoded == 1)

# Simpler table
freqWD <- freq(group0$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# Simpler table
freqSP <- freq(group1$accuracy, 
               report.nas = FALSE, 
               totals     = FALSE, 
               cumul      = FALSE, 
               headings   = FALSE)

# For Choice Urgency (in WD vs SP)
freqWD
freqSP

plotCURT <- ggplot(data, aes(x = response_time)) + geom_density(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) + geom_density(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw response times between choice urgency", 
       subtitle = "Red = Minimal CU, Green = High CU",
       x = "Response time (ms)",
       y = "Count") + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )

plotCURT


plotCUAcc <- ggplot(data, aes(x = accuracy)) + geom_bar(data = group0, fill = "#FF6699", color = "black", alpha = 0.4) + geom_bar(data = group1, fill = "#00CC33", alpha = 0.3, color = "black") +
  labs(title = "Raw response times between choice urgency", 
       subtitle = "Red = Minimal CU, Green = High CU",
       x = "Accuracy",
       y = "Count") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )
plotCUAcc

# PATCHWORK
png(filename = "plotCU.RTAcc.png", width = 700, height = 500, units = "px")
plotCURT + plotCUAcc
dev.off()


###################################################################
# Explore RT and accuracy by individual

pData <- data %>% 
  group_by(participant_id)

# Creating a PID variable that is numbered
dataID <- pData %>%
  dplyr::mutate(PID = cur_group_id())

data <- dataID
unique(data$PID)

col1 <- as.numeric(unique(data$PID))
col2 <- unique(data$participant_id)
participantList <- cbind(col1, col2)
participantList <- as.data.frame(participantList)
participantList[order(participantList$col1),]


freqPerPTab = matrix(NA, pNo, 6)
colnames(freqPerPTab) = c("pid", "incorrect", "correct", "incorrectPct", "correctPct", "totalN")

for(i in 1:pNo){
  miniDf <- data %>% filter(PID == participantList[i,1])
  freqPerP <- freq(miniDf$accuracy,
                   report.nas = FALSE, 
                   totals     = FALSE, 
                   cumul      = FALSE, 
                   headings   = FALSE)
  freqPerPTab[i,1] = i
  freqPerPTab[i,2] = freqPerP[1]
  freqPerPTab[i,3] = freqPerP[2]
  freqPerPTab[i,4] = round(freqPerP[5],3)
  freqPerPTab[i,5] = round(freqPerP[6],3)
  freqPerPTab[i,6] = freqPerP[4] 
}
freqPerPTab

### Export overall accuracy per participants
write.csv(freqPerPTab, file = "freqPerPTab.csv")

# *****************************************
# Export processed dataset for DDM analysis
# *****************************************
fileName = "dataForAnalysisMergedN68.csv"
write.csv(fData2, file = fileName)

fileNameDem = "dataForAnalysisDemoN68.csv"
write.csv(demData, file = fileNameDem)



###########################################
# PART 3. Create summarized dataframe
###########################################

# Convert response times from milliseconds to seconds
data$response_time_sec <- data$response_time / 1000

# Group data by participant_id, experimental condition, trial type, and day
summary_data <- data %>%
  group_by(participant_id, version, StudyTimeCoded, ChoiceUrgencyCoded, 
           ProbOfChangeCoded, ProbeTypeCoded, day) %>%
  summarise(
    mean_RT_sec = mean(response_time_sec, na.rm = TRUE),  # Mean response time in seconds
    variance_RT_sec = var(response_time_sec, na.rm = TRUE),  # Variance of RT in seconds
    total_accuracy = sum(accuracy, na.rm = TRUE),              # Total accurate responses
    nTrials = n() 
  ) %>%
  mutate(
    version_numeric = as.numeric(factor(version))              # Convert 'version' to numeric
  )

# Merging demographics with the summarized data
merged_summary_data <- merge(demData, summary_data, by = "participant_id")

# Converting factor variables in demographics data to numeric
demData_numeric <- demData %>%
  mutate(
    raceNew_numeric = as.numeric(raceNew),
    ethnicity_numeric = as.numeric(ethnicity),
    sex_numeric = as.numeric(sex),
    education_numeric = as.numeric(education)
  ) %>%   select(participant_id, age, raceNew_numeric, ethnicity_numeric,
                 sex_numeric, education_numeric, covid)  # Keep only numeric columns


# Merging the numeric demographic data with the summary data
merged_summary_data_numeric <- merge(demData_numeric, summary_data, by = "participant_id")

# View the new summarized dataframe
head(merged_summary_data_numeric)

# Optionally save the summarized dataframe as a CSV file
write.csv(merged_summary_data_numeric, file = "summarized_demographics_experiment_data_numeric.csv")

###########################################
# PART 4. Create and print codebook
###########################################

# Create a codebook for reference
codebook <- data.frame(
  Variable = c(
    "participant_id", "version", "StudyTimeCoded", "ChoiceUrgencyCoded", 
    "ProbOfChangeCoded", "ProbeTypeCoded", "day", 
    "mean_RT_sec", "variance_RT_sec", "mean_accuracy",
    "raceNew_numeric", "ethnicity_numeric", "sex_numeric", "education_numeric"
  ),
  Description = c(
    "Unique ID for each participant",
    "Experimental condition version (1-16 coded from task_version)",
    "Study time condition (0: ST- (500ms), 1: ST+ (2000ms))",
    "Choice urgency condition (0: RT+ (500ms), 1: RT- (3000ms))",
    "Probability of change condition (0: PC- (50/50), 1: PC+ (20/80))",
    "Probe type condition (0: whole display, 1: single probe)",
    "Day of the experiment for each trial",
    "Mean response time in seconds for each participant/condition/day",
    "Variance of response time in seconds for each participant/condition/day",
    "Mean accuracy (0: incorrect, 1: correct) for each participant/condition/day",
    "Race coded as numeric (1: White, 2: Black/AA, ..., 7: Multi-ethnic)",
    "Ethnicity coded as numeric (1: Not Hispanic, 2: Hispanic)",
    "Sex coded as numeric (1: Female, 2: Male, 3: Other)",
    "Education level coded as numeric (1: Elementary, ..., 9: Doctoral degree)"
  )
)

# Print the codebook
print(codebook)

# Optionally save the codebook as a CSV file
write.csv(codebook, file = "codebook.csv")

###########################################
# End of script
###########################################
