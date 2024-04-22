# load relevant libraries
library(ggplot2)
library(dplyr)

# hiVote (hiVotes.csv)
hiVotes <- read.csv("/Users/kaylasimpson/Downloads/hiVotes.csv") # read the csv file
head(hiVotes) # view the first few rows of data
mean_hiVote <- mean(hiVotes$hiVote, na.rm = TRUE) # calculate the mean 
sd_hiVote <- sd(hiVotes$hiVote, na.rm = TRUE) # calculate the standard deviation
mean_hiVote <- round(mean_hiVote, 2) # round the results to the nearest hundredth
sd_hiVote <- round(sd_hiVote, 2)
print(paste("Mean of hiVote: ", mean_hiVote)) # print the results
print(paste("Standard Deviation of hiVote: ", sd_hiVote))

# scoreVote (scoreVotes.csv)
scoreVotes <- read.csv("/Users/kaylasimpson/Downloads/scoreVotes.csv") # read the csv file
head(scoreVotes) # view the first few rows of data
mean_scoreVote <- mean(scoreVotes$scoreVote, na.rm = TRUE) # calculate the mean 
sd_scoreVote <- sd(scoreVotes$scoreVote, na.rm = TRUE) #calculate the standard deviation
mean_scoreVote <- round(mean_scoreVote, 2) # round the results to the nearest hundredth
sd_scoreVote <- round(sd_scoreVote, 2)
print(paste("Mean of scoreVote: ", mean_scoreVote)) # print the results
print(paste("Standard Deviation of scoreVote: ", sd_scoreVote))

# scoreVote for questions about “Wellbeing"
scoreMetadata <- read.csv("/Users/kaylasimpson/Downloads/scoreMetadata.csv") # read file
joinedData <- merge(scoreVotes, scoreMetadata, by = "scoreId") # merge the datasets by common key
wellbeingData <- subset(joinedData, name == "Wellbeing")# filter to just questions about Wellbeing
head(wellbeingData) # view the first few rows
mean_wellbeing <- mean(wellbeingData$scoreVote, na.rm = TRUE) # calculate the mean 
sd_wellbeing <- sd(wellbeingData$scoreVote, na.rm = TRUE) # calculate the standard deviation
mean_wellbeing <- round(mean_wellbeing, 2) # round the results to the nearest hundredth
sd_wellbeing <- round(sd_wellbeing, 2)
print(paste("Mean of Wellbeing scoreVote: ", mean_wellbeing)) # print the results
print(paste("Standard Deviation of Wellbeing scoreVote: ", sd_wellbeing))

# scoreVote for the question "On a scale of 1 to 10, how would you rate the work-related stress?"
stressData <- subset(joinedData, question == "On a scale from 1 to 10, how would you rate the work-related stress?")
mean_stress <- mean(stressData$scoreVote, na.rm = TRUE) # calculate mean and sd
sd_stress <- sd(stressData$scoreVote, na.rm = TRUE)
mean_stress <- round(mean_stress, 2) # round
sd_stress <- round(sd_stress, 2)
print(paste("Mean of work-related stress scoreVote: ", mean_stress)) # print results
print(paste("Standard Deviation of work-related stress scoreVote: ", mean_wellbeing))

# create a bar plot of the number of companies per industry represented in this dataset 
companyData <- read.csv("/Users/kaylasimpson/Downloads/companyMetadata.csv")
# Count companies per industry
industryCounts <- companyData %>% 
  group_by(industry) %>% # group the data by the industry column
  summarise(Count = n()) %>% # calculates the count of companies in each industry
  arrange(desc(Count)) %>% # sorts industries in descending order by count
  top_n(10) # selects the top 10 industries

ggplot(industryCounts, aes(x = industry, y = count, fill = industry)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Industry", y = "Number of Companies", title = "Top 10 Industries by Number of Companies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

