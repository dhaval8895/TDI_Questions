setwd("F:/TDI/Questions/")
#Loading Libraries

library(openxlsx)
library(dplyr)
library(lubridate)

#Quesion1

##Reading data

ques1 <- openxlsx::read.xlsx("ccrb_datatransparencyinitiative_20170207.xlsx", sheet = 2)
glimpse(ques1)

###1.1

sapply(ques1, function(x) sum(is.na(x)))
new_ques1 <- ques1[complete.cases(ques1), ]
dim(new_ques1)
ans1 <- length(unique(new_ques1$UniqueComplaintId))
print(paste("There are", ans1, "number of Unique Complaints that appear in the complete information dataset"))


###1.2

comp_boro <- new_ques1 %>% group_by(Borough.of.Occurrence) %>%
  summarise(Total_Complaints = n())
comp_boro1 <- comp_boro %>%  
  mutate(Proportion = Total_Complaints / sum(Total_Complaints)) %>%
  arrange(desc(Total_Complaints))
ans2 <- round(comp_boro1$Proportion[1], 9)
print(paste("Proportion of complaints that occur in the borough with the largest number of complaints is",
            ans2))

###1.3

new_ques2 <- filter(new_ques1, Received.Year == 2016)
boros <- c("BROOKLYN", "BRONX", "MANHATTAN", "QUEENS", "STATEN ISLAN")
population <- c(2648771,  1471160, 1664727, 2358582, 479458)
new3 <- data.frame(boros, population = population)
comp_boro2 <- comp_boro[-4, ]
comp_boro2 <- comp_boro2 %>%  
  mutate(Proportion = Total_Complaints / sum(Total_Complaints)) %>%
  arrange(desc(Total_Complaints))
comp_boro2$Complaints.Per.Capita <- format(
  round(comp_boro2$Total_Complaints / new3$population, 10))
comp_boro2$Complaints.Per.Capita <- as.numeric(as.character(comp_boro2$Complaints.Per.Capita))
comp_boro2$Complaints.100k <- 100000 * comp_boro2$Complaints.Per.Capita
comp_boro22 <- comp_boro2 %>% arrange(desc(Complaints.100k))
ans3 <- round(comp_boro2$Total_Complaints[2] / new3$population[2] * 100000, 6)
print(paste("Number of complaints per 100k residents in the borough with the highest number of
            complaints per capita resulting from incidents in 2016 is", ans3))

###1.4

new_ques1$Resultant <- new_ques1$Close.Year - new_ques1$Received.Year
ans4 <- round(mean(new_ques1$Resultant), 9)
print(paste("On an average it takes", ans4,
            "years for a Complaint to get closed"))
      

###1.5
sandf <- new_ques1 %>% group_by(Received.Year, `Complaint.Contains.Stop.&.Frisk.Allegations`) %>%
  summarise(Complaints = n())
sandf_t <- filter(sandf, `Complaint.Contains.Stop.&.Frisk.Allegations` == TRUE)
plot(sandf_t$Received.Year, sandf_t$Complaints, type = "b", lwd = 2,col = 'brown',
     main = 'Total Stop and Frisk Incidents that lead to Complaints from 1999 to 2017',
     xlab = "Received Year", ylab = "Total Complaints")

##We find the pick of Stop and Frisk incidents to occur at 2005 hence we will include from 2005

sadf_new <- filter(sandf_t, Received.Year >= 2005 & Received.Year <= 2016)
str(sadf_new)
model <- lm(Complaints ~ Received.Year, data = sadf_new)
summary(model)
predicted <- round(as.numeric(predict(model, newdata = data.frame(Received.Year = 2018))))
print(paste("There will be", predicted, "Stop and Frisk incidents in 2018 that will eventually
            lead to a complaint"))


###1.6

chist <- chisq.test(new_ques1$Complaint.Has.Video.Evidence, new_ques1$Is.Full.Investigation)
chist
stat <- chist$statistic
print(paste("Chi-square test statistic for testing whether a complaint is more likely to receive
            a full investigation when it has video evidence is found to be", stat, ". This is very high
            and states that their is a significant relationship between Full investigation of the complaint
            if video evidence is found. Also, the p-value of the test is highly significant."))
            

###1.7

new <- numeric()
for(i in 1:length(new_ques1$Allegation.FADO.Type)){
  if(new_ques1$Allegation.FADO.Type[i] == 'Abuse of Authority'){
    new[i] = 1
  }
  else if(new_ques1$Allegation.FADO.Type[i] == 'Discourtesy'){
    new[i] = 2
  }
  else if(new_ques1$Allegation.FADO.Type[i] == 'Force'){
    new[i] = 3
  }
  else{
    new[i] = 4
  }
}
cor(new_ques4)

##From the above correlation test we can say that of the four allegations a few have
#a strong negative relation with each other. This is indicative that presence of certain type
## of allegation will contain multiple allegations

new_ques3 <- new_ques1
new_ques3 <- new_ques3[, c(2,15)]
new_ques3 <- fastDummies::dummy_cols(new_ques3)[, -2]
compids <- aggregate(. ~ UniqueComplaintId, data = new_ques3, FUN = sum)
compids$Total_Allegations <- base::rowSums(compids[,2:5])
colnames(compids) <- c("ID", "Discourtesy", "Force", "Abuse_of_Authority", "Offinsive Language", "Allegations")
model_complaints <- lm(Allegations ~ . - ID, data = compids)
summary(model_complaints)

###1.8

officers <- 36000
boros <- c("BROOKLYN", "BRONX", "MANHATTAN", "QUEENS", "STATEN ISLAN")
precints <- c(23, 12, 22, 16, 4)
population <- c(2648771,  1471160, 1664727, 2358582, 479458)
new4 <- data.frame(boros, precints, population)
#new4$proportion_precints <- new4$precints / sum(new4$precints)
#new4$Avg_officers <- (36000 * new4$proportion_precints) / new4$precints

ques2 <- filter(ques1, Received.Year == 2016)
comp_boro3 <- ques2 %>% group_by(Borough.of.Occurrence) %>%
  summarise(Total_Complaints = n())
comp_boro3 <- comp_boro3[-c(4,7), ] %>% mutate(Proportion = Total_Complaints / sum(Total_Complaints)) %>%
  arrange(desc(Total_Complaints))

comp_boro3$Complaints.Per.Capita <- format(
  round(comp_boro3$Total_Complaints / new4$population, 10))

comp_boro3$Complaints.Per.Capita <- as.numeric(as.character(comp_boro3$Complaints.Per.Capita))
comp_boro3$Total_Officers <- officers * comp_boro3$Proportion
comp_boro3$Precints <- new4$precints
comp_boro3$Avg_Officers <- comp_boro3$Total_Officers / comp_boro3$Precints
ratio <- max(comp_boro3$Avg_Officers) / min(comp_boro3$Avg_Officers)
print(paste("The ratio of the highest number of officers per precinct to the lowest number of
            officers per precinct is", round(ratio, 10)))