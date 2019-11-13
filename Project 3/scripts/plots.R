##### 

# plots to be used in report and presentation for bios 611 project 3, UMD part 3

library(tools)
library(tidyverse)

dat <- read.csv("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 3\\data\\p3_clean.csv",stringsAsFactors=FALSE)

colnames(dat)

# demog plots: overall, how old are clients? histogram of age
demog <- unique(dat[,c("Client.ID","Client.Age.at.Entry","Client.Gender","Client.Veteran.Status","Client.Ethnicity","Client.Primary.Race")]) 
meanage <- mean(as.numeric(demog$Client.Age.at.Entry),na.rm=TRUE)
medage <- median(as.numeric(demog$Client.Age.at.Entry),na.rm=TRUE)
p_agehist <- ggplot(data=demog,aes(x=Client.Age.at.Entry)) +
  geom_histogram(bins=20,fill="snow3",color="skyblue4") +
  theme_minimal() +
  geom_vline(aes(xintercept=meanage),color="darkgreen") + # mean = 44.3
  geom_vline(aes(xintercept=medage),color="seagreen") + # med = 46, n=3 are missing
  labs(title="Figure 1. Histogram of Client Age Upon Arrival",x="Age (years)",y="Count")
p_agehist

# increase in people staying, by year, lines by gender and total
yearinc <- dat %>% group_by(first_year,Client.Gender) %>% mutate(newIDs=length(unique(Client.ID))) %>%
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>% filter(!is.na(Client.Gender)) %>%
  mutate(gender=gsub(" or Male to Female","",Client.Gender)) %>% distinct()
yearinc.all <- dat %>% group_by(first_year) %>% mutate(newIDs=length(unique(Client.ID))) %>%
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>%
  mutate(gender="All") %>% distinct()
yearinc <- data.frame(rbind(yearinc,yearinc.all)) %>% mutate(gender=factor(gender,levels=unique(gender)))


p_yeargend <- ggplot(yearinc,aes(x=first_year)) + 
  geom_point(aes(y=newIDs, col=gender)) +
  geom_smooth(aes(y=newIDs, col=gender),se=FALSE)
p_yeargend

# histograms of people arriving, split by month


# histogram of people leaving, split by month


# length of stay by length of stay in previous place


# 











