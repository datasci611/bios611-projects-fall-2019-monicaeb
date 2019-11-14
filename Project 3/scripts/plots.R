##### 

# plots to be used in report and presentation for bios 611 project 3, UMD part 3

library(tools)
library(tidyverse)
library(ggmap)

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
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>% filter(!is.na(Client.Gender) & Client.Gender!="") %>%
  mutate(gender=gsub(" or Male to Female","",Client.Gender)) %>% distinct()
yearinc.all <- dat %>% group_by(first_year) %>% mutate(newIDs=length(unique(Client.ID))) %>%
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>%
  mutate(gender="All") %>% distinct()
yearinc <- data.frame(rbind(yearinc,yearinc.all)) %>% mutate(Gender=factor(gender,levels=unique(gender)))


p_yeargend <- ggplot(yearinc,aes(x=first_year)) + 
  geom_point(aes(y=newIDs, col=Gender)) +
  geom_smooth(aes(y=newIDs, col=Gender),se=FALSE) +
  theme_minimal() +
  labs(title="Figure 2. Number of New Clients per Year by Gender",x="Year",y="# Of Clients Arriving")
p_yeargend

# histograms of people arriving/leaving, split by month
f_visit <- dat %>% group_by(first_year,first_m) %>% mutate(sumIDs=length(unique(Client.ID))) %>%
  ungroup() %>% select(first_year,first_m,sumIDs) %>%
  mutate(first_c=month.abb[first_m]) %>% 
  mutate(first_c=factor(first_c,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
l_visit <- dat %>% group_by(last_year,last_m) %>% mutate(sumIDs=length(unique(Client.ID))) %>%
  ungroup() %>% select(last_year,last_m,sumIDs) %>%
  mutate(last_c=month.abb[last_m]) %>%
  mutate(last_c=factor(last_c,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

p_histf <- ggplot(data=f_visit,aes(x=first_c,y=sumIDs)) +
  geom_boxplot(color="lightcyan4",fill="lavender") +
  geom_point(color="skyblue4") +
  theme_minimal() +
  labs(title="Figure 3: Distribution of Number of New Clients by Month")
p_histf

p_histl <- ggplot(data=l_visit,aes(x=last_c,y=sumIDs)) +
  geom_boxplot(color="lightcyan4",fill="honeydew1") +
  geom_point(color="skyblue4") +
  theme_minimal() +
  labs(title="Figure 4: Distribution of Number of Departures by Month")
p_histl

# box plot length of stay at UMD by length of stay in previous place
dat$prevstay <- dat$Length.of.Stay.in.Previous.Place.1934.
dat <- dat %>% mutate(prevstay=ifelse(prevstay%in%c("Client doesn't know (HUD)", "Client refused (HUD)","Data not collected (HUD)"),"Unknown",
                                      ifelse(prevstay%in%c("One night or less","One week or less (HUD)","Two to six nights"),"< 1 week",
                                             ifelse(prevstay=="One week or more, but less than one month","1 week - 1 month",
                                                    ifelse(prevstay=="One month or more, but less than 90 days","1 - 3 months",
                                                           ifelse(prevstay=="90 days or more, but less than one year","3 months - 1 year",
                                                                  ifelse(prevstay=="One year or longer (HUD)","1+ year",prevstay)))))))

dat.prev <- dat %>% select(prevstay,diffdays,Client.ID,Client.Gender) %>% distinct() %>% 
  filter(!is.na(diffdays),diffdays!="") %>% filter(!is.na(prevstay),prevstay!="") 
quantile(dat.prev$diffdays,0.95)
p_histgen <- ggplot(data=dat.prev[dat.prev$diffdays<=175 & dat.prev$Client.Gender!="Trans Female (MTF or Male to Female)",]) +
  geom_violin(aes(x=Client.Gender,y=diffdays))
p_histgen

p_length <- ggplot(data=dat.prev,aes(x=prevstay,y=diffdays)) +
  geom_point() +
  geom_violin()
p_length
# 











