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

# box plot length of stay at UMD by housing status
dat$stat <- dat$Housing.Status.2703.
dat <- dat %>% mutate(status=ifelse(stat=="At-risk of homelessness (HUD)","At Risk",
                                    ifelse(stat%in%c("Category 1 - Homeless (HUD)","Category 3 - Homeless only under other federal statutes (HUD)"),"Homeless",
                                           ifelse(stat=="Category 2 - At imminent risk of losing housing (HUD)","At imminent risk",
                                                  ifelse(stat=="Stably housed (HUD)","Stably housed",
                                                         ifelse(stat=="","","Unknown/Not reported"))))))

dat.stat <- dat %>% select(status,diffdays,Client.ID,Client.Gender) %>% distinct() %>% 
  filter(!is.na(diffdays),diffdays!="") %>% filter(!is.na(status),status!="") 

quantile(dat.stat$diffdays,0.95)

p_length <- ggplot(data=dat.stat[dat.stat$diffdays<=179,]) +
  geom_boxplot(aes(x=status,y=diffdays),color="snow4",fill="lavenderblush") + coord_flip() +
  #geom_point(aes(x=status,y=diffdays),color="snow4",size=0.9) + 
  #geom_jitter(aes(x=status,y=diffdays),width=0.25,color="snow4",size=0.3) +
  theme_minimal() + theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
  labs(title="Figure 4: Days spent at UMD Shelter (up to 95% quantile) by Housing Status",y="Days",x="Housing\nStatus")
p_length

## housing status by destination
unique(dat$Destination)
dat.dest <- dat %>%
  mutate(dest=ifelse(grepl(", permanent tenure",Destination),"friends/fam/perm",
                     ifelse(grepl(", temporary tenure",Destination),"friends/fam/temp",
                            ifelse(grepl("Rental|rental",Destination),"rental",
                                   ifelse(grepl("psychiatric|abuse|care facility|Hospital",Destination),"hosp/psych/care facility",
                                          ifelse(grepl("Owned by client",Destination),"owned",
                                                 ifelse(grepl("project|shelter|Transitional|formerly homeless persons",Destination),"shelter/proj/trans/hotel",
                                                        ifelse(grepl("Jail",Destination),"jail","Other/Unknown")))))))) %>%
  select(Client.ID,dest,status,diffdays) %>% filter(dest!="",status!="")

p_statdest <- ggplot(data=dat.dest, aes(status,fill=dest)) + 
  geom_bar(position="fill") + scale_color_brewer(palette = "Set2")
p_statdest



