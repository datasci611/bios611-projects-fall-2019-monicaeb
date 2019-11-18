##### 

# plots to be used in report and presentation for bios 611 project 3, UMD part 3

library(tools)
library(tidyverse)
library(ggmap)

dat <- read.csv("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 3\\data\\p3_clean.csv",stringsAsFactors=FALSE)
setwd("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 3")
colnames(dat)

# demog plots: overall, how old are clients? histogram of age
demog <- unique(dat[,c("Client.ID","Client.Age.at.Entry","Client.Gender","Client.Veteran.Status","Client.Ethnicity","Client.Primary.Race")]) 
meanage <- mean(as.numeric(demog$Client.Age.at.Entry),na.rm=TRUE)
medage <- median(as.numeric(demog$Client.Age.at.Entry),na.rm=TRUE)
p_agehist <- ggplot(data=demog,aes(x=Client.Age.at.Entry)) +
  geom_histogram(bins=20,fill="snow2",color="skyblue4") +
  theme_minimal() +
  geom_vline(aes(xintercept=meanage),color="indianred3",size=1) + # mean = 44.3
  geom_vline(aes(xintercept=medage),color="indianred4",size=1) + # med = 46, n=3 are missing
  labs(title="Figure 1. Histogram of Client Age Upon Arrival",x="Age (years)",y="Count")
p_agehist

p_agehistf <- ggplot(data=demog[demog$Client.Gender=="Female",],aes(x=Client.Age.at.Entry)) +
  geom_histogram(bins=20,fill="snow2",color="skyblue4") +
  theme_minimal() +
  #geom_vline(aes(xintercept=meanage),color="indianred3",size=1) + # mean = 44.3
  #geom_vline(aes(xintercept=medage),color="indianred4",size=1) + # med = 46, n=3 are missing
  labs(title="Figure 1. Histogram of Client Age Upon Arrival",x="Age (years)",y="Count")
p_agehistf

p_agehistm <- ggplot(data=demog[demog$Client.Gender=="Male",],aes(x=Client.Age.at.Entry)) +
  geom_histogram(bins=20,fill="snow2",color="skyblue4") +
  theme_minimal() +
  #geom_vline(aes(xintercept=meanage),color="indianred3",size=1) + # mean = 44.3
  #geom_vline(aes(xintercept=medage),color="indianred4",size=1) + # med = 46, n=3 are missing
  labs(title="Figure 1. Histogram of Client Age Upon Arrival",x="Age (years)",y="Count")
p_agehistm

ggsave(plot=p_agehist,filename="./report/p_agehist.png",device="png")

# increase in people staying, by year, lines by gender and total
yearinc <- dat %>% group_by(first_year,Client.Gender) %>% mutate(newIDs=length(Client.ID)) %>%
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>% filter(!is.na(Client.Gender) & Client.Gender!="") %>%
  mutate(gender=gsub(" or Male to Female","",Client.Gender)) %>% distinct()
yearinc.all <- dat %>% group_by(first_year) %>% mutate(newIDs=length(Client.ID)) %>%
  ungroup() %>% select(Client.Gender,first_year,newIDs) %>%
  mutate(gender="All") %>% distinct()
yearinc <- data.frame(rbind(yearinc,yearinc.all)) %>% mutate(Gender=factor(gender,levels=unique(gender)))

p_yeargend <- ggplot(yearinc,aes(x=first_year)) + 
  geom_point(aes(y=newIDs, col=Gender)) +
  geom_smooth(aes(y=newIDs, col=Gender),se=FALSE) +
  theme_minimal() + scale_color_brewer(palette = "Set2") +
  labs(title="Figure 2. Number of New Clients per Year by Gender",x="Year",y="# Of Clients Arriving")
p_yeargend

ggsave(plot=p_yeargend,filename="./report/p_yeargend.png",device="png")


# histograms of people arriving/leaving, split by month
f_visit <- dat %>% group_by(first_year,first_m) %>% mutate(sumIDs=length(Client.ID)) %>%
  ungroup() %>% select(first_year,first_m,sumIDs) %>%
  mutate(first_c=month.abb[first_m]) %>% filter(!is.na(first_c)) %>% distinct() %>%
  mutate(first_c=factor(first_c,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
l_visit <- dat %>% group_by(last_year,last_m) %>% mutate(sumIDs=length(Client.ID)) %>%
  ungroup() %>% select(last_year,last_m,sumIDs) %>%
  mutate(last_c=month.abb[last_m]) %>% filter(!is.na(last_c)) %>% distinct() %>%
  mutate(last_c=factor(last_c,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

p_histf <- ggplot(data=f_visit,aes(x=first_c,y=sumIDs)) +
  geom_boxplot(color="lightcyan4",fill="lavender") +
  geom_point(color="skyblue4") +
  theme_minimal() + ylim(c(0,230)) +
  labs(title="Figure 3: Distribution of Number of New Clients by Month",x="Month",y="Number of New Clients")
p_histf

p_histl <- ggplot(data=l_visit,aes(x=last_c,y=sumIDs)) +
  geom_boxplot(color="lightcyan4",fill="honeydew1") +
  geom_point(color="skyblue4") +
  theme_minimal() + ylim(c(0,230)) +
  labs(title="Figure 4: Distribution of Number of Departures by Month",x="Month",y="Number of Departures")
p_histl

ggsave(plot=p_histf,filename="./report/p_histf.png",device="png")
ggsave(plot=p_histl,filename="./report/p_histl.png",device="png")

## is there any association between length of stay in previous place and stay at UMD?
unique(dat$Length.of.Stay.in.Previous.Place.1934.)

dat <- dat %>% mutate(prevstay=Length.of.Stay.in.Previous.Place.1934.) %>%
  mutate(prevstay=ifelse(prevstay%in%c("Client doesn't know (HUD)", "Client refused (HUD)","Data not collected (HUD)"),"Unknown",
                         ifelse(prevstay%in%c("One night or less","One week or less (HUD)","Two to six nights"),"< 1 week",
                                ifelse(prevstay=="One week or more, but less than one month","1 week - 1 month",
                                       ifelse(prevstay=="One month or more, but less than 90 days","1 - 3 months",
                                              ifelse(prevstay=="90 days or more, but less than one year","3 months - 1 year",
                                                     ifelse(prevstay=="One year or longer (HUD)","1+ year","")))))))

dat.prevlen <- dat %>% select(prevstay,diffdays) %>% filter(!is.na(prevstay)) %>%
  filter(prevstay!="",!is.na(diffdays)) %>% distinct()

# plot up to 0.95 quantile because time spent is so skewed right
quantile(dat$diffdays,0.95,na.rm=TRUE)
quantile(dat$diffdays,0.99,na.rm=TRUE)

p_prevlen <- ggplot(data=dat.prevlen[dat.prevlen$diffdays<=303,],aes(x=prevstay,y=diffdays)) +
  geom_boxplot(color="rosybrown4",fill="seashell1") + 
  theme_minimal() + theme(axis.text.y = element_text(angle = 30, hjust = 1)) + coord_flip() +
  labs(title="Figure 5: Days Spent at UMD Shelter (up to 95% quantile)\n by Time in Previous Location",
       x="Time spent at previous place",y="Days spent at UMD shelter")
p_prevlen

ggsave(plot=p_statdest,filename="./report/p_statdest.png",device="png")


 # box plot length of stay at UMD by housing status
dat$stat <- dat$Housing.Status.2703.
dat <- dat %>% mutate(status=ifelse(stat=="At-risk of homelessness (HUD)","At Risk",
                                    ifelse(stat%in%c("Category 1 - Homeless (HUD)","Category 3 - Homeless only under other federal statutes (HUD)"),"Homeless",
                                           ifelse(stat=="Category 2 - At imminent risk of losing housing (HUD)","At imminent risk",
                                                  ifelse(stat=="Stably housed (HUD)","Stably housed",
                                                         ifelse(stat=="","","Unknown/\nNot reported"))))))

dat.stat <- dat %>% select(status,diffdays,Client.ID,Client.Gender) %>% distinct() %>% 
  filter(!is.na(diffdays),diffdays!="") %>% filter(!is.na(status),status!="") 

p_lengthstat <- ggplot(data=dat.stat[dat.stat$diffdays<=303,]) +
  geom_boxplot(aes(x=status,y=diffdays),color="snow4",fill="lavenderblush") + coord_flip() +
  #geom_point(aes(x=status,y=diffdays),color="snow4",size=0.9) + 
  #geom_jitter(aes(x=status,y=diffdays),width=0.25,color="snow4",size=0.3) +
  theme_minimal() + theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
  labs(title="Figure 6: Days spent at UMD Shelter (up to 95% quantile)\n by Housing Status",y="Days",x="Housing\nStatus")
p_lengthstat

ggsave(plot=p_lengthstat,filename="./report/p_lengthstat.png",device="png")

## housing status by destination
unique(dat$Destination)
dat.dest <- dat %>%
  mutate(dest=ifelse(grepl(", permanent tenure",Destination),"Friends/family, permanent",
                     ifelse(grepl(", temporary tenure",Destination),"Friends/family, temp",
                            ifelse(grepl("Rental|rental",Destination),"Rental",
                                   ifelse(grepl("psychiatric|abuse|care facility|Hospital",Destination),"Hospital/psych/care facility",
                                          ifelse(grepl("Owned by client",Destination),"Owned",
                                                 ifelse(grepl("project|shelter|Transitional|formerly homeless persons",Destination),"Shelter/project/transitional",
                                                        ifelse(grepl("Jail",Destination),"Jail","Unknown")))))))) %>%
  select(Client.ID,dest,status,diffdays) %>% filter(dest!="",status!="")  %>%
  mutate(dest=factor(dest,levels=c("Rental","Owned","Friends/family, temp","Friends/family, permanent","Hospital/psych/care facility","Shelter/project/transitional","Unknown", "Jail"))) %>% filter(!is.na(dest))

p_statdest <- ggplot(data=dat.dest, aes(status,fill=dest)) + 
  geom_bar(position="fill") + scale_fill_brewer(palette = "Set2") +
  labs(title="Figure 7: Destination after UMD by Housing Status at Entry",x="Housing status",y="% of Clients") +
  guides(fill=guide_legend(title="Next destination")) #+ theme(axis.text.x = element_text(angle =30, hjust = 1))
p_statdest

ggsave(plot=p_statdest,filename="./report/p_statdest.png",device="png")



