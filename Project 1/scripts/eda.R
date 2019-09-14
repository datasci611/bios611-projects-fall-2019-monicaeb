
### exploratory analysis and cleaning for urban ministries project
### output: dataset for analysis and visualizations

library(tidyverse)
library(tools) #for text manipulation

# read in data, tab separated
dat.orig <- read_delim("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\Projects\\Project 1\\data\\umd.txt",delim="\t")
dat <- dat.orig #save original version
colnames(dat) #only first 13 columns defined in metadata on github
dat <- dat[,c(1:13)] %>% select(-c(Referrals))
sapply(dat,class) 

# how many people are we working with?
length(unique(dat$`Client File Number`)) #15,352

# format dates for total sorting. create month and day column for trending.
head(dat$Date)
dat <- dat %>% mutate(DATENUM=format(strptime(Date,"%m/%d/%Y"),"%Y%m%d"),
                      YEAR=as.numeric(substr(DATENUM,1,4)),
                      MONTH=as.numeric(substr(DATENUM,5,6)),
                      MONTH.C=format(strptime(DATENUM,"%Y%m%d"),"%b"),
                      MONTH.C=factor(MONTH.C,levels=c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")),
                      DAY=as.numeric(substr(DATENUM,7,8))) %>%
  arrange(`Client File Number`,DATENUM) %>%
  rename(ID=`Client File Number`) %>% #renaming ID
  select(-c(`Client File Merge`)) # not going to use this

# create range of services with first and last entry per person
dat <- dat %>% mutate(firstvis=as.numeric(strptime(DATENUM,"%Y%m%d")),
                      lastvis=as.numeric(strptime(DATENUM,"%Y%m%d")),
                      difftime(lastvis,firstvis))

# take a look at unique values of service notes
length(unique(dat$`Notes of Service`))
dat$`Notes of Service` <- trimws(tolower(dat$`Notes of Service`))

#use grep to replace plurals with singluar
dat$`Notes of Service` <- gsub("tickets","ticket",dat$`Notes of Service`)
dat$`Notes of Service` <- gsub("referrals","referral",dat$`Notes of Service`)

#i noticed often populated with clothes or clothing when the clothing column is actually NA. hmm
sum(!is.na(dat$`Clothing Items`))
#View(dat[is.na(dat$`Clothing Items`),])

# look at some associations. more bus tickets or diapers certain times of year? popular types of clothes?
# someone who had a bus pass, did they come back?
# avg number of finances per person? sum of financial support given how long people stayed?
# number of people food purchased for associated with time of year?

# fill in missing bus ticket info where there's a comment about bus tickets
#dat$`Bus Tickets (Number of)`[is.na(dat$`Bus Tickets (Number of)`&"bus"%in%dat$`Notes of Service`)]
length(unique(dat[!is.na(dat$`Bus Tickets (Number of)`) & dat$`Bus Tickets (Number of)`>0,]$ID)) #92 people got bus tickets
dat <- dat %>% group_by(ID) %>% mutate(tookbus=ifelse(sum(`Bus Tickets (Number of)`)==0,"N","Y")) %>%
  mutate(tookbus=ifelse(is.na(`Bus Tickets (Number of)`),NA,tookbus)) %>%
  mutate(nevents=sum(!is.na(ID))) %>% ungroup() #count number of events associated with a person
#View(unique(dat[,c("ID","tookbus")]))


# financial support pretty constant over months
ggplot(data=dat[dat$`Financial Support`>0,]) +
  geom_point(aes(x=MONTH.C,y=`Financial Support`)) +
  geom_boxplot(aes(x=MONTH.C,y=`Financial Support`))
  
# financial support pretty constant over months (spending per person per month for those with financial supprt > 0)
dat <- dat %>% group_by(ID,MONTH.C) %>% #calculate total dollars per month per person
  mutate(dollarsmonth=sum(`Financial Support`)) %>% ungroup()
dat.dm <- dat %>% select(MONTH.C,ID,dollarsmonth) %>% distinct() %>% filter(dollarsmonth>0)
ggplot(data=dat.dm) +
  geom_boxplot(aes(x=MONTH.C,y=dollarsmonth)) +
  geom_point(aes(x=MONTH.C,y=dollarsmonth)) 

# explore clothing?
dat.cl <- dat %>% filter(!is.na(`Clothing Items`))
dat.cl <- dat.cl %>% group_by(MONTH.C,YEAR) %>% #calculate total dollars per month per person
  mutate(clothesmonth=sum(`Clothing Items`)) %>% ungroup()
dat.cl <- dat.cl %>% select(MONTH.C,YEAR,clothesmonth) %>% distinct() %>% filter(clothesmonth>0)
hist(dat.cl$clothesmonth)

# how has the number of clients grown over the years?


