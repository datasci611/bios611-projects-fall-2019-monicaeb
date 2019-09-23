
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


# format dates for total sorting. create month and day column for trending. filter for last 20 yrs because of some erroneous data early on.

dat <- dat %>% mutate(DATENUM=format(strptime(Date,"%m/%d/%Y"),"%Y%m%d"),
                      YEAR=as.numeric(substr(DATENUM,1,4)),
                      MONTH=as.numeric(substr(DATENUM,5,6)),
                      MONTH.C=format(strptime(Date,"%m/%d/%Y"),"%b"),
                      DAY=as.numeric(substr(DATENUM,7,8))) %>%
  arrange(`Client File Number`,DATENUM) %>%
  rename(ID=`Client File Number`) %>% #renaming ID
  select(-c(`Client File Merge`)) %>% # not going to use this
  filter(YEAR%in%c(1989:2019)) %>%
  mutate(ID=as.character(ID))

# big outlier in 2018: client #12943 had 450121 pounds which is probably not right
dat <- dat %>% filter(`Food Pounds`<450121 | is.na(`Food Pounds`))

#order months for plots by month
dat$MONTH.C <- factor(dat$MONTH.C,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# create range of services with first and last entry per person
dat <- dat %>% group_by(ID) %>%
  mutate(firstvis=as.numeric(strptime(DATENUM[1],"%Y%m%d")),
         lastvis=as.numeric(strptime(max(DATENUM),"%Y%m%d"))) %>%
         mutate(rangedays=(lastvis-firstvis)/86400 +1) %>%
  ungroup() # convert back to number of days by dividing by number of seconds/day


# # take a look at unique values of service notes
# length(unique(dat$`Notes of Service`))
# dat$`Notes of Service` <- trimws(tolower(dat$`Notes of Service`))
# #use grep to replace plurals with singluar
# dat$`Notes of Service` <- gsub("tickets","ticket",dat$`Notes of Service`)
# dat$`Notes of Service` <- gsub("referrals","referral",dat$`Notes of Service`)

#i noticed often populated with clothes or clothing when the clothing column is actually NA. hmm
#sum(!is.na(dat$`Clothing Items`))
#View(dat[is.na(dat$`Clothing Items`),])

# look at some associations. more bus tickets or diapers certain times of year? popular types of clothes?
# someone who had a bus pass, did they come back?
# avg number of finances per person? sum of financial support given how long people stayed?
# number of people food purchased for associated with time of year?

# fill in missing bus ticket info where there's a comment about bus tickets
#dat$`Bus Tickets (Number of)`[is.na(dat$`Bus Tickets (Number of)`&"bus"%in%dat$`Notes of Service`)]
# length(unique(dat[!is.na(dat$`Bus Tickets (Number of)`) & dat$`Bus Tickets (Number of)`>0,]$ID)) #92 people got bus tickets
# dat <- dat %>% group_by(ID) %>% mutate(tookbus=ifelse(sum(`Bus Tickets (Number of)`)==0,"N","Y")) %>%
#   mutate(tookbus=ifelse(is.na(`Bus Tickets (Number of)`),NA,tookbus)) %>%
#   mutate(nevents=sum(!is.na(ID))) %>% ungroup() #count number of events associated with a person
# #View(unique(dat[,c("ID","tookbus")]))
# 
# 
# financial support pretty constant over months
ggplot(data=dat[dat$`Financial Support`>0,]) +
  geom_point(aes(x=MONTH.C,y=`Financial Support`)) +
  geom_boxplot(aes(x=MONTH.C,y=`Financial Support`))


  
# financial support pretty constant over months (spending per person per month for those with financial supprt > 0)
dat1.fin <- dat %>% group_by(ID,MONTH.C) %>% #calculate total dollars per month per person
  mutate(dollarsmonth=sum(`Financial Support`)) %>% ungroup()
dat.dm <- dat.fin %>% select(MONTH.C,ID,dollarsmonth) %>% distinct() %>% filter(dollarsmonth>0)
ggplot(data=dat.dm) +
  geom_boxplot(aes(x=MONTH.C,y=dollarsmonth)) +
  geom_point(aes(x=MONTH.C,y=dollarsmonth)) 
# 
# # explore clothing?
# dat.cl <- dat %>% filter(!is.na(`Clothing Items`))
# dat.cl <- dat.cl %>% group_by(MONTH.C,YEAR) %>% #calculate total dollars per month per person
#   mutate(clothesmonth=sum(`Clothing Items`)) %>% ungroup()
# dat.cl <- dat.cl %>% select(MONTH.C,YEAR,clothesmonth) %>% distinct() %>% filter(clothesmonth>0)
# hist(dat.cl$clothesmonth)

# pretty consistent number of events per month, june looks is a little more active
dat.events <- dat %>% group_by(MONTH.C,YEAR) %>%
  mutate(sumevents=length(ID)) %>% ungroup() %>%
  select(MONTH.C,MONTH,YEAR,sumevents) %>% distinct() 

ggplot(data=dat.events) +
  geom_boxplot(aes(x=MONTH.C,y=sumevents)) +
  geom_point(aes(x=MONTH.C,y=sumevents))
# # more school supplies in august? not really useful or interesting.
# dat.sch <- dat %>% group_by(MONTH.C) %>%
#   mutate(`School Kits`=ifelse(is.na(`School Kits`),0,`School Kits`)) %>%
#   mutate(sumschool=sum(`School Kits`)) %>% ungroup() %>%
#   select(MONTH.C,MONTH,YEAR,sumschool) %>% distinct() 
# plot(dat.sch$MONTH.C,dat.sch$sumschool) #pretty consistent action between months too
# 
# dat.diap <- dat %>% filter(!is.na(Diapers) & Diapers>0)

############## useful plots to export #####################
# how has the number of clients grown over the years?
dat.id <- dat %>% group_by(YEAR) %>%
  mutate(sumID=length(unique(ID))) %>% ungroup() %>%
  select(YEAR,sumID) %>% distinct()

#use uptick in clients as an introduction
ggplot(dat.id,mapping=aes(x=YEAR,y=sumID)) +
  geom_point(aes(color=YEAR))

dat.id.food <- dat %>% group_by(YEAR) %>%
  mutate(sumID=length(unique(ID)),sumfood=sum(`Food Pounds`,na.rm=TRUE)) %>% ungroup() %>%
  select(YEAR,sumID,sumfood) %>% distinct()

ggplot(dat.id.food,mapping=aes(x=YEAR,y=sumfood)) +
  geom_point() # big outlier in 2018: client #12943 had 4500121 pounds which is probably not right

food.month.18 <- dat %>% filter(YEAR==2018) %>% group_by(MONTH.C) %>%
  mutate(sumfood=sum(`Food Pounds`,na.rm=TRUE)) %>%
  select(MONTH.C,sumfood) %>% distinct()
ggplot(food.month.18,mapping=aes(x=MONTH.C,y=sumfood)) +
  geom_point() # big outlier in 2018


dat.events <- dat %>% group_by(MONTH.C,YEAR) %>%
  mutate(sumevents=length(ID)) %>% ungroup() %>%
  select(MONTH.C,MONTH,YEAR,sumevents) %>% distinct() 

ggplot(data=dat.events) +
  geom_boxplot(aes(x=MONTH.C,y=sumevents)) +
  geom_point(aes(x=MONTH.C,y=sumevents))

#### distribution of how long people stay
id.range <-dat[,c("ID","rangedays")] %>% mutate(sumevents=length(ID)) %>% ungroup() %>%
  select(ID,sumevents) %>% distinct()
ggplot(data=id.range,aes(x=rangedays)) +
  geom_histogram(bins=40)
ggplot(data=id.range[id.range$rangedays>1,],aes(x=rangedays)) +
  geom_histogram(bins=40) +
  geom_vline(aes(xintercept=mean(rangedays))) +
  geom_vline(aes(xintercept=median(rangedays)))

#### distribution of number of events (number of days on which they received services)
id.events <- dat %>% group_by(ID) %>% mutate(sumevents=length(ID)) %>% ungroup() %>%
  select(ID,sumevents) %>% distinct()

#plot all data
ggplot(data=id.events,aes(x=sumevents)) +
  geom_histogram(bins=40)

#plot up to the 0.99 quantile
ggplot(data=id.events[id.events$sumevents<51,],aes(x=sumevents)) +
  geom_histogram(bins=40) +
  geom_vline(aes(xintercept=mean(sumevents))) +
  geom_vline(aes(xintercept=median(sumevents)))



# what month or season is most popular for a first event?
dat.first1 <- dat %>% group_by(ID) %>% arrange(DATENUM) %>% 
  mutate(firstev=MONTH.C[1]) %>%
  ungroup() %>% select(ID,firstev) %>% distinct() 
ggplot(data=dat.first1) + 
  geom_bar(aes(x=firstev))

###### looking into last days #####
# including clients with only 1 record
dat.last1 <- dat %>% group_by(ID) %>% mutate(DATENUM2=as.numeric(strptime(DATENUM,"%Y%m%d"))) %>% 
  filter(DATENUM2==lastvis) %>% ungroup() 
# clients with 2 or more
dat.last <- dat %>% group_by(ID) %>% 
  mutate(DATENUM2=as.numeric(strptime(DATENUM,"%Y%m%d")),numevents=length(MONTH.C)) %>% 
  filter(DATENUM2==lastvis & numevents>1 & rangedays>1) %>% ungroup() 

# last day investigation
ggplot(data=dat.last) + # excluding day 1
  geom_bar(aes(x=MONTH.C))

# type of service (bus, finances, hygiene, school) at last visit vs frequency. clothing and food overwhelmingly prevalent
dat.last.serv <- dat.last %>% 
  select(ID,DATENUM,MONTH.C,`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`) %>%
  gather(`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`,key="typeof",value="value") %>%
  filter(!is.na(value) & value >0) %>%
  mutate(typeof=ifelse(grepl("Food",typeof),"Food",typeof)) %>% #consolidate food into 1 category
  mutate(value=ifelse(is.na(value),"","yes")) %>% distinct() %>%
  filter(typeof!="Clothing Items" & typeof!="Food")
ggplot(data=dat.last.serv) +
  geom_bar(mapping = aes(x = typeof, y = ..prop.., group = 1), stat = "count") +
  ylim(0,1) 
# all days
dat.serv.all <- dat %>% 
  select(ID,DATENUM,MONTH.C,`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`) %>%
  gather(`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`,key="typeof",value="value") %>%
  filter(!is.na(value) & value >0) %>%
  mutate(typeof=ifelse(grepl("Food",typeof),"Food",typeof)) %>% #consolidate food into 1 category
  mutate(value=ifelse(is.na(value),"","yes")) %>% distinct() %>%
  filter(typeof!="Clothing Items" & typeof!="Food")
ggplot(data=dat.serv.all) +
  geom_bar(mapping = aes(x = typeof, y = ..prop.., group = 1), stat = "count") +
  ylim(0,1)





