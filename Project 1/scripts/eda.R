
### exploratory analysis and cleaning for urban ministries project
### output: dataset for analysis and visualizations

library(tidyverse)
library(tools) #for text manipulation
library(lme4)

# read in data, tab separated
dat.orig <- read_delim("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\Projects\\Project 1\\data\\umd.txt",delim="\t")
dat <- dat.orig #save original version

sapply(dat,class)  #only first 13 columns defined in metadata on github
dat <- dat[,c(1:13)] %>% select(-c(Referrals))


# format dates for total sorting. create month and day column for trending.
dat <- dat %>% mutate(DATENUM=format(strptime(Date,"%m/%d/%Y"),"%Y%m%d"),
                      YEAR=as.numeric(substr(DATENUM,1,4)),
                      MONTH=as.numeric(substr(DATENUM,5,6)),
                      MONTH.C=format(strptime(Date,"%m/%d/%Y"),"%b"),
                      DAY=as.numeric(substr(DATENUM,7,8))) %>%
  arrange(`Client File Number`,DATENUM) %>%
  rename(ID=`Client File Number`) %>% #renaming ID
  select(-c(`Client File Merge`)) %>% # not going to use this
  mutate(ID=as.character(ID))


# big outlier in 2018: client #12943 had 450121 pounds which is probably not right
# also filter for last 20 years, some worrisome erroneous data before.
dat <- dat %>% filter(`Food Pounds`<450121 | is.na(`Food Pounds`),
                      YEAR>2000 & YEAR<2019)

#order months for plots by month
dat$MONTH.C <- factor(dat$MONTH.C,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# create range of services with first and last entry per person
dat <- dat %>% group_by(ID) %>%
  mutate(firstvis=as.numeric(strptime(DATENUM[1],"%Y%m%d")),
         lastvis=as.numeric(strptime(max(DATENUM),"%Y%m%d"))) %>%
         mutate(rangedays=(round((lastvis-firstvis)/86400,0))) %>%
  ungroup() # convert back to number of days by dividing by number of seconds/day

# compute useful variables for plots
 dat <- dat %>% group_by(ID,MONTH.C,YEAR) %>% #calculate dollars per month per person
   mutate(dollars.m.ind=sum(`Financial Support`,na.rm=TRUE)) %>% ungroup() %>%
   group_by(MONTH.C,YEAR) %>% 
   mutate(dollars.m.y=sum(`Financial Support`,na.rm=TRUE), #total dollars per month in financial assistance
          foodlbs.m.y=sum(`Food Pounds`,na.rm=TRUE),
          id.m.y=length(unique(ID))) %>% # total per month
   ungroup() %>% group_by(YEAR) %>%
   mutate(sumID.yr=length(unique(ID)),
          sumfood.yr=sum(`Food Pounds`,na.rm=TRUE),
          dollars.yr=sum(`Financial Support`,na.rm=TRUE)) %>%
   ungroup()

 # calculate sum of services received by month and sum by id
 dat <- dat %>% group_by(ID) %>%
   mutate(sumevents.ID=sum(sum(!is.na(`Bus Tickets (Number of)`) & `Bus Tickets (Number of)`>0),
                           sum(!is.na(`Food Pounds`) & `Food Pounds`>0 | !is.na(`Food Provided for`) & `Food Provided for`>0),
                           sum(!is.na(`Clothing Items`) & `Clothing Items`>0),
                           sum(!is.na(`School Kits`) & `School Kits`>0),
                           sum(!is.na(Diapers) & Diapers >0),
                           sum(!is.na(`Financial Support`) & `Financial Support`>0), 
                           sum(!is.na(`Hygiene Kits`) & `Hygiene Kits`>0))) %>%
   ungroup() %>% group_by(YEAR,MONTH) %>%
   mutate(events.m.y=sum(sum(!is.na(`Bus Tickets (Number of)`) & `Bus Tickets (Number of)`>0),
                          sum(!is.na(`Food Pounds`) & `Food Pounds`>0 | !is.na(`Food Provided for`) & `Food Provided for`>0),
                          sum(!is.na(`Clothing Items`) & `Clothing Items`>0),
                          sum(!is.na(`School Kits`) & `School Kits`>0),
                          sum(!is.na(Diapers) & Diapers >0),
                          sum(!is.na(`Financial Support`) & `Financial Support`>0), 
                          sum(!is.na(`Hygiene Kits`) & `Hygiene Kits`>0))) %>% ungroup()
 
### subsets and plots to use### 
# dollars per month spent pretty constant
dat.dm <- unique(dat[,c("YEAR","MONTH.C","dollars.m.y")])  %>% 
  filter(YEAR%in%c(2002:2008))
p<-ggplot(data=dat.dm) +
  geom_boxplot(aes(x=MONTH.C,y=dollars.m.y),color="darkslategray") +
  geom_point(aes(x=MONTH.C,y=dollars.m.y),color="darkolivegreen") +
  theme_minimal() +
  labs(title="Total Financial Assistance by Month",x="Month",y="Dollars")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\dollars.month.box.png")

# how has the number of clients grown over the years?
# use uptick in clients as an introduction
dat.id <- dat %>% select(YEAR,sumID.yr) %>% distinct()
p <- ggplot(dat.id,mapping=aes(x=YEAR,y=sumID.yr)) +
  geom_point(color="darkorchid4") +
  theme_minimal() +
  labs(title="Figure 1.Total Clients Receiving Service",x="Year",y="Number of Clients")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\clients.year.point.png")

## incidence of new clients each month
dat.new.id.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
  group_by(ID) %>% filter(DATENUM==min(DATENUM)) %>% ungroup() %>% #keep first instance of each subject
  group_by(MONTH.C,YEAR) %>% mutate(new.ids=length(unique(ID))) %>% ungroup() %>%
  select(new.ids,YEAR,MONTH,MONTH.C) %>% distinct()
dat.new.id.mean <- dat.new.id.m %>% group_by(MONTH.C) %>% mutate(mean.ids=mean(new.ids)) %>%
  select(mean.ids,MONTH.C) %>% ungroup() %>% distinct() 
p<-ggplot(dat.new.id.m,mapping=aes(x=MONTH.C,y=new.ids)) +
  geom_point(color="darkorchid4") +
  geom_boxplot() +
  geom_point(dat=dat.new.id.mean,aes(x=MONTH.C,y=mean.ids),color="cornflowerblue") +
  theme_minimal() +
  labs(title="Figure 2. New Clients by Month",x="Month",y="Number of New Clients")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\new.ids.month.box.png")


dat.id.food <- dat %>% select(YEAR,sumID.yr,sumfood.yr) %>% distinct()
# ggplot(dat.id.food,mapping=aes(x=YEAR,y=sumfood.yr)) +
#   geom_point() # big outlier in 2018 removed above

#total food lbs by month and year
dat.food.month <- dat %>% select(YEAR,MONTH,foodlbs.m.y,id.m.y) %>% distinct() %>%
  mutate(YEARMONTH=as.numeric(paste0(YEAR,MONTH)))
# ggplot(data = dat.food.month,aes(x=YEARMONTH,y=foodlbs.m.y)) +
#   geom_point() +
#   xlim(200500,201900) +
#   geom_smooth(method="lm")

# create linear model for food lbs per month ~ people served per month
glm(data=dat.food.month,foodlbs.m.y ~ id.m.y)
aov(data = dat.food.month,foodlbs.m.y ~ id.m.y)

p<-ggplot(data=dat.food.month) +
  geom_point(aes(x=id.m.y,y=foodlbs.m.y),color="steelblue") +
  theme_minimal() +
  labs(title="Monthly Pounds of Food by Number of Clients",x="Number of Clients",y="Food (lbs) given")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\food.id.all.scat.png")

p<-ggplot(data=dat.food.month[dat.food.month$foodlbs.m.y>0,],aes(x=id.m.y,y=foodlbs.m.y)) +
  geom_point(color="steelblue")+
  geom_smooth(se=FALSE,method="lm",color="lightsteelblue4") +
  ylim(c(0,20000)) +
  theme_minimal() +
  labs(title="Figure 8. Monthly Pounds of Food (>0) by Number of Clients",x="Number of Clients",y="Food (lbs) given")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\food.id.reg.scat.png")

# financial not really useful
dat.fin.cli.yr <- dat %>% select(YEAR,sumID.yr,dollars.yr) %>% distinct() %>% 
  filter(YEAR%in%c(2002:2008))
dat.id.fin <- dat %>% select(YEAR,sumID.yr) %>% 
  distinct() %>% filter(YEAR%in%c(2002:2008))
ggplot(data=dat.fin.cli.yr) +
  geom_point(aes(x=YEAR,y=dollars.yr))
ggplot(data=dat.id.fin) +
  geom_point(aes(x=YEAR,y=sumID.yr))

# pretty consistent number of events per month, june looks is a little more active
dat.events <- dat %>% select(MONTH.C,MONTH,YEAR,events.m.y) %>% distinct() 
ggplot(data=dat.events) +
  geom_boxplot(aes(x=MONTH.C,y=events.m.y)) +
  geom_point(aes(x=MONTH.C,y=events.m.y))

#### distribution of how long people stay
id.range <- dat %>% select(ID,rangedays) %>% distinct()

ggplot(data=id.range,aes(x=rangedays)) +
  geom_histogram(bins=40)
p<-ggplot(data=id.range[id.range$rangedays>1,],aes(x=rangedays)) +
  geom_histogram(bins=40,fill="snow3",color="snow4") +
  geom_vline(aes(xintercept=mean(rangedays)),color="darkgreen") +
  geom_vline(aes(xintercept=median(rangedays)),color="seagreen") +
  theme_minimal() + labs(title="Figure 4. Histogram of Range of Days (>1) per Client",x="Range of days from first to last visit",y="Count")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\days.range.hist.png")

#### distribution of number of events (number of days on which they received services)
id.events <- dat %>% select(ID,sumevents.ID) %>% distinct()

#plot all data
ggplot(data=id.events,aes(x=sumevents.ID)) +
  geom_histogram(bins=40)

#plot up to the 0.95 quantile
median(id.events$sumevents.ID)
mean(id.events$sumevents.ID)
p<-ggplot(data=id.events[id.events$sumevents.ID<quantile(id.events$sumevents.ID,0.95),],aes(x=sumevents.ID)) +
  geom_histogram(bins=16,fill="snow2",color="snow4") +
  geom_vline(aes(xintercept=mean(sumevents.ID)),color="cornflowerblue") +
  geom_vline(aes(xintercept=median(sumevents.ID)),color="slateblue4") +
  theme_minimal() + labs(title="Figure 3. Histogram of Number of Services (up to 0.95 quantile) \nProvided per Client",x="Number of Services",y="Count")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\serv.hist.95.png")

### plots by first and last events ###
# what month or season is most popular for a first event?
# dat.first1 <- dat %>% group_by(ID) %>% arrange(DATENUM) %>% 
#   mutate(firstev=MONTH.C[1]) %>%
#   ungroup() %>% select(ID,firstev) %>% distinct() 
# ggplot(data=dat.first1) + 
#   geom_bar(aes(x=firstev))

#### looking into last days ###
# including clients with only 1 record
dat.last1 <- dat %>% group_by(ID) %>% mutate(DATENUM2=as.numeric(strptime(DATENUM,"%Y%m%d"))) %>% 
  filter(DATENUM2==lastvis) %>% ungroup() 
# people with 2 or more distinct days
dat.last <- dat %>% group_by(ID) %>% 
  mutate(DATENUM2=as.numeric(strptime(DATENUM,"%Y%m%d")),numevents=length(MONTH.C)) %>% 
  filter(DATENUM2==lastvis & numevents>1 & rangedays>1) %>% ungroup() 
# last day investigation
ggplot(data=dat.last) + # excluding day 1
  geom_bar(aes(x=MONTH.C))

dat.last.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
  group_by(ID) %>% filter(DATENUM==max(DATENUM)) %>% ungroup() %>% #keep first instance of each subject
  group_by(MONTH.C,YEAR) %>% mutate(ids.last=length(unique(ID))) %>% ungroup() %>%
  select(ids.last,YEAR,MONTH,MONTH.C) %>% distinct()
p<-ggplot(data=dat.last.m,mapping=aes(x=MONTH.C,y=ids.last)) +
  geom_point(color="darkorchid4") +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Figure 5. Last Visits by Month",x="Month",y="Number of Last Visits")
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\last.ids.month.box.png")

# type of service (bus, finances, hygiene, school) at last visit vs frequency. clothing and food overwhelmingly prevalent
dat.last.serv <- dat.last %>% 
  select(ID,DATENUM,MONTH.C,`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`) %>%
  gather(`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`,key="typeof",value="value") %>%
  filter(!is.na(value) & value >0) %>%
  mutate(typeof=ifelse(grepl("Food",typeof),"Food",typeof)) %>% #consolidate food into 1 category
  mutate(value=ifelse(is.na(value),"","yes")) %>% distinct() %>%
  filter(typeof!="Clothing Items" & typeof!="Food")
p<-ggplot(data=dat.last.serv) +
  geom_bar(mapping = aes(x = typeof, y = ..prop.., group = 1), stat = "count",color="aquamarine4",fill="snow3") +
  ylim(0,1) + labs(title="Figure 7. Distribution of non-food, non-clothing services among last visits",x="Service",y="Proportion excluding food/clothes") +
  theme_minimal() 
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\last.serv.png")


# all days
dat.serv.all <- dat %>% 
  select(ID,DATENUM,MONTH.C,`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`) %>%
  gather(`Bus Tickets (Number of)`,`Food Pounds`,`Food Provided for`,`Clothing Items`,`Hygiene Kits`,`School Kits`,`Financial Support`,key="typeof",value="value") %>%
  filter(!is.na(value) & value >0) %>%
  mutate(typeof=ifelse(grepl("Food",typeof),"Food",typeof)) %>% #consolidate food into 1 category
  mutate(value=ifelse(is.na(value),"","yes")) %>% distinct() %>%
  filter(typeof!="Clothing Items" & typeof!="Food")
p<-ggplot(data=dat.serv.all) +
  geom_bar(aes(x = typeof, y = ..prop.., group = 1), stat = "count",color="cadetblue3",fill="snow3") +
  ylim(0,1) + labs(title="Figure 6. Distribution of non-food, non-clothing services among all visits",x="Service",y="Proportion excluding food/clothes") +
  theme_minimal() 
ggsave(plot=p,"C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 1\\results\\all.serv.png")





