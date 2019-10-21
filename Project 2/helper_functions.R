## helper functions and data wrangling for BIOS 611 project 2
## Monica Borges, fall 2019

#load required packages
library(tidyverse)

##################      wrangling      ####################
# read in data, tab separated
#dat.orig <- readr::read_delim("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\Projects\\Project 1\\data\\umd.txt",delim="\t")
dat.orig <- readr::read_delim("umd.txt",delim="\t")
dat <- dat.orig #save original version

dat <- dat[,c(1:13)] %>% select(-c(Referrals)) #keep first 13 columns as described in metadata


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


# remove big outlier in 2018: client #12943 had 450121 pounds which is probably not right
# also filter for last 20 years, some worrisome erroneous data before.
dat <- dat %>% filter(`Food Pounds`<450121 | is.na(`Food Pounds`),
                      YEAR>2001 & YEAR<2019)

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




#################       plot functions for shiny app       #################

## plot for total clients receiving service by month. users can change year range for granularity. 
## if year range is small enough, counts will show at each point.

tot.ids.my <- function(yrlow,yrhi,points.typ) { #args: year min, year max, points type (year or year with months)
  
  
  if (points.typ=="both") { #if few years are selected,  increased granularity for months
    dat.id <- dat %>% select(YEAR,MONTH,id.m.y) %>% 
      mutate(YEAR.adj=as.numeric(substr(YEAR,3,4))-1,
             YEAR.month=YEAR.adj + (MONTH-1)/12) %>% distinct()
    
    p <- ggplot(dat.id,mapping=aes(x=YEAR.month,y=id.m.y)) +
      geom_point(color="darkorchid4") +
      theme_minimal() +
      labs(title="Total Clients Receiving Service",x="Years Since UMD Campus Opening (2001)",y="Total Number of Clients")
    
  } else {
    dat.id <- dat %>% mutate(YEAR.adj=as.numeric(substr(YEAR,3,4))-1) %>%
      select(YEAR.adj,sumID.yr) %>% distinct()
    p <- ggplot(dat.id,mapping=aes(x=YEAR.adj,y=sumID.yr)) +
      geom_point(color="darkorchid4") +
      theme_minimal() +
      labs(title="Total Clients Receiving Service",x="Years Since UMD Campus Opening (2001)",y="Total Number of Clients")
  }
  return(p)
}

## function to create text string, tell users that ranges of <6 years show more detail
numserved_txt <- function(yrlow,yrhi) {
  
  dat.id <- dat %>% select(YEAR,sumID.yr) %>% distinct()
  
  lownum <- (unique(dat.id[as.numeric(dat.id$YEAR)==yrlow,]$sumID.yr))
  hinum <- (unique(dat.id[as.numeric(dat.id$YEAR)==yrhi,]$sumID.yr))
  
  t <- paste0("The number of individuals served at UMD increased from ",lownum," total people in ",yrlow," to ", hinum ," total people in ",yrhi,".")
  
  return(t)
  
}

## boxplot for clients coming and going: arrive.go.month
## if only one year is selected, display a bar chart
## add a disclaimer that if 'last' is selected, clients with visits after that date are classified as 'last'
arr.go.plot <- function(yrlow,yrhi,visit) {  ## visit takes values 'first' or 'last', to represent 'coming' and going'
  
  if (visit=="first"){
    
    #create subset if 'first' visit is selected to look at new client temporal trends
    dat.new.id.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
      group_by(ID) %>% filter(DATENUM==min(DATENUM)) %>% ungroup() %>% #keep first instance of each subject
      group_by(MONTH.C,YEAR) %>% mutate(new.ids=length(unique(ID))) %>% ungroup() %>%
      select(new.ids,YEAR,MONTH,MONTH.C) %>% distinct() %>%
      filter(YEAR>=yrlow & YEAR<=yrhi)
    dat.new.id.mean <- dat.new.id.m %>% group_by(MONTH.C) %>% mutate(mean.ids=mean(new.ids)) %>%
      select(mean.ids,MONTH.C) %>% ungroup() %>% distinct() 
    
    #if only 1 year selected, create bar chart by month
    if (yrhi==yrlow) {
      dat.new.id.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
        group_by(ID) %>% filter(DATENUM==min(DATENUM)) %>% ungroup() %>% #keep first instance of each subject
        filter(YEAR>=yrlow & YEAR<=yrhi) %>%
        select(ID,MONTH.C) #%>% distinct() 
      
      p<-ggplot(dat.new.id.m) +
        geom_bar(aes(x=MONTH.C),color="mediumpurple4",fill="mediumpurple") +
        theme_minimal() +
        labs(title="New Clients by Month",x="Month",y="Number of New Clients")
      
    } else {
      
      p<-ggplot(dat.new.id.m,mapping=aes(x=MONTH.C,y=new.ids)) +
        geom_point(color="mediumpurple4") +
        geom_boxplot() +
        geom_point(dat=dat.new.id.mean,aes(x=MONTH.C,y=mean.ids),color="cornflowerblue") +
        theme_minimal() +
        labs(title="Distribution of New Clients by Month",x="Month",y="Number of New Clients")
    }
    
  } else if (visit=="last"){
    
    #create subset if 'first' visit is selected to look at new client temporal trends
    dat.last.id.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
      group_by(ID) %>% filter(DATENUM==max(DATENUM)) %>% ungroup() %>% #keep last instance of each subject
      group_by(MONTH.C,YEAR) %>% mutate(last.ids=length(unique(ID))) %>% ungroup() %>%
      select(last.ids,YEAR,MONTH,MONTH.C) %>% distinct() %>%
      filter(YEAR>=yrlow & YEAR<=yrhi)
    dat.last.id.mean <- dat.last.id.m %>% group_by(MONTH.C) %>% mutate(mean.ids=mean(last.ids)) %>%
      select(mean.ids,MONTH.C) %>% ungroup() %>% distinct() 
    
    #if only 1 year selected, create bar chart by month
    if (yrhi==yrlow) {
      dat.last.id.m <- dat %>% select(ID,MONTH,DAY,MONTH.C,YEAR,DATENUM) %>% arrange(ID,DATENUM) %>%
        group_by(ID) %>% filter(DATENUM==max(DATENUM)) %>% ungroup() %>% #keep last instance of each subject
        filter(YEAR>=yrlow & YEAR<=yrhi) %>%
        select(ID,MONTH.C) #%>% distinct() 
      
      p<-ggplot(dat.last.id.m) +
        geom_bar(aes(x=MONTH.C),color="mediumpurple4",fill="mediumpurple") +
        theme_minimal() +
        labs(title="Clients' Last Visits by Month",x="Month",y="Number of Last Visits")
      
    } else {
      
      p<-ggplot(dat.last.id.m,mapping=aes(x=MONTH.C,y=last.ids)) +
        geom_point(color="mediumpurple4") +
        geom_boxplot() +
        geom_point(dat=dat.last.id.mean,aes(x=MONTH.C,y=mean.ids),color="cornflowerblue") +
        theme_minimal() +
        labs(title="Distribution of Clients' Last Visits by Month",x="Month",y="Number of New Clients")
      
    }
  }
  return(p)
  
}

arr_go_text <- function(yrlow,yrhi,visit) {  ## visit takes values 'first' or 'last', to represent 'coming' and going'
  
  if (visit=="first" & !is.na(visit)){
    
    #if only 1 year selected, create bar chart by month
    if ((yrhi-yrlow)==0 & !is.na(yrhi) & !is.na(yrlow)) {
      
      t <- paste0("Aboslute counts of new UMD visitors in ",yrhi," are displayed in the above bar plot.")
      
    } else {
      
      t <- paste0("First visits in earlier years may simply include visits after UMD began collecting this type of data.")
      
    }
    
  } else {
    
    
    #if only 1 year selected, create bar chart by month
    if ((yrhi-yrlow)==0 & !is.na(yrhi) & !is.na(yrlow)) {
      
      t <- paste0("Aboslute counts of UMD visitors' last or most recent visitors in ",yrhi," are displayed in the above bar plot.")
      
    } else {
      
      t <- paste0("Last visits in more recent year may simply include most recent visits for clients planning to return to UMD.")
      
    }
  }
  return(t)
  
}

