
### exploratory analysis for urban ministries project

library(tidyverse)
library(tools) #for text manipulation

# read in data, tab separated
dat <- read_delim("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\Projects\\Project 1\\data\\umd.txt",delim="\t")

colnames(dat) #only first 13 columns defined in metadata on github
dat <- dat[,c(1:13)] %>% select(-c(Referrals))
sapply(dat,class) 

# how many people are we working with?
length(unique(dat$`Client File Number`)) #15,352

# format dates for total sorting. create month and day column for trending.


# take a look at unique values of service notes
length(unique(dat$`Notes of Service`))

dat$`Notes of Service` <- trimws(tolower(dat$`Notes of Service`))

#use grep to replace plurals with singluar
dat$`Notes of Service` <- gsub("tickets","ticket",dat$`Notes of Service`)
dat$`Notes of Service` <- gsub("referrals","referral",dat$`Notes of Service`)

#what is in here?
dat$`Notes of Service`[30:90]

#i noticed often populated with clothes or clothing when the clothing column is actually NA. hmm


