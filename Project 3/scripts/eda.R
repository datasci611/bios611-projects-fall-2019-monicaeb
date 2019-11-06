## EDA and cleaning for project 3, bios 611, umd

# load packages
library(tidyverse)
library(stringr)
library(tools)

demog.orig <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/CLIENT_191102.tsv'),delim="\t")
inc_exit <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/INCOME_EXIT_191102.tsv'),delim="\t")
inc_enter <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/INCOME_ENTRY_191102.tsv'),delim="\t")
hins_exit <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/HEALTH_INS_EXIT_191102.tsv'),delim="\t")
hins_enter <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/HEALTH_INS_ENTRY_191102.tsv'),delim="\t")
visits.orig <- read_delim(url('https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/ENTRY_EXIT_191102.tsv'),delim="\t") 

# descriptives of demographic data
demog <- demog.orig
summary(demog$`Client Age at Entry`)
summary(demog$`Client Age at Exit`)
table(demog$`Client Gender`)
table(demog$`Client Primary Race`)

# clean up, prep to merge with other datasets
demog <- demog.orig %>% rename(entage=`Client Age at Entry`,exage=`Client Age at Exit`,UID=`EE UID`,
                          gender=`Client Gender`,race=`Client Primary Race`,ethnic=`Client Ethnicity`) %>%
  select(`Client ID`,entage,exage,gender,race,ethnic,UID) %>% distinct() # merge on EE UID var

# visuals: pie chart of race

# histogram of ages

# clients growth over the years by gender

### a concept: when are people coming and going to and from the shelter? describe monthly trends, avg length of stay. split on interesting demogs.
# using visits data, see when people are coming and going
# create numeric dates for sorting, split up dates
visits <- visits %>%
  mutate(first.datenum = as.numeric(strptime(`Entry Date`,"%m/%d/%Y")),
         last.datenum = as.numeric(strptime(`Exit Date`,"%m/%d/%Y")),
         first.m = format(strptime(`Entry Date`,"%m/%d/%Y"),"%m"),
         last.m = format(strptime(`Exit Date`,"%m/%d/%Y"),"%m"),
         first.mc = format(strptime(`Entry Date`,"%m/%d/%Y"),"%b"),
         last.mc = format(strptime(`Exit Date`,"%m/%d/%Y"),"%b"),
         first.d = format(strptime(`Entry Date`,"%m/%d/%Y"),"%d"),
         last.d = format(strptime(`Exit Date`,"%m/%d/%Y"),"%d"),
         first.y = format(strptime(`Entry Date`,"%m/%d/%Y"),"%Y"),
         last.y = format(strptime(`Exit Date`,"%m/%d/%Y"),"%Y"))

# some descriptives
min(as.numeric(visits$first.y),na.rm=TRUE) # 2012
max(as.numeric(visits$last.y),na.rm=TRUE) # 2019
length(unique(visits$`Client ID`)) # 2364 people

# box plot of first visits by month, each point is the sum for that year

# box plot of last visits by month, each point is the sum for that year

## concept: when are people coming and going throughout the year: trend or lack of trend has been established. what has changed in their lives between entry and departure? maybe income has increased. 

## concept: for those with monthly income listed, where is it coming from? is income higher when people leave UMD? 
table(inc_enter$`Income Source (Entry)`[!is.na(inc_enter$`Monthly Amount (Entry)`)]) # mostly earned and SSDI
table(inc_exit$`Source of Income (Exit)`[!is.na(inc_exit$`Monthly Amount (Exit)`)]) # mostly earned and SSDI


















