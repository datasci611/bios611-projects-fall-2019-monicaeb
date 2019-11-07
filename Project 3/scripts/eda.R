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

## initial cleaning of previous dataset
#dat.orig <- readr::read_delim(url("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv"),delim="\t")

# descriptives of demographic data
demog <- demog.orig
summary(demog$`Client Age at Entry`)
summary(demog$`Client Age at Exit`)
table(demog$`Client Gender`)
table(demog$`Client Primary Race`)

# clean up, prep to merge with other datasets
demog <- demog.orig %>% rename(entage=`Client Age at Entry`,exage=`Client Age at Exit`,
                          gender=`Client Gender`,race=`Client Primary Race`,ethnic=`Client Ethnicity`) %>%
  select(`Client ID`,entage,exage,gender,race,ethnic,`EE UID`) %>% distinct() # merge on EE UID var

# visuals: pie chart of race, histogram of ages

# clients growth over the years by gender

### a concept: when are people coming and going to and from the shelter? describe monthly trends, avg length of stay. split on interesting demogs.
# using visits data, see when people are coming and going
# create numeric dates for sorting, split up dates
visits <- visits.orig %>%
  mutate(first_datenum = as.numeric(strptime(`Entry Date`,"%m/%d/%Y")),
         last_datenum = as.numeric(strptime(`Exit Date`,"%m/%d/%Y")),
         first_m = format(strptime(`Entry Date`,"%m/%d/%Y"),"%m"),
         last_m = format(strptime(`Exit Date`,"%m/%d/%Y"),"%m"),
         first_mc = format(strptime(`Entry Date`,"%m/%d/%Y"),"%b"),
         last_mc = format(strptime(`Exit Date`,"%m/%d/%Y"),"%b"),
         first_d = format(strptime(`Entry Date`,"%m/%d/%Y"),"%d"),
         last_d = format(strptime(`Exit Date`,"%m/%d/%Y"),"%d"),
         first_y = format(strptime(`Entry Date`,"%m/%d/%Y"),"%Y"),
         last_y = format(strptime(`Exit Date`,"%m/%d/%Y"),"%Y"))

# some descriptives
min(as.numeric(visits$first_y),na.rm=TRUE) # 2012
max(as.numeric(visits$last_y),na_rm=TRUE) # 2019
length(unique(visits$`Client ID`)) # 2364 people

# box plot of first visits by month, each point is the sum for that year
# box plot of last visits by month, each point is the sum for that year
firstvis_box_df <- visits %>% select(first_m,first_mc,first_y,`Client ID`) %>% 
  group_by(first_m,first_y) %>%
  mutate(sumids_m=length(unique(`Client ID`))) %>% ungroup() %>%
  distinct()
lastvis_box_df <- visits %>% select(last_m,last_mc,last_y,`Client ID`) %>% 
  group_by(last_m,last_y) %>%
  mutate(sumids_m=length(unique(`Client ID`))) %>% ungroup() %>%
  distinct()

# visual check here, actual plots to be made in python
boxplot(firstvis_box_df$sumids_m ~ firstvis_box_df$first_m)
boxplot(lastvis_box_df$sumids_m ~ lastvis_box_df$last_m)

# total counts of new clients per year by gender
visits_fm <- visits %>% select(first_y,first_m,first_mc,`EE UID`, `Client ID`) %>%
  distinct() %>% left_join(demog,by=c("EE UID","Client ID"))

## concept: when are people coming and going throughout the year: trend or lack of trend has been established. what has changed in their lives between entry and departure? maybe income has increased. 
summary(inc_enter$`Monthly Amount (Entry)`,na.rm=TRUE)
summary(inc_exit$`Monthly Amount (Exit)`,na.rm=TRUE)

## concept: for those with monthly income listed, where is it coming from? is income higher when people leave UMD? 
table(inc_enter$`Income Source (Entry)`[!is.na(inc_enter$`Monthly Amount (Entry)`)]) # mostly earned and SSDI
table(inc_exit$`Source of Income (Exit)`[!is.na(inc_exit$`Monthly Amount (Exit)`)]) # mostly earned and SSDI


















