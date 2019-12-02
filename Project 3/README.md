# BIOS 611 Project 3 - UMD Part 3
### Monica Borges 
##### @monicaeb

#### Background
[Urban Ministries of Durham](http://umdurham.org/) (UMD) has been helping end homelessness in the neighborhood for decades. Through a community shelter, cafe, food pantry, and clothing closet, UMD's efforts have touched over 15,000 lives and helped this many individuals pave a path out of homelessness. 
The intended audience for this project is primarily those working with or at UMD. Additionally, anyone interested in nonprofit data or poverty around North Carolina may find this project informative.

[Click here](https://rawcdn.githack.com/datasci611/bios611-projects-fall-2019-monicaeb/3753ac463060810f4142d3fbedda377e23a3db1a/Project 3/report/p3-umd-report.html) to see the rendered html report in a browser without having to download anything it raw.
[Click here](https://docs.google.com/presentation/d/1Gc2DPtDxYp77OK9zwu9oE_6pLZ7wyUgq3v9M5vwSy5A/edit?usp=sharing) to view accompanying presentation slides. 

#### Source Data
* Datasets provided by UMD describe shelter services provided at the Durham campus. 
* [Raw survey data](https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/EE_UDES_191102.tsv)
* [Raw entry and exit data](https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/ENTRY_EXIT_191102.tsv)
* [Raw demographic data](https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/CLIENT_191102.tsv)

#### Exploratory goals
* Similarly to Project 1 and Project 2, this project focuses on general comings-and-goings of clients to and from UMD. 
* The plots display temporal trends for when people arrive at or leave UMD, as well as trends with respect to length of stay in previous places, housing status, and some demographics.
* The goal of this analysis is to help UMD, or anyone else interested in homelessness in the Triangle, understand when, how, and for how long individuals in Durham use the UMD shelter.

#### Scripts
* eda.ipynb contains data cleaning and subsetting. The analysis data set created using eda.ipynb has all the information needed for the report.
* plots.R includes code to create and save plots used in the report and presentation.
* report.Rmd is the Rmarkdown file used to create the report. This script depends on plots.R and eda.ipynb to compile.

#### Make and Docker
To recreate this analysis and report, a [makefile](/scripts/makefile) and [docker file](/scripts/dockerfile) are available to manage workflow and ensure use of the correct R and python packages.





