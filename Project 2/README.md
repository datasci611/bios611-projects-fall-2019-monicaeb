#BIOS 611 Project 2 - UMD Part 2
### Monica Borges

#### Background
[Urban Ministries of Durham](http://umdurham.org/) (UMD) has been helping end homelessness in the neighborhood for decades. Through a community shelter, cafe, food pantry, and clothing closet, UMD's efforts have touched over 15,000 lives and helped this many individuals pave a path out of homelessness. 
The intended audience for this project is primarily those working with or at UMD. Additionally, anyone interested in nonprofit data or poverty around North Carolina may find this Shiny app useful.

#### Source Data
Raw data provided by the client includes 79,838 records with 18 variables and 15,352 distinct Client ID Numbers. This dataset is found in the *data* folder.
Variables describe food pounds, bus tickets, clothing, financial assistance, dates, hygiene kits, diaper distribution, and school supplies kits.

#### Exploratory goals
* Allow static plots to be subset by year to increase level of detail
* Allow closer looks at activity during a single year by displaying different types of plots
Particular areas of focus for this project include when clients come and go throughout the year, and general increase in people served. 

#### Rshiny
Users can explore relationships between services and number of clients over time in the R Shiny App interface. 

#### Scripts
* app.R contains code for the Rshiny user interface and server. 
* helper.R contains data cleaning code and 'helper functions' to support the Shiny app.