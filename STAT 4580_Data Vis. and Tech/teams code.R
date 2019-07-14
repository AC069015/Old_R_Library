# Entering and Cleaning Data #

## CITATION: https://stats.idre.ucla.edu/r/codefragments/read_multiple/ for help in automating reading in multiple files

text_file_names <- c("2002_Teams.txt", "2003_Teams.txt", "2004_Teams.txt", "2005_Teams.txt", "2006_Teams.txt", "2007_Teams.txt", 
                     "2008_Teams.txt", "2009_Teams.txt", "2010_Teams.txt", "2011_Teams.txt", "2012_Teams.txt", "2013_Teams.txt", 
                     "2014_Teams.txt", "2015_Teams.txt", "2016_Teams.txt", "2017_Teams.txt", "2018_Teams.txt")

f <- file.path("C:/Users/Anthony/OneDrive/Documents/STAT 4580 Project/Text Files", c(text_file_names))
teams <- lapply(f, readr::read_tsv)

## CITATION: https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r for the loop help
## CITATION: https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters by 
## Stack Overflow user Cleonidas for the "removeNumbers" recommendation

## Getting tourney seed numbers out of "Team" variable
library(tm)
for(i in 1:17){
  teams[[i]]$Team <- removeNumbers(teams[[i]]$Team)
  teams[[i]]$Team <- trimws(teams[[i]]$Team)
}

## Add a "Year" variable
library(dplyr)
for(i in 1:17){
  for(j in 2002:2018){
    if(i == j - 2001){
      teams[[i]] <- mutate(teams[[i]], Year = j)
      teams[[i]] <- select(teams[[i]], Year, everything())
    } else{
      next
    }
  }
}
