# This program scrapes two websites for arabic names written in english 
# and saves the names to csv files along with their gender. The program
# genderFinder.R should be used to query the csv files for someone's 
# gender based on their first name.
#
# Author: Zak Ahmad
# Email: zakahmad@gatech.edu
#
#
# ____________________________________________________________________

library(XML)      # required for parsing html
library(reshape2) # required for the melt function

# file names for feminine and masculine names
femalesFile <- "females_en.csv"
malesFile <- "males_en.csv"

# to prevent redundant requests, if any of the csv files already exist
# in the current directory we abort the program
stopifnot(!file.exists(femalesFile))
stopifnot(!file.exists(malesFile))

# include the conditioners.R file
source("conditioners.R")

################################################
#######  FIRST TYPE URLs TO SCRAPE  ############
################################################

# this function parses the first website which stores the names between <b> tags
firstTypeUrlParser <- function(URL,gender,removeRTI = FALSE){
    # extract only the data between the <b> and </b> tags from the URL
    Parse <- xpathApply(htmlTreeParse(URL,useInternalNodes=T), path="//b", fun=xmlValue)
    # choose only the names, ignoring the other junk
    Names <- Parse[32:length(Parse)-2]
    # create data frame with names after making sure each row has only one name and
    # unecessary characters have been stripped out
    DF <- data.frame(singleEntry(removeChars(Names))[1],gender)
    colnames(DF) <- c("Name","Gender")
    # remove duplicates
    DF <- removeDuplicateNames(DF,removeRTI = TRUE)
}

# urls for the feminine and masculine names
fURL <- 'http://www.sudairy.com/arabic/fem.html'
mURL <- 'http://www.sudairy.com/arabic/masc.html'

# data frames containing feminine and masculine names
fDF <- firstTypeUrlParser(fURL,gender="Female",removeRTI=TRUE)
mDF <- firstTypeUrlParser(mURL,gender="Male",removeRTI=FALSE)

# write dataframes to csv
writeToCSV(fDF,femalesFile)
writeToCSV(mDF,malesFile)

################################################
#######   SECOND TYPE URLs TO SCRAPE    ########
################################################

# this function parses the second website which stores the names between <a> tags
secondTypeUrlParser <- function(URL,gender,removeRTI = FALSE){
    # extract only the data between the <a> and </a> tags from the URL
    Parse <- xpathApply(htmlTreeParse(URL,useInternalNodes=T), path="//a", fun=xmlValue)
    # in this website, the names happen to be between the following two strings
    start <- last(which(Parse == "Z")) + 1
    end <- last(which(Parse == "Quran Teacher New")) - 1
    # choose only the names, ignoring the other junk
    Names <- Parse[start:end]
    # create data frame with names after making sure each row has only one name and
    # unecessary characters have been stripped out
    DF <- data.frame(singleEntry(removeChars(Names))[1],gender)
    colnames(DF) <- c("Name","Gender")
    # remove duplicates
    DF <- removeDuplicateNames(DF,removeRTI = TRUE)
}

### These are sample urls for feminine and masculine names, each url ends with a letter
### from the alphabet and the scrapper loops through all the letters
# Females Letter A: http://www.searchtruth.com/baby_names/names.php?ntype=f&find=2&letter=A
# Males Letter A: http://www.searchtruth.com/baby_names/names.php?ntype=m&find=2&letter=A

# letters from the alphabet to append at the end of the urls
letters <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
mSkip <- c("C","V","X") # those letters have no names on that website
fSkip <- c("X") # those letters have no names on that website

# Scrape feminine names loop
for (i in 1:length(letters)){
    # the url without the letter
    fURL2 <- "http://www.searchtruth.com/baby_names/names.php?ntype=f&find=2&letter="
    # if a letter is in the fSkip list then we skip it
    if (!(letters[i] %in% fSkip)){
        # create url
        fURL2 <- paste(fURL2,letters[i],sep="")
        if (i>=2){
            # append new names to data frame
            fDF2 <- rbind(fDF2,secondTypeUrlParser(fURL2,gender="Female"))
        }else{
            # create new data frame
            fDF2 <- secondTypeUrlParser(fURL2,gender="Female")
        }
    }
}

# Scrape masculine names loop
for (i in 1:length(letters)){
    # the url without the letter
    mURL2 <- "http://www.searchtruth.com/baby_names/names.php?ntype=m&find=2&letter="
    # if a letter is in the mSkip list then we skip it
    if (!(letters[i] %in% mSkip)){
        # create url
        mURL2 <- paste(mURL2,letters[i],sep="")
        if (i>=2){
            # append new names to data frame
            mDF2 <- rbind(mDF2,secondTypeUrlParser(mURL2,gender="Male"))
        }else{
            # create new data frame
            mDF2 <- secondTypeUrlParser(mURL2,gender="Male")
        }
    }
}

# append new names removing duplicate ones to the first type url results
writeToCSV(fDF2,femalesFile)
writeToCSV(mDF2,malesFile)
