# This program parses html files which were downloaded from this website
# for masculine names (http://www.childclinic.net/names/first_name_boys.html),
# and this for feminine (http://www.childclinic.net/pain/girls.html)
# The html files contain arabic names written in arabic letters (as opposed
# to english). The files are in the "ar_html_files" directory and were
# downloaded using a simple R program which basically used the download.file
# function. The program genderFinder.R should be used to query the csv files for
# someone's gender based on their first name.
#
# Author: Zak Ahmad
# Email: zakahmad@gatech.edu
#
#
# ____________________________________________________________________

library(XML)      # required for parsing html

# function that loops thru the arabic alphabet and parses each html file
loopThruAlphabet <- function(urlString){
    # list of arabic alphabet
    alphabet <- c("A","B","T","TH","J","HA","KHA","D","THAL","R","Z","S","SH","SAD","DAD","TT","DDAD","AIN","GHIN","F","KAF","K","L","M","N","H","O","I")  
    # initialize empty list to contain names parsed
    Parse <- list()
    # loop thru alphabet
    for (i in 1:length(alphabet)){
        # generate the path to each html file
        filePath <- paste(getwd(),urlString,alphabet[i],".html",sep="")
        # get the html tree for the file
        Tree <- htmlTreeParse(filePath,useInternalNodes=T)
        # extract only the names which have font color #0000FF
        tmp<-xpathApply(Tree, path="//font[@color='#0000FF']",fun=xmlValue,encoding="UTF-8")
        # remove any spaces from the name
        tmp <- gsub(" ","",unlist(tmp))
        # remove new-line characters
        tmp <- gsub("\n","",tmp)
        # some entries in the html file are empty and should be removed
        ind <- which(nchar(tmp)==1)
        if (length(ind)!=0){tmp <- tmp[-ind]}
        # names that begin with letter A can be written differently
        if (alphabet[i] == "A"){            
            s <- substring(tmp,2,nchar(tmp))
            s <- paste0("ا",s)
            tmp <- c(tmp,s)
        }
        # concatenate the names from the current letter to the main list
        Parse <- c(Parse,tmp)
    }
    return(unlist(Parse))
}
# get list of feminine names
fList <- loopThruAlphabet("/ar_html_files/first_name_girls_")
# create a dataframe from the list
fDF <- data.frame(fList,"Female")
# rename columns to a more descriptive name
colnames(fDF) <- c("Name","Gender")

# get list of masculine names
mList <- loopThruAlphabet("/ar_html_files/first_name_boys_")
# create a dataframe from the list
mDF <- data.frame(mList,"Male")
# rename columns to a more descriptive name
colnames(mDF) <- c("Name","Gender")

# scrape masculine names from this link
URL <- "http://www.childclinic.net/names/child_names_god.html"
# extract only the names which have font color #0000FF
mNames <- unlist(xpathApply(htmlTreeParse(URL,useInternalNodes=T), path="//font[@color='#0000FF']",fun=xmlValue,encoding="UTF-8"))
# drop first entry (its just a column title not a name)
mNames <- mNames[-1]
# remove spaces and other characters
mNames <- gsub("\n","",mNames)
mNames <- gsub("\r","",mNames)
mNames <- gsub(" ","",mNames)
# append names to dataframe
mDF <- rbind(mDF,data.frame(Name=mNames,Gender="Male"))
# re-order (sort) dataframe alphabetically by name
mDF <- mDF[order(as.character(mDF$Name)),]

# write the dataframes to csv files
mFile <- "males_ar.csv"
fFile <- "females_ar.csv"
write.csv(mDF,file=mFile,quote=FALSE,row.names=FALSE)
write.csv(fDF,file=fFile,quote=FALSE,row.names=FALSE)