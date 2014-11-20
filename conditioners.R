# function to remove certain characters from names
removeChars <- function(namesList, tick = FALSE, or = FALSE, dash = FALSE, space = FALSE, ALL = TRUE){ 
  tmp <- namesList
  if (tick || or || dash || space){ALL = FALSE}
  if (ALL){
    tick = TRUE
    or = TRUE
    dash = TRUE
    space = TRUE
  }
  # remove ' from names 
  if (tick){tmp <- gsub("'","",namesList)}
  
  # replace the " or " with " , "
  if (or){tmp <- gsub(" or "," , ",tmp)}
  
  # remove - from names  
  if (dash){tmp <- gsub("-","",tmp)}
  
  # remove space from names
  if(space){tmp <- gsub(" ","",tmp)}
  
  return(tmp)
}

# function to make sure that each row in the list has a single name
singleEntry <- function(namesList, lowerCase = TRUE, splitList = TRUE, meltList = TRUE){  
  tmp <- namesList
  
  # convert to lower case
  if(lowerCase){tmp <- tolower(tmp)}
  
  # split the strings which have more than one name separated by a comma
  if(splitList){tmp <- strsplit(tmp,split=',')}
  
  # melt the rows with more than one name to one name per row
  if(meltList){tmp <- melt(tmp,level=1)}
  
  return(tmp)
}

# function to remove duplicate names in a dataframe
removeDuplicateNames <- function(namesDF, removeRTI = FALSE){  
  # remove the "returntoindex" entries which some websites have
  if(removeRTI){namesDF <- namesDF[which(namesDF$Name != "returntoindex"),]}

  return(namesDF[!duplicated(namesDF),])
}

# function writes dataframe to csv
writeToCSV <- function(DF,filename){
  if (!file.exists(filename)){
      # if it's the first time and file doesn't exist, save the data
      write.csv(DF,file=filename,quote=FALSE,row.names=FALSE)
  }else{
      # otherwise, read the data and append to new names
      DF <- rbind(read.csv(filename,stringsAsFactors=FALSE,check.names = FALSE,as.is=TRUE),DF)
      # remove any duplicate names then write the data
      DF <- removeDuplicateNames(DF)
      # sort alphabetically by name
      DF <- DF[order(DF$Name),] 
      write.csv(DF,file=filename,quote=FALSE,row.names=FALSE)
  }
}

# function returns last item in list, vector, or data frame
last <- function(x) { tail(x, n = 1) }