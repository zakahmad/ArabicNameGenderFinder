# This program is an example of how to query a name for the gender typically
# associated with that name. Whats important is that some names are common
# between both males and females, for instance the name "Noor" is one of them.
#
# Author: Zak Ahmad
# Email: zakahmad@gatech.edu
#
#
# ____________________________________________________________________

# combine Arabic names written in English letters into one dataframe
enDF <- rbind(read.csv("males_en.csv",stringsAsFactors=FALSE),read.csv("females_en.csv",stringsAsFactors=FALSE))
# combine Arabic names written in Arabic letters into one dataframe
arDF <- rbind(read.csv("males_ar.csv",stringsAsFactors=FALSE),read.csv("females_ar.csv",stringsAsFactors=FALSE))

# now change the gender column for those names who are common to
# both genders to "Unkonwn"
enDF$Gender[which(duplicated(enDF$Name)==TRUE)] <- "Unknown"
arDF$Gender[which(duplicated(arDF$Name)==TRUE)] <- "Unknown"

# now query the gender given a name
en_name <- "Ahmad"
# convert the name to lowercase and find it
ind <- which(enDF$Name == tolower(en_name))
# if found, the length of ind will be non-zero
if (length(ind)){
    en_gender <- enDF$Gender[ind]
}else{
    en_gender <- "Name Not Found"
}

# the same can be done for a name in arabic letter
ar_name <- "أحمد"
# convert the name to lowercase and find it
ind <- which(arDF$Name == tolower(ar_name))
# if found, the length of ind will be non-zero
if (length(ind)){
    ar_gender <- arDF$Gender[ind]
}else{
    ar_gender <- "Name Not Found"
}
