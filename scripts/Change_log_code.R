#Set path to your working directory
setwd ("D:\\Reach\\UCVA\\VCL")
Sys.setlocale('LC_ALL', "Ukrainian") #setting a locale you are using!

library(openxlsx)
library(tidyr)

#Set names for the original and updated files 
CMA_org <- read.xlsx("File_raw.xlsx", skipEmptyCols = TRUE, na.strings = "", sheet = 1)
CMA_chg <- read.xlsx("File_cleaned.xlsx", skipEmptyCols = TRUE, na.strings = "", sheet = 1)

CMA_org[is.na(CMA_org)] <- "NA"
CMA_chg[is.na(CMA_chg)] <- "NA"

change <- data.frame("",stringsAsFactors = FALSE)

i <- 1
j <- 1

while (i < nrow(CMA_org)){
  while (j <= ncol(CMA_org)){
    ifelse(CMA_org[i,j] == CMA_chg[i,j], change[i,j] <- "", change[i,j] <- paste(colnames(CMA_org[j]),CMA_org[i,325],CMA_org[i,j], CMA_chg[i,j], sep="="))
    j = j+1
  }
  i=i+1
  j=j-ncol(CMA_org)
}

#in the line 22 in the part CMA_org[i,325] please put instead of 325, the number of column where uuid is situated
change_1 <- change[,!sapply(change, function(x) all(x == ""))]
change_1 <- change_1[!apply(change == "", 1, all),]
change_1 <- data.frame(t(change_1), stringsAsFactors=FALSE)
change_1 <- stack(change_1)
change_1 <- subset(change_1, values != "")
change_1 <- separate(change_1, col="values", into = c("question.name","uuid","old.value","new.value"), sep = "=")
change_1$issue <- ""
change_1$feedback <- ""
change_1$changed <- ""

change_ex <- change_1[,c(2,1,6,7,8,3,4)]

#final output
write.xlsx(change_ex,file="Changelog.xlsx")

#by Yaroslav Smirnov 