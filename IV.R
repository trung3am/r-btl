library(moments)
library(dplyr)
library(readxl)

CDR <- read_excel("192_CO1007.xlsx", 
                          range = "B2:C12")

LO <- read_excel("192_CO1007.xlsx", 
                          sheet = "GK_0", range = "B8:AA12", col_names = FALSE)

colnames(LO)[1] <- "MADE" 
LO
#1-2
LietKe <- CDR[is.na(CDR[,"STT"] ),]
SoCDR <- nrow(LietKe)

SoCDR
LietKe

#3