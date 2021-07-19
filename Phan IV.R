#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("tibble")
#install.packages("xlsx")
#library

library(xlsx)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tibble)

#Problem 1 & 2 ----
#import data midterm exam

GKCDR <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 3, startRow = 1, endRow = 16)

#select Learning outcome

GKCDR <- GKCDR[7:10, 1:26]
GKCDR.1921 <- GKCDR %>% filter(GKCDR[1] == "1921") %>% select(X1:X25) %>% apply(2,as.numeric)
GKCDR.1922 <- GKCDR %>% filter(GKCDR[1] == "1922") %>% select(X1:X25) %>% apply(2,as.numeric)
GKCDR.1923 <- GKCDR %>% filter(GKCDR[1] == "1923") %>% select(X1:X25) %>% apply(2,as.numeric)
GKCDR.1924 <- GKCDR %>% filter(GKCDR[1] == "1924") %>% select(X1:X25) %>% apply(2,as.numeric)

#Quantity of Learning outcome related question appreared in midterm exam

length(names(table(GKCDR.1921)))

#List of learning outcome related question appeared in midterm exam

names(table(GKCDR.1921))

#import data final exam

CKCDR <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 5, startRow = 1, endRow = 16)

#select Learning outcome

CKCDR <- CKCDR[7:10, 1:30]
CKCDR.1921 <- CKCDR %>% filter(CKCDR[1] == "1921") %>% select(X1:X29) %>% apply(2,as.numeric)
CKCDR.1922 <- CKCDR %>% filter(CKCDR[1] == "1922") %>% select(X1:X29) %>% apply(2,as.numeric)
CKCDR.1923 <- CKCDR %>% filter(CKCDR[1] == "1923") %>% select(X1:X29) %>% apply(2,as.numeric)
CKCDR.1924 <- CKCDR %>% filter(CKCDR[1] == "1924") %>% select(X1:X29) %>% apply(2,as.numeric)

#Quantity of Learning outcome related question appeared in midterm exam

length(names(table(CKCDR.1921)))

#List of learning outcome related question appreared in midterm exam

names(table(CKCDR.1921))

#Problem 3 ---

#Frequency of Learning outcome in midterm exam

table(GKCDR.1921)

#Graph
G3GK.data <- as.data.frame(table(GKCDR.1921))

G3GK <- ggplot(G3GK.data, aes(x = GKCDR.1921,y =G3GK.data[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=G3GK.data[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Bieu do tan suat cua learning outcome trong thi GK", x = "Learning outcome", y = "Frequency") +
  theme(panel.background = element_rect(fill = "#ffaa80"))
G3GK 

#Problem 4 ---

#Frequency of Learning outcome in final exam

table(CKCDR.1921)

#Graph
G3CK.data <- as.data.frame(table(CKCDR.1921))

G3CK <- ggplot(G3CK.data, aes(x = CKCDR.1921,y =G3CK.data[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=G3CK.data[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Bieu do tan suat cua learning outcome trong thi CK", x = "Learning outcome", y = "Frequency") +
  theme(panel.background = element_rect(fill = "#ffaa80"))
G3CK 

#Problem 5 ---

#import data 

DiemGK <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 2, startRow = 5)

#Filter blank

DiemGK <- DiemGK %>% filter(No != "")

#Select data

DiemGK <- subset(DiemGK, select =  No:MADE)

DiemGK.1921 <- DiemGK %>% filter(MADE == 1921) %>% select(X1:X25) %>% apply(2,as.numeric)
DiemGK.1922 <- DiemGK %>% filter(MADE == 1922) %>% select(X1:X25) %>% apply(2,as.numeric)
DiemGK.1923 <- DiemGK %>% filter(MADE == 1923) %>% select(X1:X25) %>% apply(2,as.numeric)
DiemGK.1924 <- DiemGK %>% filter(MADE == 1924) %>% select(X1:X25) %>% apply(2,as.numeric)

#Add Learning outcome at top of data frame
DiemGK.1921.LO <- as.data.frame(rbind(GKCDR.1921,DiemGK.1921))
DiemGK.1922.LO <- as.data.frame(rbind(GKCDR.1922,DiemGK.1922))
DiemGK.1923.LO <- as.data.frame(rbind(GKCDR.1923,DiemGK.1923))
DiemGK.1924.LO <- as.data.frame(rbind(GKCDR.1924,DiemGK.1924))

#Learning outcome name in midterm
names(table(GKCDR.1921))

#1921
#Sorting answer into learning outcome group

DiemGK.1921.LO11 <- DiemGK.1921.LO[-1,DiemGK.1921.LO[1,] == 11]
DiemGK.1921.LO12 <- DiemGK.1921.LO[-1,DiemGK.1921.LO[1,] == 12]
DiemGK.1921.LO21 <- DiemGK.1921.LO[-1,DiemGK.1921.LO[1,] == 21]
DiemGK.1921.LO22 <- DiemGK.1921.LO[-1,DiemGK.1921.LO[1,] == 22]
DiemGK.1921.LO31 <- DiemGK.1921.LO[-1,DiemGK.1921.LO[1,] == 31]

#Sum row data frame and map sum result into instance group
DiemGK.1921.LO11 <-table(apply(DiemGK.1921.LO11, 1, sum))
DiemGK.1921.LO12 <-table(apply(DiemGK.1921.LO12, 1, sum))
DiemGK.1921.LO21 <-table(apply(DiemGK.1921.LO21, 1, sum))
DiemGK.1921.LO22 <-table(apply(DiemGK.1921.LO22, 1, sum))
DiemGK.1921.LO31 <-table(apply(DiemGK.1921.LO31, 1, sum))


#1922
#Sorting answer into learning outcome group

DiemGK.1922.LO11 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 11]
DiemGK.1922.LO12 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 12]
DiemGK.1922.LO21 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 21]
DiemGK.1922.LO22 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 22]
DiemGK.1922.LO31 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 31]

#Sum row data frame and map sum result into instance group
DiemGK.1922.LO11 <-table(apply(DiemGK.1922.LO11, 1, sum))
DiemGK.1922.LO12 <-table(apply(DiemGK.1922.LO12, 1, sum))
DiemGK.1922.LO21 <-table(apply(DiemGK.1922.LO21, 1, sum))
DiemGK.1922.LO22 <-table(apply(DiemGK.1922.LO22, 1, sum))
DiemGK.1922.LO31 <-table(apply(DiemGK.1922.LO31, 1, sum))

#1923
#Sorting answer into learning outcome group

DiemGK.1923.LO11 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 11]
DiemGK.1923.LO12 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 12]
DiemGK.1923.LO21 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 21]
DiemGK.1923.LO22 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 22]
DiemGK.1923.LO31 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 31]

#Sum row data frame and map sum result into instance group
DiemGK.1923.LO11 <-table(apply(DiemGK.1923.LO11, 1, sum))
DiemGK.1923.LO12 <-table(apply(DiemGK.1923.LO12, 1, sum))
DiemGK.1923.LO21 <-table(apply(DiemGK.1923.LO21, 1, sum))
DiemGK.1923.LO22 <-table(apply(DiemGK.1923.LO22, 1, sum))
DiemGK.1923.LO31 <-table(apply(DiemGK.1923.LO31, 1, sum))


#1924
#Sorting answer into learning outcome group

DiemGK.1924.LO11 <- DiemGK.1924.LO[-1,DiemGK.1924.LO[1,] == 11]
DiemGK.1924.LO12 <- DiemGK.1924.LO[-1,DiemGK.1924.LO[1,] == 12]
DiemGK.1924.LO21 <- DiemGK.1924.LO[-1,DiemGK.1924.LO[1,] == 21]
DiemGK.1924.LO22 <- DiemGK.1924.LO[-1,DiemGK.1924.LO[1,] == 22]
DiemGK.1924.LO31 <- DiemGK.1924.LO[-1,DiemGK.1924.LO[1,] == 31]

#Sum row data frame and map sum result into instance group
DiemGK.1924.LO11 <-table(apply(DiemGK.1924.LO11, 1, sum))
DiemGK.1924.LO12 <-table(apply(DiemGK.1924.LO12, 1, sum))
DiemGK.1924.LO21 <-table(apply(DiemGK.1924.LO21, 1, sum))
DiemGK.1924.LO22 <-table(apply(DiemGK.1924.LO22, 1, sum))
DiemGK.1924.LO31 <-table(apply(DiemGK.1924.LO31, 1, sum))

#Learning outcome 11 (merge into usable data frame)
LOR.GK11 <- as.data.frame(bind_rows(DiemGK.1921.LO11,DiemGK.1922.LO11,DiemGK.1923.LO11,DiemGK.1924.LO11))
#Fill N/A cell with 0
LOR.GK11[is.na(LOR.GK11)] <- 0
#Sum column
LOR.GK11 <- as.data.frame(apply(LOR.GK11,2,sum))
LOR.GK11$SoCauTLDung <- row.names(LOR.GK11)

#re order data
LOR.GK11<- transform(LOR.GK11, SoCauTLDung = as.numeric(SoCauTLDung))
LOR.GK11<- arrange(LOR.GK11,SoCauTLDung)
LOR.GK11

#Learning outcome 12 (merge into usable data frame)
LOR.GK12 <- as.data.frame(bind_rows(DiemGK.1921.LO12,DiemGK.1922.LO12,DiemGK.1923.LO12,DiemGK.1924.LO12))
#Fill N/A cell with 0
LOR.GK12[is.na(LOR.GK12)] <- 0
#Sum column
LOR.GK12 <- as.data.frame(apply(LOR.GK12,2,sum))
LOR.GK12$SoCauTLDung <- row.names(LOR.GK12)


#Learning outcome 21 (merge into usable data frame)
LOR.GK21 <- as.data.frame(bind_rows(DiemGK.1921.LO21,DiemGK.1922.LO21,DiemGK.1923.LO21,DiemGK.1924.LO21))
#Fill N/A cell with 0
LOR.GK21[is.na(LOR.GK21)] <- 0
#Sum column
LOR.GK21 <- as.data.frame(apply(LOR.GK21,2,sum))

LOR.GK21$SoCauTLDung <- row.names(LOR.GK21)


#Learning outcome 22 (merge into usable data frame)
LOR.GK22 <- as.data.frame(bind_rows(DiemGK.1921.LO22,DiemGK.1922.LO22,DiemGK.1923.LO22,DiemGK.1924.LO22))
#Fill N/A cell with 0
LOR.GK22[is.na(LOR.GK22)] <- 0
#Sum column
LOR.GK22 <- as.data.frame(apply(LOR.GK22,2,sum))

LOR.GK22$SoCauTLDung <- row.names(LOR.GK22)


#Learning outcome 31 (merge into usable data frame)
LOR.GK31 <- as.data.frame(bind_rows(DiemGK.1921.LO31,DiemGK.1922.LO31,DiemGK.1923.LO31,DiemGK.1924.LO31))
#Fill N/A cell with 0
LOR.GK31[is.na(LOR.GK31)] <- 0
#Sum column
LOR.GK31 <- as.data.frame(apply(LOR.GK31,2,sum))

LOR.GK31$SoCauTLDung <- row.names(LOR.GK31)


#Graph ---


GLOR.GK11 <- ggplot(LOR.GK11, aes(x = as.character(LOR.GK11[,2]), y = LOR.GK11[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK11[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 11", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK12 <-  ggplot(LOR.GK12, aes(x = as.character(LOR.GK12[,2]), y = LOR.GK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK21 <- ggplot(LOR.GK21, aes(x = as.character(LOR.GK21[,2]), y = LOR.GK21[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK21[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 21", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK22 <-  ggplot(LOR.GK22, aes(x = as.character(LOR.GK22[,2]), y = LOR.GK22[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK22[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 22", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK31 <- ggplot(LOR.GK31, aes(x = as.character(LOR.GK31[,2]), y = LOR.GK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK <-ggarrange(GLOR.GK11,GLOR.GK12,GLOR.GK21,GLOR.GK22,GLOR.GK31, ncol = 3, nrow = 2)
GLOR.GK
#Problem 6 ------
#import data 

DiemCK <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 4, startRow = 5)

#Filter blank

DiemCK <- DiemCK %>% filter(No != "")

#Select data

DiemCK <- subset(DiemCK, select =  No:MADE)

DiemCK.1921 <- DiemCK %>% filter(MADE == 1921) %>% select(X1:X29) %>% apply(2,as.numeric)
DiemCK.1922 <- DiemCK %>% filter(MADE == 1922) %>% select(X1:X29) %>% apply(2,as.numeric)
DiemCK.1923 <- DiemCK %>% filter(MADE == 1923) %>% select(X1:X29) %>% apply(2,as.numeric)
DiemCK.1924 <- DiemCK %>% filter(MADE == 1924) %>% select(X1:X29) %>% apply(2,as.numeric)

#Add Learning outcome at top of data frame
DiemCK.1921.LO <- as.data.frame(rbind(CKCDR.1921,DiemCK.1921))
DiemCK.1922.LO <- as.data.frame(rbind(CKCDR.1922,DiemCK.1922))
DiemCK.1923.LO <- as.data.frame(rbind(CKCDR.1923,DiemCK.1923))
DiemCK.1924.LO <- as.data.frame(rbind(CKCDR.1924,DiemCK.1924))

#Learning outcome name in final exam
names(table(CKCDR.1921))

#1921
#Sorting answer into learning outcome group


DiemCK.1921.LO12 <- DiemCK.1921.LO[-1,DiemCK.1921.LO[1,] == 12]
DiemCK.1921.LO23 <- DiemCK.1921.LO[-1,DiemCK.1921.LO[1,] == 23]
DiemCK.1921.LO31 <- DiemCK.1921.LO[-1,DiemCK.1921.LO[1,] == 31]
DiemCK.1921.LO32 <- DiemCK.1921.LO[-1,DiemCK.1921.LO[1,] == 32]

#Sum row data frame and map sum result into instance group

DiemCK.1921.LO12 <-table(apply(DiemCK.1921.LO12, 1, sum))
DiemCK.1921.LO23 <-table(apply(DiemCK.1921.LO23, 1, sum))
DiemCK.1921.LO31 <-table(apply(DiemCK.1921.LO31, 1, sum))
DiemCK.1921.LO32 <-table(apply(DiemCK.1921.LO32, 1, sum))

#1922
#Sorting answer into learning outcome group


DiemCK.1922.LO12 <- DiemCK.1922.LO[-1,DiemCK.1922.LO[1,] == 12]
DiemCK.1922.LO23 <- DiemCK.1922.LO[-1,DiemCK.1922.LO[1,] == 23]
DiemCK.1922.LO31 <- DiemCK.1922.LO[-1,DiemCK.1922.LO[1,] == 31]
DiemCK.1922.LO32 <- DiemCK.1922.LO[-1,DiemCK.1922.LO[1,] == 32]

#Sum row data frame and map sum result into instance group

DiemCK.1922.LO12 <-table(apply(DiemCK.1922.LO12, 1, sum))
DiemCK.1922.LO23 <-table(apply(DiemCK.1922.LO23, 1, sum))
DiemCK.1922.LO31 <-table(apply(DiemCK.1922.LO31, 1, sum))
DiemCK.1922.LO32 <-table(apply(DiemCK.1922.LO32, 1, sum))

#1923
#Sorting answer into learning outcome group


DiemCK.1923.LO12 <- DiemCK.1923.LO[-1,DiemCK.1923.LO[1,] == 12]
DiemCK.1923.LO23 <- DiemCK.1923.LO[-1,DiemCK.1923.LO[1,] == 23]
DiemCK.1923.LO31 <- DiemCK.1923.LO[-1,DiemCK.1923.LO[1,] == 31]
DiemCK.1923.LO32 <- DiemCK.1923.LO[-1,DiemCK.1923.LO[1,] == 32]

#Sum row data frame and map sum result into instance group

DiemCK.1923.LO12 <-table(apply(DiemCK.1923.LO12, 1, sum))
DiemCK.1923.LO23 <-table(apply(DiemCK.1923.LO23, 1, sum))
DiemCK.1923.LO31 <-table(apply(DiemCK.1923.LO31, 1, sum))
DiemCK.1923.LO32 <-table(apply(DiemCK.1923.LO32, 1, sum))

#1924
#Sorting answer into learning outcome group


DiemCK.1924.LO12 <- DiemCK.1924.LO[-1,DiemCK.1924.LO[1,] == 12]
DiemCK.1924.LO23 <- DiemCK.1924.LO[-1,DiemCK.1924.LO[1,] == 23]
DiemCK.1924.LO31 <- DiemCK.1924.LO[-1,DiemCK.1924.LO[1,] == 31]
DiemCK.1924.LO32 <- DiemCK.1924.LO[-1,DiemCK.1924.LO[1,] == 32]

#Sum row data frame and map sum result into instance group

DiemCK.1924.LO12 <-table(apply(DiemCK.1924.LO12, 1, sum))
DiemCK.1924.LO23 <-table(apply(DiemCK.1924.LO23, 1, sum))
DiemCK.1924.LO31 <-table(apply(DiemCK.1924.LO31, 1, sum))
DiemCK.1924.LO32 <-table(apply(DiemCK.1924.LO32, 1, sum))

#Learning outcome 12 (merge into usable data frame)
LOR.CK12 <- as.data.frame(bind_rows(DiemCK.1921.LO12,DiemCK.1922.LO12,DiemCK.1923.LO12,DiemCK.1924.LO12))

#Fill N/A cell with 0
LOR.CK12[is.na(LOR.CK12)] <- 0
#Sum column
LOR.CK12 <- as.data.frame(apply(LOR.CK12,2,sum))

LOR.CK12$SoCauTLDung <- row.names(LOR.CK12)
LOR.CK12

#Learning outcome 23 (merge into usable data frame)

LOR.CK23 <- as.data.frame(bind_rows(DiemCK.1921.LO23,DiemCK.1922.LO23,DiemCK.1923.LO23,DiemCK.1924.LO23))

#Fill N/A cell with 0
LOR.CK23[is.na(LOR.CK23)] <- 0
#Sum column
LOR.CK23 <- as.data.frame(apply(LOR.CK23,2,sum))

LOR.CK23$SoCauTLDung <- row.names(LOR.CK23)
LOR.CK23




#Learning outcome 31 (merge into usable data frame)

LOR.CK31 <- as.data.frame(bind_rows(DiemCK.1921.LO31,DiemCK.1922.LO31,DiemCK.1923.LO31,DiemCK.1924.LO31))

#Fill N/A cell with 0
LOR.CK31[is.na(LOR.CK31)] <- 0
#Sum column
LOR.CK31 <- as.data.frame(apply(LOR.CK31,2,sum))

LOR.CK31$SoCauTLDung <- row.names(LOR.CK31)
LOR.CK31

#Learning outcome 32 (merge into usable data frame)
LOR.CK32 <- as.data.frame(bind_rows(DiemCK.1921.LO32,DiemCK.1922.LO32,DiemCK.1923.LO32,DiemCK.1924.LO32))

#Fill N/A cell with 0
LOR.CK32[is.na(LOR.CK32)] <- 0
#Sum column
LOR.CK32 <- as.data.frame(apply(LOR.CK32,2,sum))

LOR.CK32$SoCauTLDung <- row.names(LOR.CK32)
LOR.CK32

#Graph ----
GLOR.CK12 <-ggplot(LOR.CK12, aes(x = as.character(LOR.CK12[,2]), y = LOR.CK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK23 <- ggplot(LOR.CK23, aes(x = as.character(LOR.CK23[,2]), y = LOR.CK23[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK23[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK31 <-ggplot(LOR.CK31, aes(x = as.character(LOR.CK31[,2]), y = LOR.CK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK32 <- ggplot(LOR.CK32, aes(x = as.character(LOR.CK32[,2]), y = LOR.CK32[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK32[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 32", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK <-ggarrange(GLOR.CK12,GLOR.CK23,GLOR.CK31,GLOR.CK32, ncol = 3, nrow = 2)
GLOR.CK

#Problem 7 ----
#Data of Learning outcome mid term and final exam
LO.data<- as.data.frame(bind_rows(table(GKCDR.1921),table(CKCDR.1921)))
#Convert in to usable data for chart drawing
LO.data[is.na(LO.data)] <- 0
LO.data<- as.data.frame(apply(LO.data,2,sum))
LO.data$LO <- row.names(LO.data)

#graph 
GLO.data <- ggplot(LO.data, aes(x = as.character(LO.data[,2]), y = LO.data[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LO.data[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Question per learning outcome", x = "Learning outcome", y = "Questions quantity") + theme(panel.background = element_rect(fill = "#57d5c5"))
GLO.data

#
TotalQFrameCK<-as.data.frame(table(as.numeric(CKCDR[1,-1])))
TotalQFrameGK<-as.data.frame(table(as.numeric(GKCDR[1,-1])))

g <- as.numeric(t[1,2])

#Problem 8 ----
#convert data from problem 6 for reuse

LOW.GK11 <- LOR.GK11
LOW.GK11 <- transform(LOW.GK11, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK11$SoCauTLSai <- abs(LOW.GK11$SoCauTLDung - as.numeric(TotalQFrameGK[1,2]))
LOW.GK11 <- LOW.GK11[,c(1,3)]

#percentaging
LOW.GK11[,1] <- round(LOW.GK11[,1]/sum(LOW.GK11[,1]),3)
LOW.GK11

LOW.GK12 <- LOR.GK12
LOW.GK12 <- transform(LOW.GK12, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK12$SoCauTLSai <- abs(LOW.GK12$SoCauTLDung - as.numeric(TotalQFrameGK[2,2]))
LOW.GK12 <- LOW.GK12[,c(1,3)]

#percentaging
LOW.GK12[,1] <- round(LOW.GK12[,1]/sum(LOW.GK12[,1]),3)
LOW.GK12

LOW.GK21 <- LOR.GK21
LOW.GK21 <- transform(LOW.GK21, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK21$SoCauTLSai <- abs(LOW.GK21$SoCauTLDung - as.numeric(TotalQFrameGK[3,2]))

LOW.GK21
#percentaging
LOW.GK21[,1] <- round(LOW.GK21[,1]/sum(LOW.GK21[,1]),3)
LOW.GK21


LOW.GK22 <- LOR.GK22
LOW.GK22 <- transform(LOW.GK22, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK22$SoCauTLSai <- abs(LOW.GK22$SoCauTLDung - as.numeric(TotalQFrameGK[4,2]))
LOW.GK22 <- LOW.GK22[,c(1,3)]

#percentaging
LOW.GK22[,1] <- round(LOW.GK22[,1]/sum(LOW.GK22[,1]),3)
LOW.GK22

LOW.GK31 <- LOR.GK31
LOW.GK31 <- transform(LOW.GK31, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK31$SoCauTLSai <- abs(LOW.GK31$SoCauTLDung - as.numeric(TotalQFrameGK[5,2]))
LOW.GK31 <- LOW.GK31[,c(1,3)]

#percentaging
LOW.GK31[,1] <- round(LOW.GK31[,1]/sum(LOW.GK31[,1]),3)
LOW.GK31

#Graph ---

GLOW.GK11 <- ggplot(LOW.GK11, aes(x = as.character(LOW.GK11[,2]), y = LOW.GK11[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK11[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 11", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK12 <-  ggplot(LOW.GK12, aes(x = as.character(LOW.GK12[,2]), y = LOW.GK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK21 <- ggplot(LOW.GK21, aes(x = as.character(LOW.GK21[,2]), y = LOW.GK21[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK21[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 21", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK22 <-  ggplot(LOW.GK22, aes(x = as.character(LOW.GK22[,2]), y = LOW.GK22[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK22[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 22", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK31 <- ggplot(LOW.GK31, aes(x = as.character(LOW.GK31[,2]), y = LOW.GK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK <-ggarrange(GLOW.GK11,GLOW.GK12,GLOW.GK21,GLOW.GK22,GLOW.GK31, ncol = 3, nrow = 2)

GLOW.GK

#Problem 9


names(table(CKCDR.1921))

#convert data from problem 6 for reuse

LOW.CK12 <- LOR.CK12
LOW.CK12 <- transform(LOW.CK12, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK12$SoCauTLSai <- abs(LOW.CK12$SoCauTLDung - as.numeric(TotalQFrameCK[1,2]))
LOW.CK12 <- LOW.CK12[,c(1,3)]

#percentaging
LOW.CK12[,1] <- round(LOW.CK12[,1]/sum(LOW.CK12[,1]),3)
LOW.CK12

LOW.CK23 <- LOR.CK23
LOW.CK23 <- transform(LOW.CK23, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK23$SoCauTLSai <- abs(LOW.CK23$SoCauTLDung - as.numeric(TotalQFrameCK[2,2]))
LOW.CK23 <- LOW.CK23[,c(1,3)]

#percentaging
LOW.CK23[,1] <- round(LOW.CK23[,1]/sum(LOW.CK23[,1]),3)
LOW.CK23

LOW.CK31 <- LOR.CK31
LOW.CK31 <- transform(LOW.CK31, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK31$SoCauTLSai <- abs(LOW.CK31$SoCauTLDung - as.numeric(TotalQFrameCK[3,2]))
LOW.CK31 <- LOW.CK31[,c(1,3)]

#percentaging
LOW.CK31[,1] <- round(LOW.CK31[,1]/sum(LOW.CK31[,1]),3)
LOW.CK31

LOW.CK32 <- LOR.CK32
LOW.CK32 <- transform(LOW.CK32, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK32$SoCauTLSai <- abs(LOW.CK32$SoCauTLDung - as.numeric(TotalQFrameCK[4,2]))
LOW.CK32 <- LOW.CK32[,c(1,3)]

#percentaging
LOW.CK32[,1] <- round(LOW.CK32[,1]/sum(LOW.CK32[,1]),3)
LOW.CK32

#Graph---

GLOW.CK12 <- ggplot(LOW.CK12, aes(x = as.character(LOW.CK12[,2]), y = LOW.CK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK23 <- ggplot(LOW.CK23, aes(x = as.character(LOW.CK23[,2]), y = LOW.CK23[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK23[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK31 <- ggplot(LOW.CK31, aes(x = as.character(LOW.CK31[,2]), y = LOW.CK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK32 <- ggplot(LOW.CK32, aes(x = as.character(LOW.CK32[,2]), y = LOW.CK32[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK32[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 32", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK <-ggarrange(GLOW.CK12,GLOW.CK23,GLOW.CK31,GLOW.CK32, ncol = 3, nrow = 2)

GLOW.CK

#Problem 10 ---


