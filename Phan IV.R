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
LOR.GK11

#Learning outcome 12 (merge into usable data frame)
LOR.GK12 <- as.data.frame(bind_rows(DiemGK.1921.LO12,DiemGK.1922.LO12,DiemGK.1923.LO12,DiemGK.1924.LO12))
#Fill N/A cell with 0
LOR.GK12[is.na(LOR.GK12)] <- 0
#Sum column
LOR.GK12 <- as.data.frame(apply(LOR.GK12,2,sum))

LOR.GK12$SoCauTLDung <- row.names(LOR.GK12)
LOR.GK12

#Learning outcome 21 (merge into usable data frame)
LOR.GK21 <- as.data.frame(bind_rows(DiemGK.1921.LO21,DiemGK.1922.LO21,DiemGK.1923.LO21,DiemGK.1924.LO21))
#Fill N/A cell with 0
LOR.GK21[is.na(LOR.GK21)] <- 0
#Sum column
LOR.GK21 <- as.data.frame(apply(LOR.GK21,2,sum))

LOR.GK21$SoCauTLDung <- row.names(LOR.GK21)
LOR.GK21

#Learning outcome 22 (merge into usable data frame)
LOR.GK22 <- as.data.frame(bind_rows(DiemGK.1921.LO22,DiemGK.1922.LO22,DiemGK.1923.LO22,DiemGK.1924.LO22))
#Fill N/A cell with 0
LOR.GK22[is.na(LOR.GK22)] <- 0
#Sum column
LOR.GK22 <- as.data.frame(apply(LOR.GK22,2,sum))

LOR.GK22$SoCauTLDung <- row.names(LOR.GK22)
LOR.GK22

#Learning outcome 31 (merge into usable data frame)
LOR.GK31 <- as.data.frame(bind_rows(DiemGK.1921.LO31,DiemGK.1922.LO31,DiemGK.1923.LO31,DiemGK.1924.LO31))
#Fill N/A cell with 0
LOR.GK31[is.na(LOR.GK31)] <- 0
#Sum column
LOR.GK31 <- as.data.frame(apply(LOR.GK31,2,sum))

LOR.GK31$SoCauTLDung <- row.names(LOR.GK31)
LOR.GK31

#Graph ---

GLOR.GK11 <- plot2 <- ggplot(LOR.GK11, aes(x = as.character(LOR.GK11[,2]), y = LOR.GK11[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK11[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 11", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK12 <- plot2 <- ggplot(LOR.GK12, aes(x = as.character(LOR.GK12[,2]), y = LOR.GK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK21 <- plot2 <- ggplot(LOR.GK21, aes(x = as.character(LOR.GK21[,2]), y = LOR.GK21[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK21[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 21", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK22 <- plot2 <- ggplot(LOR.GK22, aes(x = as.character(LOR.GK22[,2]), y = LOR.GK22[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK22[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 22", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GK31 <- plot2 <- ggplot(LOR.GK31, aes(x = as.character(LOR.GK31[,2]), y = LOR.GK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOR.GKGK <-ggarrange(GLOR.GK11,GLOR.GK12,GLOR.GK21,GLOR.GK22,GLOR.GK31, ncol = 3, nrow = 2)

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
GLOR.CK12 <- plot2 <- ggplot(LOR.CK12, aes(x = as.character(LOR.CK12[,2]), y = LOR.CK12[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK12[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK23 <- plot2 <- ggplot(LOR.CK23, aes(x = as.character(LOR.CK23[,2]), y = LOR.CK23[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK23[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK31 <- plot2 <- ggplot(LOR.CK31, aes(x = as.character(LOR.CK31[,2]), y = LOR.CK31[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK31[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK32 <- plot2 <- ggplot(LOR.CK32, aes(x = as.character(LOR.CK32[,2]), y = LOR.CK32[,1])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.CK32[,1]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 32", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#57d5c5"))

GLOR.CK <-ggarrange(GLOR.CK12,GLOR.CK23,GLOR.CK31,GLOR.CK32, ncol = 3, nrow = 2)


#Problem 7 ----

