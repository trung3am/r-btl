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
length(names(table(GKCDR.1922)))
length(names(table(GKCDR.1923)))
length(names(table(GKCDR.1924)))
#List of learning outcome related question appeared in midterm exam

names(table(GKCDR.1921))
names(table(GKCDR.1922))
names(table(GKCDR.1923))
names(table(GKCDR.1924))
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
length(names(table(CKCDR.1922)))
length(names(table(CKCDR.1923)))
length(names(table(CKCDR.1924)))
#List of learning outcome related question appreared in midterm exam

names(table(CKCDR.1921))
names(table(CKCDR.1922))
names(table(CKCDR.1923))
names(table(CKCDR.1924))
#Problem 3 ---

#Frequency of Learning outcome in midterm exam

table(GKCDR.1921)

#Graph
G3GK.data1921 <- as.data.frame(table(GKCDR.1921))
G3GK.data1922 <- as.data.frame(table(GKCDR.1922))
G3GK.data1923 <- as.data.frame(table(GKCDR.1923))
G3GK.data1924 <- as.data.frame(table(GKCDR.1924))
#MD 1921, 1923, 1924 share mutual frequency

G3GK1921 <- ggplot(G3GK.data1921, aes(x = GKCDR.1921,y =G3GK.data1921[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=G3GK.data1921[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Bieu do tan suat Learning outcome GK-1921 ", x = "Learning outcome", y = "Frequency") +
  theme(panel.background = element_rect(fill = "#ffaa80"))
G3GK1921

G3GK1922 <- ggplot(G3GK.data1922, aes(x = GKCDR.1922,y =G3GK.data1922[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=G3GK.data1922[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Bieu do tan suat Learning outcome GK-1922", x = "Learning outcome", y = "Frequency") +
  theme(panel.background = element_rect(fill = "#ffaa80"))
G3GK1922

G3GK <-ggarrange(G3GK1922,G3GK1921, ncol = 2, nrow = 1)
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
DiemGK.1922.LO23 <- DiemGK.1922.LO[-1,DiemGK.1922.LO[1,] == 23]

#Sum row data frame and map sum result into instance group
DiemGK.1922.LO11 <-table(apply(DiemGK.1922.LO11, 1, sum))
DiemGK.1922.LO12 <-table(apply(DiemGK.1922.LO12, 1, sum))
DiemGK.1922.LO21 <-table(apply(DiemGK.1922.LO21, 1, sum))
DiemGK.1922.LO22 <-table(apply(DiemGK.1922.LO22, 1, sum))
DiemGK.1922.LO31 <-table(apply(DiemGK.1922.LO31, 1, sum))
DiemGK.1922.LO23 <-table(DiemGK.1922.LO23)
#1923
#Sorting answer into learning outcome group

DiemGK.1923.LO11 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 11]
DiemGK.1923.LO12 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 12]
DiemGK.1923.LO21 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 21]
DiemGK.1923.LO22 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 22]
DiemGK.1923.LO31 <- DiemGK.1923.LO[-1,DiemGK.1923.LO[1,] == 31]
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

#Learning outcome 23
LOR.GK23 <-as.data.frame(DiemGK.1922.LO23)
LOR.GK23$SoCauTraLoiDung <- LOR.GK23[,1]


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

GLOR.GK23 <- ggplot(LOR.GK23, aes(x = as.character(LOR.GK23[,3]), y = LOR.GK23[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=LOR.GK23[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi dung", y = "So sinh vien") + theme(panel.background = element_rect(fill = "#d557d5"))


GLOR.GK <-ggarrange(GLOR.GK11,GLOR.GK12,GLOR.GK21,GLOR.GK22,GLOR.GK23,GLOR.GK31, ncol = 3, nrow = 2)
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

TotalQFrameGKEX<-as.data.frame(bind_rows(table(GKCDR.1921),
                                         table(GKCDR.1922),
                                         table(GKCDR.1923),
                                         table(GKCDR.1924)))
TotalQFrameGKEX[is.na(TotalQFrameGKEX)] <- 0
#exception for 1922 LO 23 & 12
TotalQFrameGKEX

#Problem 8 ----
#convert data from problem 6 for reuse

LOW.GK11 <- LOR.GK11
LOW.GK11 <- transform(LOW.GK11, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK11$SoCauTLSai <- abs(LOW.GK11$SoCauTLDung - as.numeric(TotalQFrameGK[1,2]))
LOW.GK11 <- LOW.GK11[,c(1,3)]

#percentaging
LOW.GK11$percent <- round(LOW.GK11[,1]/sum(LOW.GK11[,1]),3)
LOW.GK11

#exception handling for Learning outcome 12 


LOW.GK12.1921.3.4<- as.data.frame(bind_rows(DiemGK.1921.LO12,
                                            DiemGK.1923.LO12,
                                            DiemGK.1924.LO12,
                                            ))
#fill na with 0
LOW.GK12.1921.3.4[is.na(LOW.GK12.1921.3.4)] <- 0

LOW.GK12.1921.3.4<- as.data.frame(apply(LOW.GK12.1921.3.4,2,sum))
LOW.GK12.1921.3.4$SoCauTLSai <- abs(as.numeric(row.names(LOW.GK12.1921.3.4)) - 12)

#percentaging
LOW.GK12.1921.3.4$percent <-round(LOW.GK12.1921.3.4[,1]/sum(LOW.GK12.1921.3.4[,1]),3)

LOW.GK12.1922<-t(as.data.frame(bind_rows(DiemGK.1922.LO12)))

temp1922 <- abs(as.numeric(row.names(LOW.GK12.1922)) - 11)

LOW.GK12.1922<-as.data.frame(DiemGK.1922.LO12)
LOW.GK12.1922$SoCauTLSai <- as.numeric(temp1922)

LOW.GK12.1922$percent <- round(LOW.GK12.1922[,2]/sum(LOW.GK12.1922[,2]),3)
LOW.GK12.1922<- LOW.GK12.1922[,c(3,4)]

#reserve

DiemGK.1921.LO12.mod <- DiemGK.1921.LO12
DiemGK.1922.LO12.mod <- DiemGK.1922.LO12
DiemGK.1923.LO12.mod <- DiemGK.1923.LO12
DiemGK.1924.LO12.mod <- DiemGK.1924.LO12

rownames(DiemGK.1921.LO12.mod)<- abs(as.numeric(rownames(DiemGK.1921.LO12)) -12)
rownames(DiemGK.1922.LO12.mod)<- abs(as.numeric(rownames(DiemGK.1922.LO12)) -11)
rownames(DiemGK.1923.LO12.mod)<- abs(as.numeric(rownames(DiemGK.1923.LO12)) -12)
rownames(DiemGK.1924.LO12.mod)<- abs(as.numeric(rownames(DiemGK.1924.LO12)) -12)

LOW.GK12 <- as.data.frame(bind_rows(DiemGK.1921.LO12.mod,
                                    DiemGK.1922.LO12.mod,
                                    DiemGK.1923.LO12.mod,
                                    DiemGK.1924.LO12.mod,
))

LOW.GK12[is.na(LOW.GK12)] <- 0
as.numeric(row.names(DiemGK.1921.LO12)) -1
LOW.GK12<-as.data.frame(apply(LOW.GK12,2,sum))
LOW.GK12
LOW.GK12$SoCauTLSai <- abs(as.numeric(row.names(LOW.GK12)) - 12)
LOW.GK12$percent <- round(LOW.GK12[,1]/sum(LOW.GK12[,1]),3)
LOW.GK12


LOW.GK21 <- LOR.GK21
LOW.GK21 <- transform(LOW.GK21, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK21$SoCauTLSai <- abs(LOW.GK21$SoCauTLDung - as.numeric(TotalQFrameGK[3,2]))
LOW.GK21 <- LOW.GK21[,c(1,3)]
LOW.GK21
#percentaging
LOW.GK21$percent <- round(LOW.GK21[,1]/sum(LOW.GK21[,1]),3)
LOW.GK21


LOW.GK22 <- LOR.GK22
LOW.GK22 <- transform(LOW.GK22, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK22$SoCauTLSai <- abs(LOW.GK22$SoCauTLDung - as.numeric(TotalQFrameGK[4,2]))
LOW.GK22 <- LOW.GK22[,c(1,3)]

#percentaging
LOW.GK22$percent <- round(LOW.GK22[,1]/sum(LOW.GK22[,1]),3)
LOW.GK22

LOW.GK31 <- LOR.GK31
LOW.GK31 <- transform(LOW.GK31, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.GK31$SoCauTLSai <- abs(LOW.GK31$SoCauTLDung - as.numeric(TotalQFrameGK[5,2]))
LOW.GK31 <- LOW.GK31[,c(1,3)]

#percentaging
LOW.GK31$percent <- round(LOW.GK31[,1]/sum(LOW.GK31[,1]),3)
LOW.GK31

#exception for 1922 code
LOW.GK23 <- LOR.GK23
LOW.GK23 <- transform(LOW.GK23, SoCauTraLoiDung = as.numeric(SoCauTraLoiDung))
LOW.GK23$SoCauTLSai <- abs(as.numeric(LOW.GK23$SoCauTraLoiDung) - 2)
LOW.GK23 <- LOW.GK23[,c(2,4)]

#percentaging
LOW.GK23$percent <- round(LOW.GK23[,1]/sum(LOW.GK23[,1]),3)
LOW.GK23

#Graph ---

GLOW.GK11 <- ggplot(LOW.GK11, aes(x = as.character(LOW.GK11[,2]), y = LOW.GK11[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK11[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 11", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK12 <-  ggplot(LOW.GK12, aes(x = as.character(LOW.GK12[,2]), y = LOW.GK12[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK12[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK21 <- ggplot(LOW.GK21, aes(x = as.character(LOW.GK21[,2]), y = LOW.GK21[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK21[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 21", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK22 <-  ggplot(LOW.GK22, aes(x = as.character(LOW.GK22[,2]), y = LOW.GK22[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK22[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 22", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK31 <- ggplot(LOW.GK31, aes(x = as.character(LOW.GK31[,2]), y = LOW.GK31[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK31[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK23 <- ggplot(LOW.GK23, aes(x = as.character(LOW.GK23[,2]), y = LOW.GK23[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK23[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK12.1.3.4 <- ggplot(LOW.GK12.1921.3.4, aes(x = as.character(LOW.GK12.1921.3.4[,2]), y = LOW.GK12.1921.3.4[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK12.1921.3.4[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12-1921-3-4", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.GK12.2 <- ggplot(LOW.GK12.1922, aes(x = as.character(LOW.GK12.1922[,1]), y = LOW.GK12.1922[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.GK12.1922[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12-1922", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))


GLOW.GK <-ggarrange(GLOW.GK11,GLOW.GK12.2,GLOW.GK12.1.3.4,GLOW.GK12,GLOW.GK21,GLOW.GK22,GLOW.GK23,GLOW.GK31, ncol = 3, nrow = 3)


GLOW.GK

#Problem 9


names(table(CKCDR.1921))

#convert data from problem 6 for reuse

LOW.CK12 <- LOR.CK12
LOW.CK12 <- transform(LOW.CK12, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK12$SoCauTLSai <- abs(LOW.CK12$SoCauTLDung - as.numeric(TotalQFrameCK[1,2]))
LOW.CK12 <- LOW.CK12[,c(1,3)]

#percentaging
LOW.CK12$percent <- round(LOW.CK12[,1]/sum(LOW.CK12[,1]),3)
LOW.CK12

LOW.CK23 <- LOR.CK23
LOW.CK23 <- transform(LOW.CK23, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK23$SoCauTLSai <- abs(LOW.CK23$SoCauTLDung - as.numeric(TotalQFrameCK[2,2]))
LOW.CK23 <- LOW.CK23[,c(1,3)]

#percentaging
LOW.CK23$percent <- round(LOW.CK23[,1]/sum(LOW.CK23[,1]),3)
LOW.CK23

LOW.CK31 <- LOR.CK31
LOW.CK31 <- transform(LOW.CK31, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK31$SoCauTLSai <- abs(LOW.CK31$SoCauTLDung - as.numeric(TotalQFrameCK[3,2]))
LOW.CK31 <- LOW.CK31[,c(1,3)]

#percentaging
LOW.CK31$percent <- round(LOW.CK31[,1]/sum(LOW.CK31[,1]),3)
LOW.CK31

LOW.CK32 <- LOR.CK32
LOW.CK32 <- transform(LOW.CK32, SoCauTLDung = as.numeric(SoCauTLDung))
LOW.CK32$SoCauTLSai <- abs(LOW.CK32$SoCauTLDung - as.numeric(TotalQFrameCK[4,2]))
LOW.CK32 <- LOW.CK32[,c(1,3)]

#percentaging
LOW.CK32$percent <- round(LOW.CK32[,1]/sum(LOW.CK32[,1]),3)
LOW.CK32

#Graph---

GLOW.CK12 <- ggplot(LOW.CK12, aes(x = as.character(LOW.CK12[,2]), y = LOW.CK12[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK12[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK23 <- ggplot(LOW.CK23, aes(x = as.character(LOW.CK23[,2]), y = LOW.CK23[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK23[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK31 <- ggplot(LOW.CK31, aes(x = as.character(LOW.CK31[,2]), y = LOW.CK31[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK31[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK32 <- ggplot(LOW.CK32, aes(x = as.character(LOW.CK32[,2]), y = LOW.CK32[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW.CK32[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 32", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GLOW.CK <-ggarrange(GLOW.CK12,GLOW.CK23,GLOW.CK31,GLOW.CK32, ncol = 3, nrow = 2)

GLOW.CK

#Problem 10 ---

names(table(CKCDR.1921))
names(table(GKCDR.1921))
LoListCK <- data.frame(LO= names(table(CKCDR.1921)))
LoListGK <- data.frame(LO= names(table(GKCDR.1921)))
LoAp<-as.data.frame(table(rbind(LoListCK,LoListGK)))

#Lo appeared in both midterm and final
LoAp[LoAp$Freq == 2,]
TotalQFrameCK[TotalQFrameCK$Var1 == 12,]
TotalQFrameGK[TotalQFrameGK$Var1 == 12,]

#Number of students in mid/final 
nrow(DiemGK)
nrow(DiemCK)
#create data with 361 students(based on final)
#we will deal with 5 last students later

DiemGKCK<- cbind( DiemGK[1:361,],DiemCK)
#Ma de pool
md <- as.numeric(CKCDR[,1])
#add combined made
DiemGKCK$mdCK_GK <- as.numeric(paste(DiemGKCK[,29] ,DiemGKCK[,62], sep = ""))
#rearrange
DiemGKCK <- DiemGKCK[,c(63,1,4:28,33:61)]
colnames(DiemGKCK)[c(3:56)] <- c(1:54)

#rename column
GKCDR.colrename <-GKCDR
CKCDR.colrename <-CKCDR
colnames(GKCDR.colrename)[2:26] <- c(1:25)
colnames(CKCDR.colrename)[2:30] <- c(26:54)


MD1921.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1921.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1921.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1921.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1922.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1922.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1922.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1922.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1923.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1923.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1923.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1923.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1924.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1924.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1924.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1924.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])

ncol(MD1924.1924)

Diem1921.1921<-DiemGKCK[DiemGKCK[1] == 19211921,]
Diem1921.1922<-DiemGKCK[DiemGKCK[1] == 19211922,]
Diem1921.1923<-DiemGKCK[DiemGKCK[1] == 19211923,]
Diem1921.1924<-DiemGKCK[DiemGKCK[1] == 19211924,]
Diem1922.1921<-DiemGKCK[DiemGKCK[1] == 19221921,]
Diem1922.1922<-DiemGKCK[DiemGKCK[1] == 19221922,]
Diem1922.1923<-DiemGKCK[DiemGKCK[1] == 19221923,]
Diem1922.1924<-DiemGKCK[DiemGKCK[1] == 19221924,]
Diem1923.1921<-DiemGKCK[DiemGKCK[1] == 19231921,]
Diem1923.1922<-DiemGKCK[DiemGKCK[1] == 19231922,]
Diem1923.1923<-DiemGKCK[DiemGKCK[1] == 19231923,]
Diem1923.1924<-DiemGKCK[DiemGKCK[1] == 19231924,]
Diem1924.1921<-DiemGKCK[DiemGKCK[1] == 19241921,]
Diem1924.1922<-DiemGKCK[DiemGKCK[1] == 19241922,]
Diem1924.1923<-DiemGKCK[DiemGKCK[1] == 19241923,]
Diem1924.1924<-DiemGKCK[DiemGKCK[1] == 19241924,]

Diem1921.1921 <- apply(bind_rows(MD1921.1921,Diem1921.1921[,c(3:56)]),2,as.numeric)
Diem1921.1922 <- apply(bind_rows(MD1921.1922,Diem1921.1922[,c(3:56)]),2,as.numeric)
Diem1921.1923 <- apply(bind_rows(MD1921.1923,Diem1921.1923[,c(3:56)]),2,as.numeric)
Diem1921.1924 <- apply(bind_rows(MD1921.1924,Diem1921.1924[,c(3:56)]),2,as.numeric)
Diem1922.1921 <- apply(bind_rows(MD1922.1921,Diem1922.1921[,c(3:56)]),2,as.numeric)
Diem1922.1922 <- apply(bind_rows(MD1922.1922,Diem1922.1922[,c(3:56)]),2,as.numeric)
Diem1922.1923 <- apply(bind_rows(MD1922.1923,Diem1922.1923[,c(3:56)]),2,as.numeric)
Diem1922.1924 <- apply(bind_rows(MD1922.1924,Diem1922.1924[,c(3:56)]),2,as.numeric)
Diem1923.1921 <- apply(bind_rows(MD1923.1921,Diem1923.1921[,c(3:56)]),2,as.numeric)
Diem1923.1922 <- apply(bind_rows(MD1923.1922,Diem1923.1922[,c(3:56)]),2,as.numeric)
Diem1923.1923 <- apply(bind_rows(MD1923.1923,Diem1923.1923[,c(3:56)]),2,as.numeric)
Diem1923.1924 <- apply(bind_rows(MD1923.1924,Diem1923.1924[,c(3:56)]),2,as.numeric)
Diem1924.1921 <- apply(bind_rows(MD1924.1921,Diem1924.1921[,c(3:56)]),2,as.numeric)
Diem1924.1922 <- apply(bind_rows(MD1924.1922,Diem1924.1922[,c(3:56)]),2,as.numeric)
Diem1924.1923 <- apply(bind_rows(MD1924.1923,Diem1924.1923[,c(3:56)]),2,as.numeric)
Diem1924.1924 <- apply(bind_rows(MD1924.1924,Diem1924.1924[,c(3:56)]),2,as.numeric)

#last 5 students did midterm exam but did not do final exam 
last5.1922 <- DiemGK[362:366,]
last5.1923 <- DiemGK[362:366,]
last5.1924 <- DiemGK[362:366,]
last5.1922 <- last5.1922 %>% filter(MADE==1922) %>% select(X1:X25) %>% apply(2,as.numeric)
last5.1923 <- last5.1923 %>% filter(MADE==1923) %>% select(X1:X25) %>% apply(2,as.numeric)
last5.1924 <- last5.1924 %>% filter(MADE==1924) %>% select(X1:X25) %>% apply(2,as.numeric)

last5.1922 <- as.data.frame(rbind(GKCDR.1922,last5.1922))
last5.1923 <- as.data.frame(rbind(GKCDR.1923,last5.1923))
last5.1924 <- as.data.frame(rbind(GKCDR.1924,last5.1924))


#Learning outcome 12

LO12.last5.1922 <- table(apply(last5.1922[-1,last5.1922[1,]==12],1,sum))
LO12.last5.1923 <- table(apply(last5.1923[-1,last5.1923[1,]==12],1,sum))
LO12.last5.1924 <- table(apply(last5.1924[-1,last5.1924[1,]==12],1,sum))

rownames(LO12.last5.1922) <- abs(as.numeric(rownames(LO12.last5.1922)) - ncol(GKCDR[GKCDR[,1]==1922,GKCDR[2,] == 12]))
rownames(LO12.last5.1923) <- abs(as.numeric(rownames(LO12.last5.1923)) - ncol(GKCDR[GKCDR[,1]==1923,GKCDR[2,] == 12]))
rownames(LO12.last5.1924) <- abs(as.numeric(rownames(LO12.last5.1924)) - ncol(GKCDR[GKCDR[,1]==1924,GKCDR[2,] == 12]))


LO12.Diem1921.1921 <- table(apply(Diem1921.1921[-1,Diem1921.1921[1,]==12],1,sum))
LO12.Diem1921.1922 <- table(apply(Diem1921.1922[-1,Diem1921.1922[1,]==12],1,sum))
LO12.Diem1921.1923 <- table(apply(Diem1921.1923[-1,Diem1921.1923[1,]==12],1,sum))
LO12.Diem1921.1924 <- table(apply(Diem1921.1924[-1,Diem1921.1924[1,]==12],1,sum))
LO12.Diem1922.1921 <- table(apply(Diem1922.1921[-1,Diem1922.1921[1,]==12],1,sum))
LO12.Diem1922.1922 <- table(apply(Diem1922.1922[-1,Diem1922.1922[1,]==12],1,sum))
LO12.Diem1922.1923 <- table(apply(Diem1922.1923[-1,Diem1922.1923[1,]==12],1,sum))
LO12.Diem1922.1924 <- table(apply(Diem1922.1924[-1,Diem1922.1924[1,]==12],1,sum))
LO12.Diem1923.1921 <- table(apply(Diem1923.1921[-1,Diem1923.1921[1,]==12],1,sum))
LO12.Diem1923.1922 <- table(apply(Diem1923.1922[-1,Diem1923.1922[1,]==12],1,sum))
LO12.Diem1923.1923 <- table(apply(Diem1923.1923[-1,Diem1923.1923[1,]==12],1,sum))
LO12.Diem1923.1924 <- table(apply(Diem1923.1924[-1,Diem1923.1924[1,]==12],1,sum))
LO12.Diem1924.1921 <- table(apply(Diem1924.1921[-1,Diem1924.1921[1,]==12],1,sum))
LO12.Diem1924.1922 <- table(apply(Diem1924.1922[-1,Diem1924.1922[1,]==12],1,sum))
LO12.Diem1924.1923 <- table(apply(Diem1924.1923[-1,Diem1924.1923[1,]==12],1,sum))
LO12.Diem1924.1924 <- table(apply(Diem1924.1924[-1,Diem1924.1924[1,]==12],1,sum))

#make count right answer -> wrong answer
rownames(LO12.Diem1921.1921) <- abs(as.numeric(rownames(LO12.Diem1921.1921)) - ncol(MD1921.1921[,MD1921.1921[1,]==12]))
rownames(LO12.Diem1921.1922) <- abs(as.numeric(rownames(LO12.Diem1921.1922)) - ncol(MD1921.1922[,MD1921.1922[1,]==12]))
rownames(LO12.Diem1921.1923) <- abs(as.numeric(rownames(LO12.Diem1921.1923)) - ncol(MD1921.1923[,MD1921.1923[1,]==12]))
rownames(LO12.Diem1921.1924) <- abs(as.numeric(rownames(LO12.Diem1921.1924)) - ncol(MD1921.1924[,MD1921.1924[1,]==12]))

#different is here
rownames(LO12.Diem1922.1921) <- abs(as.numeric(rownames(LO12.Diem1922.1921)) - ncol(MD1922.1921[,MD1922.1921[1,]==12]))
rownames(LO12.Diem1922.1922) <- abs(as.numeric(rownames(LO12.Diem1922.1922)) - ncol(MD1922.1922[,MD1922.1922[1,]==12]))
rownames(LO12.Diem1922.1923) <- abs(as.numeric(rownames(LO12.Diem1922.1923)) - ncol(MD1922.1923[,MD1922.1923[1,]==12]))
rownames(LO12.Diem1922.1924) <- abs(as.numeric(rownames(LO12.Diem1922.1924)) - ncol(MD1922.1924[,MD1922.1924[1,]==12]))


#-----------------

rownames(LO12.Diem1923.1921) <- abs(as.numeric(rownames(LO12.Diem1923.1921)) - ncol(MD1923.1921[,MD1923.1921[1,]==12]))
rownames(LO12.Diem1923.1922) <- abs(as.numeric(rownames(LO12.Diem1923.1922)) - ncol(MD1923.1922[,MD1923.1922[1,]==12]))
rownames(LO12.Diem1923.1923) <- abs(as.numeric(rownames(LO12.Diem1923.1923)) - ncol(MD1923.1923[,MD1923.1923[1,]==12]))
rownames(LO12.Diem1923.1924) <- abs(as.numeric(rownames(LO12.Diem1923.1924)) - ncol(MD1923.1924[,MD1923.1924[1,]==12]))
rownames(LO12.Diem1924.1921) <- abs(as.numeric(rownames(LO12.Diem1924.1921)) - ncol(MD1924.1921[,MD1924.1921[1,]==12]))
rownames(LO12.Diem1924.1922) <- abs(as.numeric(rownames(LO12.Diem1924.1922)) - ncol(MD1924.1922[,MD1924.1922[1,]==12]))
rownames(LO12.Diem1924.1923) <- abs(as.numeric(rownames(LO12.Diem1924.1923)) - ncol(MD1924.1923[,MD1924.1923[1,]==12]))
rownames(LO12.Diem1924.1924) <- abs(as.numeric(rownames(LO12.Diem1924.1924)) - ncol(MD1924.1924[,MD1924.1924[1,]==12]))




LOW12 <- as.data.frame(bind_rows(LO12.Diem1921.1921,
                                 LO12.Diem1921.1922,
                                 LO12.Diem1921.1923,
                                 LO12.Diem1921.1924,
                                 LO12.Diem1922.1921,
                                 LO12.Diem1922.1922,
                                 LO12.Diem1922.1923,
                                 LO12.Diem1922.1924,
                                 LO12.Diem1923.1921,
                                 LO12.Diem1923.1922,
                                 LO12.Diem1923.1923,
                                 LO12.Diem1923.1924,
                                 LO12.Diem1924.1921,
                                 LO12.Diem1924.1922,
                                 LO12.Diem1924.1923,
                                 LO12.Diem1924.1924,
                                 LO12.last5.1922,
                                 LO12.last5.1923,
                                 LO12.last5.1924
))

LOW12[is.na(LOW12)] <- 0

LOW12 <- as.data.frame(apply(LOW12,2,sum))
LOW12$SoCauTLSai <- as.numeric(row.names(LOW12))

LOW12$percent <- round(LOW12[,1]/sum(LOW12[,1]),3)

#graph----

GLOW12 <- ggplot(LOW12, aes(x = as.character(LOW12[,2]), y = LOW12[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW12[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))


#Learning outcome 23

#expected to be 1

LO23.last5.1922 <- table(last5.1922[-1,last5.1922[1,]==23])
#mini conclude: 1 question related to LO23 on Made1922 was answer by 1 student
#and his/her answer was right -> LO23.last5.1922 = 0.(no need to add to combined)

LO23.Diem1921.1921 <- table(apply(Diem1921.1921[-1,Diem1921.1921[1,]==23],1,sum))
LO23.Diem1921.1922 <- table(apply(Diem1921.1922[-1,Diem1921.1922[1,]==23],1,sum))
LO23.Diem1921.1923 <- table(apply(Diem1921.1923[-1,Diem1921.1923[1,]==23],1,sum))
LO23.Diem1921.1924 <- table(apply(Diem1921.1924[-1,Diem1921.1924[1,]==23],1,sum))
LO23.Diem1922.1921 <- table(apply(Diem1922.1921[-1,Diem1922.1921[1,]==23],1,sum))
LO23.Diem1922.1922 <- table(apply(Diem1922.1922[-1,Diem1922.1922[1,]==23],1,sum))
LO23.Diem1922.1923 <- table(apply(Diem1922.1923[-1,Diem1922.1923[1,]==23],1,sum))
LO23.Diem1922.1924 <- table(apply(Diem1922.1924[-1,Diem1922.1924[1,]==23],1,sum))
LO23.Diem1923.1921 <- table(apply(Diem1923.1921[-1,Diem1923.1921[1,]==23],1,sum))
LO23.Diem1923.1922 <- table(apply(Diem1923.1922[-1,Diem1923.1922[1,]==23],1,sum))
LO23.Diem1923.1923 <- table(apply(Diem1923.1923[-1,Diem1923.1923[1,]==23],1,sum))
LO23.Diem1923.1924 <- table(apply(Diem1923.1924[-1,Diem1923.1924[1,]==23],1,sum))
LO23.Diem1924.1921 <- table(apply(Diem1924.1921[-1,Diem1924.1921[1,]==23],1,sum))
LO23.Diem1924.1922 <- table(apply(Diem1924.1922[-1,Diem1924.1922[1,]==23],1,sum))
LO23.Diem1924.1923 <- table(apply(Diem1924.1923[-1,Diem1924.1923[1,]==23],1,sum))
LO23.Diem1924.1924 <- table(apply(Diem1924.1924[-1,Diem1924.1924[1,]==23],1,sum))

#make count right answer -> wrong answer
rownames(LO23.Diem1921.1921) <- abs(as.numeric(rownames(LO23.Diem1921.1921)) - ncol(MD1921.1921[,MD1921.1921[1,]==23]))
rownames(LO23.Diem1921.1922) <- abs(as.numeric(rownames(LO23.Diem1921.1922)) - ncol(MD1921.1922[,MD1921.1922[1,]==23]))
rownames(LO23.Diem1921.1923) <- abs(as.numeric(rownames(LO23.Diem1921.1923)) - ncol(MD1921.1923[,MD1921.1923[1,]==23]))
rownames(LO23.Diem1921.1924) <- abs(as.numeric(rownames(LO23.Diem1921.1924)) - ncol(MD1921.1924[,MD1921.1924[1,]==23]))

#different is here
rownames(LO23.Diem1922.1921) <- abs(as.numeric(rownames(LO23.Diem1922.1921)) - ncol(MD1922.1921[,MD1922.1921[1,]==23]))
rownames(LO23.Diem1922.1922) <- abs(as.numeric(rownames(LO23.Diem1922.1922)) - ncol(MD1922.1922[,MD1922.1922[1,]==23]))
rownames(LO23.Diem1922.1923) <- abs(as.numeric(rownames(LO23.Diem1922.1923)) - ncol(MD1922.1923[,MD1922.1923[1,]==23]))
rownames(LO23.Diem1922.1924) <- abs(as.numeric(rownames(LO23.Diem1922.1924)) - ncol(MD1922.1924[,MD1922.1924[1,]==23]))


#-----------------

rownames(LO23.Diem1923.1921) <- abs(as.numeric(rownames(LO23.Diem1923.1921)) - ncol(MD1923.1921[,MD1923.1921[1,]==23]))
rownames(LO23.Diem1923.1922) <- abs(as.numeric(rownames(LO23.Diem1923.1922)) - ncol(MD1923.1922[,MD1923.1922[1,]==23]))
rownames(LO23.Diem1923.1923) <- abs(as.numeric(rownames(LO23.Diem1923.1923)) - ncol(MD1923.1923[,MD1923.1923[1,]==23]))
rownames(LO23.Diem1923.1924) <- abs(as.numeric(rownames(LO23.Diem1923.1924)) - ncol(MD1923.1924[,MD1923.1924[1,]==23]))
rownames(LO23.Diem1924.1921) <- abs(as.numeric(rownames(LO23.Diem1924.1921)) - ncol(MD1924.1921[,MD1924.1921[1,]==23]))
rownames(LO23.Diem1924.1922) <- abs(as.numeric(rownames(LO23.Diem1924.1922)) - ncol(MD1924.1922[,MD1924.1922[1,]==23]))
rownames(LO23.Diem1924.1923) <- abs(as.numeric(rownames(LO23.Diem1924.1923)) - ncol(MD1924.1923[,MD1924.1923[1,]==23]))
rownames(LO23.Diem1924.1924) <- abs(as.numeric(rownames(LO23.Diem1924.1924)) - ncol(MD1924.1924[,MD1924.1924[1,]==23]))



LOW23 <- as.data.frame(bind_rows(LO23.Diem1921.1921,
                                 LO23.Diem1921.1922,
                                 LO23.Diem1921.1923,
                                 LO23.Diem1921.1924,
                                 LO23.Diem1922.1921,
                                 LO23.Diem1922.1922,
                                 LO23.Diem1922.1923,
                                 LO23.Diem1922.1924,
                                 LO23.Diem1923.1921,
                                 LO23.Diem1923.1922,
                                 LO23.Diem1923.1923,
                                 LO23.Diem1923.1924,
                                 LO23.Diem1924.1921,
                                 LO23.Diem1924.1922,
                                 LO23.Diem1924.1923,
                                 LO23.Diem1924.1924,

                                 
))

LOW23[is.na(LOW23)] <- 0

LOW23 <- as.data.frame(apply(LOW23,2,sum))
LOW23$SoCauTLSai <- as.numeric(row.names(LOW23))

LOW23$percent <- round(LOW23[,1]/sum(LOW23[,1]),3)
#graph----
GLOW23 <- ggplot(LOW23, aes(x = as.character(LOW23[,2]), y = LOW23[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW23[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

#Learning outcome 31

LO31.last5.1922 <- table(apply(last5.1922[-1,last5.1922[1,]==31],1,sum))
LO31.last5.1923 <- table(apply(last5.1923[-1,last5.1923[1,]==31],1,sum))
LO31.last5.1924 <- table(apply(last5.1924[-1,last5.1924[1,]==31],1,sum))

rownames(LO31.last5.1922) <- abs(as.numeric(rownames(LO31.last5.1922)) - ncol(GKCDR[GKCDR[,1]==1922,GKCDR[2,] == 31]))
rownames(LO31.last5.1923) <- abs(as.numeric(rownames(LO31.last5.1923)) - ncol(GKCDR[GKCDR[,1]==1923,GKCDR[2,] == 31]))
rownames(LO31.last5.1924) <- abs(as.numeric(rownames(LO31.last5.1924)) - ncol(GKCDR[GKCDR[,1]==1924,GKCDR[2,] == 31]))


LO31.Diem1921.1921 <- table(apply(Diem1921.1921[-1,Diem1921.1921[1,]==31],1,sum))
LO31.Diem1921.1922 <- table(apply(Diem1921.1922[-1,Diem1921.1922[1,]==31],1,sum))
LO31.Diem1921.1923 <- table(apply(Diem1921.1923[-1,Diem1921.1923[1,]==31],1,sum))
LO31.Diem1921.1924 <- table(apply(Diem1921.1924[-1,Diem1921.1924[1,]==31],1,sum))
LO31.Diem1922.1921 <- table(apply(Diem1922.1921[-1,Diem1922.1921[1,]==31],1,sum))
LO31.Diem1922.1922 <- table(apply(Diem1922.1922[-1,Diem1922.1922[1,]==31],1,sum))
LO31.Diem1922.1923 <- table(apply(Diem1922.1923[-1,Diem1922.1923[1,]==31],1,sum))
LO31.Diem1922.1924 <- table(apply(Diem1922.1924[-1,Diem1922.1924[1,]==31],1,sum))
LO31.Diem1923.1921 <- table(apply(Diem1923.1921[-1,Diem1923.1921[1,]==31],1,sum))
LO31.Diem1923.1922 <- table(apply(Diem1923.1922[-1,Diem1923.1922[1,]==31],1,sum))
LO31.Diem1923.1923 <- table(apply(Diem1923.1923[-1,Diem1923.1923[1,]==31],1,sum))
LO31.Diem1923.1924 <- table(apply(Diem1923.1924[-1,Diem1923.1924[1,]==31],1,sum))
LO31.Diem1924.1921 <- table(apply(Diem1924.1921[-1,Diem1924.1921[1,]==31],1,sum))
LO31.Diem1924.1922 <- table(apply(Diem1924.1922[-1,Diem1924.1922[1,]==31],1,sum))
LO31.Diem1924.1923 <- table(apply(Diem1924.1923[-1,Diem1924.1923[1,]==31],1,sum))
LO31.Diem1924.1924 <- table(apply(Diem1924.1924[-1,Diem1924.1924[1,]==31],1,sum))

#make count right answer -> wrong answer
rownames(LO31.Diem1921.1921) <- abs(as.numeric(rownames(LO31.Diem1921.1921)) - ncol(MD1921.1921[,MD1921.1921[1,]==31]))
rownames(LO31.Diem1921.1922) <- abs(as.numeric(rownames(LO31.Diem1921.1922)) - ncol(MD1921.1922[,MD1921.1922[1,]==31]))
rownames(LO31.Diem1921.1923) <- abs(as.numeric(rownames(LO31.Diem1921.1923)) - ncol(MD1921.1923[,MD1921.1923[1,]==31]))
rownames(LO31.Diem1921.1924) <- abs(as.numeric(rownames(LO31.Diem1921.1924)) - ncol(MD1921.1924[,MD1921.1924[1,]==31]))

rownames(LO31.Diem1922.1921) <- abs(as.numeric(rownames(LO31.Diem1922.1921)) - ncol(MD1922.1921[,MD1922.1921[1,]==31]))
rownames(LO31.Diem1922.1922) <- abs(as.numeric(rownames(LO31.Diem1922.1922)) - ncol(MD1922.1922[,MD1922.1922[1,]==31]))
rownames(LO31.Diem1922.1923) <- abs(as.numeric(rownames(LO31.Diem1922.1923)) - ncol(MD1922.1923[,MD1922.1923[1,]==31]))
rownames(LO31.Diem1922.1924) <- abs(as.numeric(rownames(LO31.Diem1922.1924)) - ncol(MD1922.1924[,MD1922.1924[1,]==31]))


#-----------------

rownames(LO31.Diem1923.1921) <- abs(as.numeric(rownames(LO31.Diem1923.1921)) - ncol(MD1923.1921[,MD1923.1921[1,]==31]))
rownames(LO31.Diem1923.1922) <- abs(as.numeric(rownames(LO31.Diem1923.1922)) - ncol(MD1923.1922[,MD1923.1922[1,]==31]))
rownames(LO31.Diem1923.1923) <- abs(as.numeric(rownames(LO31.Diem1923.1923)) - ncol(MD1923.1923[,MD1923.1923[1,]==31]))
rownames(LO31.Diem1923.1924) <- abs(as.numeric(rownames(LO31.Diem1923.1924)) - ncol(MD1923.1924[,MD1923.1924[1,]==31]))
rownames(LO31.Diem1924.1921) <- abs(as.numeric(rownames(LO31.Diem1924.1921)) - ncol(MD1924.1921[,MD1924.1921[1,]==31]))
rownames(LO31.Diem1924.1922) <- abs(as.numeric(rownames(LO31.Diem1924.1922)) - ncol(MD1924.1922[,MD1924.1922[1,]==31]))
rownames(LO31.Diem1924.1923) <- abs(as.numeric(rownames(LO31.Diem1924.1923)) - ncol(MD1924.1923[,MD1924.1923[1,]==31]))
rownames(LO31.Diem1924.1924) <- abs(as.numeric(rownames(LO31.Diem1924.1924)) - ncol(MD1924.1924[,MD1924.1924[1,]==31]))




LOW31 <- as.data.frame(bind_rows(LO31.Diem1921.1921,
                                 LO31.Diem1921.1922,
                                 LO31.Diem1921.1923,
                                 LO31.Diem1921.1924,
                                 LO31.Diem1922.1921,
                                 LO31.Diem1922.1922,
                                 LO31.Diem1922.1923,
                                 LO31.Diem1922.1924,
                                 LO31.Diem1923.1921,
                                 LO31.Diem1923.1922,
                                 LO31.Diem1923.1923,
                                 LO31.Diem1923.1924,
                                 LO31.Diem1924.1921,
                                 LO31.Diem1924.1922,
                                 LO31.Diem1924.1923,
                                 LO31.Diem1924.1924,
                                 LO31.last5.1922,
                                 LO31.last5.1923,
                                 LO31.last5.1924
))

LOW31[is.na(LOW31)] <- 0

LOW31 <- as.data.frame(apply(LOW31,2,sum))
LOW31$SoCauTLSai <- as.numeric(row.names(LOW31))

LOW31$percent <- round(LOW31[,1]/sum(LOW31[,1]),3)

#graph----

GLOW31 <- ggplot(LOW31, aes(x = as.character(LOW31[,2]), y = LOW31[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW31[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 31", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))




GW <- ggarrange(GLOW.GK11, GLOW12, GLOW.GK21, GLOW.GK22, GLOW23, GLOW31, GLOW.CK32, ncol=3, nrow = 3)
GW