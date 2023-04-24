library(readr) 
abd<-read.csv("~/Downloads/Abandoned.csv", header=T, na.strings="")
rs<-read.csv("~/Downloads/Reservation.csv", header=T, na.strings="")
#checking randomness of Test_control variable
abd$TestingVariable  <- NA
abd$TestingVariable
abd$TestingVariable[abd$Test_Control == "test"] <- 1
abd$TestingVariable[abd$Test_Control == "control"] <- 0
summary(abd$TestingVariable)
mean(abd$TestingVariable)
sd(abd$TestingVariable)
#summary statistics for this Test_variable by blocking on States
abd$AddressVariable <- 0
abd$AddressVariable[abd$Address != ""] <- 1
summary(abd$TestingVariable[abd$AddressVariable == 1])
mean(abd$TestingVariable[abd$AddressVariable == 1])
sd(abd$TestingVariable[abd$AddressVariable == 1])
#match Email
match_email=abd$Email[complete.cases(abd$Email)] %in% rs$Email[complete.cases(rs$Email)] 
abd$match_email <-0
abd$match_email[complete.cases(abd$Email)] <- 1* match_email
sum(abd$match_email)  
#match Incoming Number
match_Incoming_Number=abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]  
abd$match_Incoming_Number <-0
abd$match_Incoming_Number[complete.cases(abd$Incoming_Phone)] <- 1* match_Incoming_Number
sum(abd$match_Incoming_Number)
#Match_Contact
match_Contact_Phone=abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]  
abd$match_Contact_Phone <-0
abd$match_Contact_Phone[complete.cases(abd$Contact_Phone)] <- 1* match_Contact_Phone
sum(abd$match_Contact_Phone)
#Match_Incoming_contact
match_Incoming_Contact=abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]  
abd$match_Incoming_Contact <-0
abd$match_Incoming_Contact[complete.cases(abd$Contact_Phone)] <- 1* match_Incoming_Contact
sum(abd$match_Incoming_Contact)
#match contact Incoming
match_Contact_Incoming=abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]  
abd$match_Contact_Incoming <-0
abd$match_Contact_Incoming[complete.cases(abd$Incoming_Phone)] <- 1* match_Contact_Incoming
sum(abd$match_Contact_Incoming)
All_Matches<-abd$match_email | abd$match_Incoming_Number | abd$match_Incoming_Contact | abd$match_Contact_Phone | abd$match_Contact_Incoming
abd_noduplicates <- abd[All_Matches,]
abd_noduplicates
#Removed duplicates using the variables
abd_noduplicates <- abd_noduplicates[!duplicated(abd_noduplicates[,c("Email")],incomparables = NA),]
abd_noduplicates <- abd_noduplicates[!duplicated(abd_noduplicates[,c("Contact_Phone")],incomparables = NA),]
abd_noduplicates <- abd_noduplicates[!duplicated(abd_noduplicates[,c("Incoming_Phone")],incomparables = NA),]
abd_FirstName_nodupe <- duplicated(abd_noduplicates[,c("Last_Name")],incomparables = NA)
abd_LastName_nodupe <- duplicated(abd_noduplicates[,c("First_Name")],incomparables = NA)
abd_Address_nodupe<- duplicated(abd_noduplicates[,c("Address")],incomparables = NA)
abd_Zipcode_nodupe<-duplicated(abd_noduplicates[,c("Zipcode")],incomparables=NA)
abd_noduplicates<-abd_noduplicates[!(abd_FirstName_nodupe & abd_LastName_nodupe & abd_Address_nodupe & abd_Zipcode_nodupe),]
abd_noduplicates
# Purchased Outcome in original dataset
abd$Purchased <- 0
abd$Purchased[as.numeric(row.names(abd_noduplicates))] <- 1
#logical dummies for variables
abd$email1 <- 1*complete.cases(abd$Email)
abd$Address1 <- 1*complete.cases(abd$Address)
abd$Zipcode1<-1*complete.cases(abd$Zipcode)
abd$TestControl<-1*(abd$Test_Control=='test')
table(abd$Purchased,abd$TestControl)
#Analysis using Address variable for 5 states
abd_states<-data.frame(subset(abd,abd$Address!=""))
table(abd_states$Purchased, abd_states$TestControl)
abd_OH<-data.frame(subset(abd,abd$Address=="OH"))
table(abd_OH$Purchased, abd_OH$TestControl)
abd_NY<-data.frame(subset(abd,abd$Address=="NY"))
table(abd_NY$Purchased, abd_NY$TestControl)
abd_FL<-data.frame(subset(abd,abd$Address=="FL"))
table(abd_FL$Purchased, abd_FL$TestControl)
abd_IL<-data.frame(subset(abd,abd$Address=="IL"))
table(abd_IL$Purchased, abd_IL$TestControl)
abd_AZ<-data.frame(subset(abd,abd$Address=="AZ"))
table(abd_AZ$Purchased, abd_AZ$TestControl)
#Forming the excel sheet all data
install.packages("writexl")
library("writexl")
uid <- paste0("ASDFG", formatC(1:8442, width = 10, format = "d", flag = "0"))
head(uid)
abd_Excel<-data.frame(uid,abd$Purchased,abd$email1,abd$Address1,abd$TestControl)
colnames(abd_Excel)<-c("CustomerID","Outcome","D_Email","D_State","Test_Variable")
abd_Cleaned<-write_xlsx(abd_Excel, "~/Downloads/midtermproject.xlsx")
library("readxl")
abd_Cleaned<-read_excel("~/Downloads/midtermproject.xlsx")
#Excel sheet for 401 records(only purchased customers)
Purchased_data <- subset(abd_Cleaned,abd_Cleaned$Outcome=="1")
write_xlsx(Purchased_data, "~/Downloads/purchasedcustomer_midterm.xlsx")
#T Test
Anova<-aov(Outcome~Test_Variable, data=abd_Cleaned)
summary(Anova)
coef(Anova)
Anova2<-aov(Outcome~Test_Variable+D_Email+D_State, data=abd_Cleaned)
summary(Anova2)
coef(Anova2)
Anova3<-aov(Outcome~Test_Variable+ Test_Variable*D_Email, data=abd_Cleaned)
summary(Anova3)
coef(Anova3)
t.test(abd_Cleaned$Outcome,abd_Cleaned$Test_Variable)
#linear Regression
out1<- lm(Outcome~Test_Variable, data=abd_Cleaned)
summary(out1)
out2<-lm(Outcome~Test_Variable+D_Email+D_State, data=abd_Cleaned)
summary(out2)
out3<-lm(Outcome~Test_Variable+ Test_Variable*D_Email, data=abd_Cleaned)
summary(out3)
out4<-lm(Outcome~Test_Variable+Test_Variable*D_State, data=abd_Cleaned)
summary(out4)
out5<-lm(Outcome~Test_Variable+ Test_Variable*D_Email+ Test_Variable*D_State, data=abd_Cleaned)
summary(out5)
install.packages("stargazer")
library("stargazer")
stargazer(out1,out2,out3,out4,out5, type="html", out="Output.htm")
AIC(out1,out2,out3,out4,out5)

#logistic regression analysis code
install.packages("corrplot")
library("corrplot")
c = cor(abd_Cleaned[,2:5])
corrplot(c)
c
out6= glm(Outcome~Test_Variable,abd_Cleaned, family=binomial())
out7= glm(Outcome~Test_Variable+D_State+D_Email,abd_Cleaned, family = binomial())
out8= glm(Outcome~Test_Variable+Test_Variable*D_State + Test_Variable*D_Email,abd_Cleaned, family =binomial())
library(stargazer)
stargazer(out6,out7,out8, type="html",out="gmlregression.htm")
AIC(out6,out7,out8)
#compute confusion matrix
Y_pred1 = 1*(predict(out6, type="response")>0.05)
Y_pred2 = 1*(predict(out7, type="response")>0.05)
Y_pred3 = 1*(predict(out8, type="response")>0.05)
table(Y_pred1,abd_Cleaned$Outcome)
table(Y_pred2,abd_Cleaned$Outcome)
table(Y_pred3,abd_Cleaned$Outcome)

