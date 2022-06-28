
#Libraries used: "dplyr" 
library(plyr)
library(dplyr)
library(openxlsx)
library(randomizr)
library(lubridate)

#Setting Working directory or file directory. This allows you to call only the data set name in subsequent code, and would allow other users to only change their file path/directory
setwd("D:/code/git/sabo_demo/sabo_files")
getwd()
list.files() #list.files() function allows you to see what files are in your directory. 

#Import diagnosis table
data_dx <- read.csv(file="APCD_Stage_dbo_dx.csv", na.strings=c("","NA"))
#View(data_dx)
data_dx <- data_dx[1:676,]

#Import eligibility table
data_el <- read.csv(file="APCD_Stage_dbo_eligibility.csv", na.strings=c("","NA"))
#View(data_el)

#Import procedure table
data_pr <- read.csv(file="APCD_Stage_dbo_procTab.csv", na.strings=c("","NA"))
#View(data_pr)

#Step 1: Identify Children Diagnosed with Bronchiolitis
  #Attempt 1: Find literal code
  data_dx$bronch1 <- ifelse(data_dx$ICD == "J21",1,0)
  #View(data_dx)
  
  #Attempt 2: Search for Stub
  list1<-grep("J21",data_dx$ICD)
  #print(list1)
  data_dx$bronch2<-0 #Create empty indicator variable for bronchiolitis diagnosis
  data_dx$bronch2[list1]<-1 #set indicator to 1 if bronchiolitis diagnosis is present
  #View(data_dx)
  
  #Reduce data to only those patients diagnosed with bronchiolitis
  data_dx_red <- data_dx[data_dx$bronch2==1,] 
  #View(data_dx_red)
  
  #Sort and grab only first incidence
  data_dx_red <- data_dx_red[order(data_dx_red$personId,data_dx_red$incurredDate),]
  #View(data_dx_red)
  data_dx_red_nr <- data_dx_red[!duplicated(data_dx_red$personId),]
  #View(data_dx_red_nr)
  
#Step 2: Merge with Eligibility Data to find those children under 2
  data_dx_el <- merge(data_dx_red_nr,data_el)
  #View(data_dx_el)
  #Reduce Redundant Rows
  data_dx_el_nr <- data_dx_el[!duplicated(data_dx_el$personId),]
  #View(data_dx_el_nr)
  
  #Determine # of Days between birth and diagnosis
    #Change Incurred Date into Years
      data_dx_el_nr$diag_year <- as.numeric(format(as.Date(data_dx_el_nr$incurredDate,format="%m/%d/%Y"),format="%Y"))
      #View(data_dx_el_nr)
    #Substract Birth Year from Diagnosis Year
      data_dx_el_nr$diff <- data_dx_el_nr$diag_year - data_dx_el_nr$birthyear
      #View(data_dx_el_nr)
      
    #Select only children with diagnosis before 2
      data_dx_el_red <- data_dx_el_nr[data_dx_el_nr$diff<=2,]
      #View(data_dx_el_red)
      
#Step 3: Determine Children with Chest Radiographs
      #Attempt 1: Find literal codes
      data_pr$cr1 <- ifelse(data_pr$PROCICD == "71045" || data_pr$PROCICD == "71046" || data_pr$PROCICD == "71047" || data_pr$PROCICD == "71048" ||
                             data_pr$PROCICD == "71010" || data_pr$PROCICD == "71015" || data_pr$PROCICD == "71020" || data_pr$PROCICD == "71021" ||
                             data_pr$PROCICD == "71022" || data_pr$PROCICD == "71023" || data_pr$PROCICD == "71030" || data_pr$PROCICD == "71035" ,1,0)
      #View(data_pr)
  
      #Attempt 2: Search for Stub
      list1<-grep("71045",data_pr$PROCICD)
      list2<-grep("71046",data_pr$PROCICD)
      list3<-grep("71047",data_pr$PROCICD)
      list4<-grep("71048",data_pr$PROCICD)
      list5<-grep("71010",data_pr$PROCICD)
      list6<-grep("71015",data_pr$PROCICD)
      list7<-grep("71020",data_pr$PROCICD)
      list8<-grep("71021",data_pr$PROCICD)
      list9<-grep("71022",data_pr$PROCICD)
      list10<-grep("71023",data_pr$PROCICD)
      list11<-grep("71030",data_pr$PROCICD)
      list12<-grep("71035",data_pr$PROCICD)
      list <- c(list1,list2,list3,list4,list5,list6,list7,list8,list9,list10,list11,list12)
            #print(list)
      data_pr$cr2<-0 #Create empty indicator variable for bronchiolitis diagnosis
      data_pr$cr2[list]<-1 #set indicator to 1 if bronchiolitis diagnosis is present
      #View(data_pr)
      
      #Reduce to only those with relevant CPT
        data_pr_red <- data_pr[data_pr$cr2==1,] 
        #View(data_pr_red)
        
      #Merge CPT table with merged dx and eligibility table
        data_fin <- merge(x=data_dx_el_red,y=data_pr,by="personId",all.x=TRUE)
        #View(data_fin)



