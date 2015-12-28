  setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#importing all supporting custom methods
  source("Methods.R")

#Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")

  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_ECLIPSE_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_ECLIPSE_SOURCE_CODE,"..\\Output\\PreAnalysis\\MSR_ECLIPSE_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  pdf("..\\Output\\PreAnalysis\\MSR_ECLIPSE_SOURCE_CODE_HIST.pdf")
  hist(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  dev.off()
  #Summary Descriptive Statistics for eclipse
  print("Summary Descriptive Statistics for eclipse")
  library(psych)
  e<-describe(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  write.table(x=e,"..\\Output\\PreAnalysis\\Stat_eclipse.csv",sep = ",")
  closeConnection(conn)
  
  

#Creating MySql connection for MSR_OPENSTACK_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_OPENSTACK_SOURCE_CODE")
  
  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_OPENSTACK_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_OPENSTACK_SOURCE_CODE,"..\\Output\\PreAnalysis\\MSR_OPENSTACK_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  pdf("..\\Output\\PreAnalysis\\MSR_OPENSTACK_SOURCE_CODE_HIST.pdf")
  hist(MSR_OPENSTACK_SOURCE_CODE$number_of_unique_author)  
  dev.off()
  #Summary Descriptive Statistics for Openstack
  print("Summary Descriptive Statistics for openstack")
  d<-describe(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  write.table(x=d,"..\\Output\\PreAnalysis\\Stat_openstack.csv",sep = ",")
  closeConnection(conn)

  
#----------------------------------------------  
#list of each of the Eclipse repos, with the number of files in each repo that have been changed by at least two people
  
  #importing all supporting custom methods
  source("Methods.R")
  
  #Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")
  
  #Finding out the repo list
  str1<-"select id from repositories"
  
  rs<-executeQuery(conn,str1)
  repoID<- fetch(rs, n = -1)
  
  repoFileCount<-data.frame()
  
  for(row in 1:nrow(repoID)){    
    
    str1<-paste("select count(*) from (select file_id,count(*) from (select distinct A.author_id,B.file_id from (SELECT id as commit_id,author_id,repository_id FROM `scmlog` where repository_id=",repoID[row,1],")A natural join actions B)C group by file_id having count(*)>1)D",sep='')  
    
    rs<-executeQuery(conn,str1)
    noOfFiles<- fetch(rs, n = -1)
    
    repoFile<-data.frame(repo=repoID[row,1],filecount=noOfFiles[1][1])
    repoFileCount<-rbind(repoFileCount,repoFile)    
  }
  
  write.csv(repoFileCount, file = "../Output/PreAnalysis/RepoBasisFileCount.csv",row.names=FALSE) 
  
  closeConnection(conn)
  
  
  
  
  
  
  
  
  