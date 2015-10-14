  setwd("C:\\Users\\IBM_ADMIN\\Desktop\\MatricsGrimoire\\PreAnalysisQuery\\Grimoire\\Metricsgrimoire-analysis\\Scripts")

#importing all supporting custom methods
  source("Methods.R")

#Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")

  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_ECLIPSE_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_ECLIPSE_SOURCE_CODE,"..\\Output\\MSR_ECLIPSE_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  pdf("..\\Output\\MSR_ECLIPSE_SOURCE_CODE_HIST.pdf")
  hist(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  dev.off()
  #Summary Descriptive Statistics for eclipse
  print("Summary Descriptive Statistics for eclipse")
  library(psych)
  e<-describe(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  write.table(x=e,"..\\Output\\Stat_eclipse.csv",sep = ",")
  closeConnection(conn)
  
  

#Creating MySql connection for MSR_OPENSTACK_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_OPENSTACK_SOURCE_CODE")
  
  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_OPENSTACK_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_OPENSTACK_SOURCE_CODE,"..\\Output\\MSR_OPENSTACK_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  pdf("..\\Output\\MSR_OPENSTACK_SOURCE_CODE_HIST.pdf")
  hist(MSR_OPENSTACK_SOURCE_CODE$number_of_unique_author)  
  dev.off()
  #Summary Descriptive Statistics for Openstack
  print("Summary Descriptive Statistics for openstack")
  d<-describe(MSR_ECLIPSE_SOURCE_CODE$number_of_unique_author)
  write.table(x=d,"..\\Output\\Stat_openstack.csv",sep = ",")
  closeConnection(conn)
  
  
  
  
  
  
  
  