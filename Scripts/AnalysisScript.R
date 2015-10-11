  setwd("C:\\Users\\IBM_ADMIN\\Desktop\\MatricsGrimoire\\PreAnalysisQuery\\Scripts")

#importing all supporting custom methods
  source("Methods.R")

#Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")

  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_ECLIPSE_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_ECLIPSE_SOURCE_CODE,"..\\Output\\MSR_ECLIPSE_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  
  closeConnection(conn)

#Creating MySql connection for MSR_OPENSTACK_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_OPENSTACK_SOURCE_CODE")
  
  rs<-executeQuery(conn,"select repository_id, count(distinct author_id) number_of_unique_author from scmlog group by repository_id order by number_of_unique_author DESC")
  MSR_OPENSTACK_SOURCE_CODE<- fetch(rs, n = -1)
  write.table(x=MSR_OPENSTACK_SOURCE_CODE,"..\\Output\\MSR_OPENSTACK_SOURCE_CODE.csv",sep = ",",row.names = FALSE)
  
  closeConnection(conn)
  
  
  
  
  
  
  
  