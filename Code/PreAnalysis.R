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
  

#Finding out the intersection of people among different DB (Eclipse)
  
  
  #importing all supporting custom methods
  source("Methods.R")
  
  #Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
  conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")
  
  #finding out the project list we are interested in
  rs<-executeQuery(conn,"select project_id from(select * from (select * from (SELECT * FROM `project_repositories`)A natural join (select project_id from (SELECT * FROM `project_repositories` group by project_id,data_source having count(*)=1)A group by project_id having count(*)=4)B)C group by C.data_source,C.repository_name having count(*)=1)D group by project_id having count(*)=4")
  
  projectList<- fetch(rs, n = -1)
  
  
  ProjectWithAuthor<-data.frame()
  
  for(row in 1:nrow(projectList)){    
    
    str1<-paste("SELECT distinct upeople_id FROM msr_eclipse_source_code.`people_upeople` where people_id in(SELECT distinct author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=(SELECT id FROM msr_eclipse_source_code.`repositories` where uri=(select repository_name from msr_eclipse_source_code.project_repositories where project_id=",projectList[row,1]," and data_source='scm')))",sep='')  
    str2<-paste("SELECT distinct upeople_id FROM msr_eclipse_tickets.`people_upeople` natural join (SELECT distinct submitted_by as people_id  FROM msr_eclipse_tickets.`comments` where issue_id in(SELECT distinct issue FROM msr_eclipse_tickets.`issues` where tracker_id=(SELECT id FROM msr_eclipse_tickets.`trackers` where url=(select repository_name from msr_eclipse_source_code.project_repositories where project_id=",projectList[row,1]," and data_source='its'))))A",sep='')
    str3<-paste("SELECT distinct upeople_id FROM msr_eclipse_reviews.`people_upeople` natural join (SELECT distinct submitted_by as people_id  FROM msr_eclipse_reviews.`comments` where issue_id in(SELECT distinct submitted_by as people_id  FROM msr_eclipse_reviews.`comments` where issue_id in(SELECT distinct issue FROM msr_eclipse_reviews.`issues` where tracker_id=(SELECT id FROM msr_eclipse_reviews.`trackers` where url=(select repository_name from msr_eclipse_source_code.project_repositories where project_id=",projectList[row,1]," and data_source='scr')))))A",sep='')    
    str4<-paste("SELECT distinct upeople_id FROM msr_eclipse_mailing_lists.`people_upeople` natural join (SELECT distinct email_address as people_id FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url=(select repository_name from msr_eclipse_source_code.project_repositories where project_id=",projectList[row,1]," and data_source='mls'))A",sep='')
    
    
    rs<-executeQuery(conn,str1)
    scmAuthor<- fetch(rs, n = -1)
    
    rs<-executeQuery(conn,str2)
    itsAuthor<- fetch(rs, n = -1)
    
    rs<-executeQuery(conn,str3)
    scrAuthor<- fetch(rs, n = -1)
    
    rs<-executeQuery(conn,str4)
    mlsAuthor<- fetch(rs, n = -1)   
    
    result<-merge(merge(scmAuthor,merge(itsAuthor,scrAuthor)),mlsAuthor)
    
    
    projectFile<-data.frame(project=projectList[row,1],author=nrow(result))
    ProjectWithAuthor<-rbind(ProjectWithAuthor,projectFile)    
  }
  
  write.csv(ProjectWithAuthor, file = "../Output/PreAnalysis/EclipseProjectWithAuthor.csv",row.names=FALSE) 
  
  closeConnection(conn)
  
  
  
  
  
  
  
  