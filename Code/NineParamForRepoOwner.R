setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")
library(sqldf)

#read csv file where all repo list are
repoList = read.csv("../Analysis/TCN/TCNrepoList.csv")  # read csv file

#read csv file where all owner list are
ownerList = read.csv("../Analysis/TCN/TCNownerList.csv")  # read csv file

#read csv file where all issue list are
issueList = read.csv("../Analysis/TCN/TCNissueList.csv")  # read csv file

if(nrow(ownerList)==0){
  ownerflag=0
}
if(nrow(issueList)==0){
  issueflag=0
}


#iterate each row/repo details in the file
for(row in 1:nrow(repoList)){
    
    #repo name, and date range
    repoName <- repoList[row,1]
    initialDate<- repoList[row,2]
    endDate<- repoList[row,3]
    
    #importing all supporting custom methods
    source("Methods.R")
    
    #Creating MySql connection for MSR_ECLIPSE_TICKETS
    conn<-mySqlConnection(dbName="MSR_ECLIPSE_TICKETS")
    
    
    #main file for the repo where to write all
    repoWiseFrame<-data.frame()
      
    
    #Finding out the author list/vertices for the network
    str1<-paste("SELECT id,assigned_to FROM `issues` where tracker_id=",repoName,sep='')
    
    rs<-executeQuery(conn,str1)
    authorIssueList<- fetch(rs, n = -1)
    
    if(ownerflag!=0){
      authorIssueList<-merge(authorIssueList,ownerList,by.x='assigned_to',by.y='owner_id')[, c(1,2)]
      
    }
    if(issueflag!=0){
      authorIssueList<-merge(authorIssueList,issueList,by.x='id',by.y='issue_id')[, c(1,2)]
    }
    
    authorList<- unique(authorIssueList[2])
    colnames(authorList)<-c("author_id")
    
    
    issues<-paste(unique(authorIssueList[,1]),collapse=",")
    authors<-paste(unique(authorIssueList[,2]),collapse=",")
    
    str1<-paste("SELECT distinct upeople_id FROM `people_upeople` where people_id in(",authors,")",sep='')
    
    rs<-executeQuery(conn,str1)
    upeopleList<- fetch(rs, n = -1)
    
    upeopleList<-unique(upeopleList)
    upeople<-paste(unique(upeopleList[,1]),collapse=",")
#--------------------------------------------------------------------------------------->
    
    str2<-paste("select id from msr_eclipse_source_code.repositories where uri in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scm')",sep='')
    
    rs<-executeQuery(conn,str2)
    scmRepoList<- fetch(rs, n = -1)
    
    str3<-paste("SELECT people_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")",sep='')
    
    rs<-executeQuery(conn,str3)
    scmPeopleList<- fetch(rs, n = -1)
    
    scmPeopleListstr<-paste(unique(scmPeopleList[,1]),collapse=",")
    
    repoWiseFrame<-upeopleList
    
    for(scmRow in 1:nrow(scmRepoList)){
      
      scmRepoName<-scmRepoList[scmRow,1]
      str4<-paste("select D.author_id,cnt as ",scmRepoName,"scmChanges from (SELECT people_id as author_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")) D left join (select author_id,count(*) as cnt from msr_eclipse_source_code.scmlog B natural join (SELECT people_id as author_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,"))A where repository_id=",scmRepoName," group by author_id )C on C.author_id=D.author_id",sep='')
      
      rs<-executeQuery(conn,str4)
      scmAuthorCnt<- fetch(rs, n = -1)
      #mapping people to upeople 
      temp<-paste(unique(scmAuthorCnt[,1]),collapse=",")      
      tempstr<-paste("SELECT people_id,upeople_id FROM msr_eclipse_source_code.`people_upeople` where people_id in(",temp,")",sep='')
      rs<-executeQuery(conn,tempstr)
      tempPeopleUpeopleMapping<- fetch(rs, n = -1)
      upeopleWithCnt<-merge(scmAuthorCnt,tempPeopleUpeopleMapping,by.x='author_id',by.y='people_id')[, c(2,3)]
      upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
      t<-upeopleWithCnt
      tempstr<-paste("select upeople_id,sum(",scmRepoName,"scmChanges) as ",scmRepoName,"scmChanges  from t group by upeople_id",sep='')
      t1<-sqldf(tempstr)
      t2<-upeopleList
      tempstr1<-paste("select t2.upeople_id,t1.",scmRepoName,"scmChanges  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
      upeopleWithCnt<-sqldf(tempstr1)
      upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
      
      repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
      
    }   
      
    repoWiseFrame[is.na(repoWiseFrame)] <- 0
    
    if(ncol(repoWiseFrame)>2){
    totalChangesMadeInSCM<-rowSums(repoWiseFrame[,2:ncol(repoWiseFrame)])
    repoWiseFrame<-cbind(repoWiseFrame,totalChangesMadeInSCM)
    } else {
      totalChangesMadeInSCM<-repoWiseFrame[2]
      colnames(totalChangesMadeInSCM)<-c("totalChangesMadeInSCM")
      repoWiseFrame<-cbind(repoWiseFrame,totalChangesMadeInSCM)
    }
#------------------------------------------------------------------------------------>

  str5<-paste("select id from msr_eclipse_reviews.trackers where url in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scr')",sep='')
 
  rs<-executeQuery(conn,str5)
  scrRepoList<- fetch(rs, n = -1)

  str6<-paste("SELECT people_id FROM msr_eclipse_reviews.`people_upeople` where upeople_id in(",upeople,")",sep='')

  rs<-executeQuery(conn,str6)
  scrPeopleList<- fetch(rs, n = -1)

  scrPeopleListstr<-paste(unique(scrPeopleList[,1]),collapse=",")

for(scrRow in 1:nrow(scrRepoList)){
  
  scrRepoName<-scrRepoList[scrRow,1]
  
  str7<-paste("select A.upeople_id,B.cnt as ",scrRepoName,"scrChanges from msr_eclipse_reviews.people_upeople A natural join (select submitted_by as people_id,count(*) as cnt from msr_eclipse_reviews.comments A natural join (SELECT distinct id as issue_id FROM msr_eclipse_reviews.`issues` where tracker_id=",scrRepoName,")B where submitted_by in(",scrPeopleListstr,") group by submitted_by)B",sep='')
   
  rs<-executeQuery(conn,str7)
  upeopleWithCnt<- fetch(rs, n = -1)
  
  
  
  t<-upeopleWithCnt
  tempstr<-paste("select upeople_id,sum(",scrRepoName,"scrChanges) as ",scrRepoName,"scrChanges  from t group by upeople_id",sep='')
  t1<-sqldf(tempstr)
  t2<-upeopleList
  tempstr1<-paste("select t2.upeople_id,t1.",scrRepoName,"scrChanges  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  upeopleWithCnt<-sqldf(tempstr1)
  upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0
#-----------------------------------------------------------------------------------------------------------------


str5<-paste("select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='mls'",sep='')

rs<-executeQuery(conn,str5)
mlsRepoList<- fetch(rs, n = -1)

str6<-paste("SELECT people_id FROM msr_eclipse_mailing_lists.`people_upeople` where upeople_id in(",upeople,")",sep='')

rs<-executeQuery(conn,str6)
mlsPeopleList<- fetch(rs, n = -1)

mlsPeopleListstr<-paste(unique(mlsPeopleList[,1]),collapse="','")

for(scrRow in 1:nrow(mlsRepoList)){
  
  mlsRepoName<-mlsRepoList[scrRow,1]
 
  str7<-paste("select upeople_id,cnt as ",scrRow,"mslChanges from msr_eclipse_mailing_lists.people_upeople A natural join (SELECT email_address as people_id,count(*) as cnt FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"') group by email_address)B",sep='')
   
  rs<-executeQuery(conn,str7)
  upeopleWithCnt<- fetch(rs, n = -1)
    
  t<-upeopleWithCnt
  tempstr<-paste("select upeople_id,sum(",scrRow,"mslChanges) as ",scrRow,"mslChanges  from t group by upeople_id",sep='')
  t1<-sqldf(tempstr)
  t2<-upeopleList
  tempstr1<-paste("select t2.upeople_id,t1.",scrRow,"mslChanges  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  upeopleWithCnt<-sqldf(tempstr1)
  upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0
filename<-paste("..\\Output\\Parameteres\\",repoName,".csv",sep='')
write.table(x=repoWiseFrame,filename,sep = ",")

}


closeConnection(conn)
