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
    upeopleList<-upeopleList[with(upeopleList, order(upeople_id)), ]
    
    upeopleList<-data.frame(upeople_id=c(upeopleList))
    
    upeopleList<-unique(upeopleList)
    upeople<-paste(unique(upeopleList[,1]),collapse=",")
#--------------------------------1. How many changes they are committing on source code.------------------------------------------------------->
    
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
      upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
      
      
      
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
#-----------------------------------------2. How many reviews they are making------------------------------------------->

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
  upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0
#------------------------------------------------3. How many mail they are exchanging.-----------------------------------------------------------------


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
  upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0

#----------------------------------------4. How many domain they have been working on.----------------------

str7<-paste("SELECT upeople_id,count(*) as number_of_domain FROM msr_eclipse_source_code.`upeople_domains` where upeople_id in(",upeople,") group by upeople_id",sep='')

rs<-executeQuery(conn,str7)
upeopleWithCnt<- fetch(rs, n = -1)
t1<-upeopleWithCnt
t2<-upeopleList
tempstr1<-paste("select t2.upeople_id,t1.number_of_domain  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
upeopleWithCnt<-sqldf(tempstr1)
upeopleWithCnt[is.na(upeopleWithCnt)] <- 0

upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]

repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      

#----------------------------------------5. Their country----------------------

str7<-paste("SELECT upeople_id,country_id as country FROM msr_eclipse_source_code.`upeople_countries` where upeople_id in(",upeople,") ",sep='')

rs<-executeQuery(conn,str7)
upeopleWithCnt<- fetch(rs, n = -1)

t1<-upeopleWithCnt
t2<-upeopleList
tempstr1<-paste("select t2.upeople_id,t1.country  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
upeopleWithCnt<-sqldf(tempstr1)
upeopleWithCnt[is.na(upeopleWithCnt)] <- 0

upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]

repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])    

#----------------------------------------6. How many lines they are committing.----------------------



str2<-paste("select id from msr_eclipse_source_code.repositories where uri in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scm')",sep='')

rs<-executeQuery(conn,str2)
scmRepoList<- fetch(rs, n = -1)

str3<-paste("SELECT people_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")",sep='')

rs<-executeQuery(conn,str3)
scmPeopleList<- fetch(rs, n = -1)

scmPeopleListstr<-paste(unique(scmPeopleList[,1]),collapse=",")


for(scmRow in 1:nrow(scmRepoList)){
  
  scmRepoName<-scmRepoList[scmRow,1]
  str4<-paste("select upeople_id,sum(added) as ",scmRepoName,"linesadded,sum(removed) as ",scmRepoName,"linesremoved from msr_eclipse_source_code.people_upeople natural join (select author_id as people_id,sum(added) as added,sum(removed) as removed from msr_eclipse_source_code.commits_lines A natural join (SELECT id as commit_id,author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in (",scmPeopleListstr,"))B group by author_id)C group by upeople_id",sep='')
  
  rs<-executeQuery(conn,str4)
  scmAuthorCnt<- fetch(rs, n = -1)
  
  t1<-scmAuthorCnt
  t2<-upeopleList
  tempstr1<-paste("select t2.upeople_id,t1.",scmRepoName,"linesadded ,t1.",scmRepoName,"linesremoved  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  upeopleWithCnt<-sqldf(tempstr1)
  upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
  
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[3])  
}   
#------------------------------------------7. How many non domain people they are interacting with.------------------------------------------------



str5<-paste("select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='mls'",sep='')

rs<-executeQuery(conn,str5)
mlsRepoList<- fetch(rs, n = -1)

str6<-paste("SELECT people_id FROM msr_eclipse_mailing_lists.`people_upeople` where upeople_id in(",upeople,")",sep='')

rs<-executeQuery(conn,str6)
mlsPeopleList<- fetch(rs, n = -1)

mlsPeopleListstr<-paste(unique(mlsPeopleList[,1]),collapse="','")

for(scrRow in 1:nrow(mlsRepoList)){
  
  mlsRepoName<-mlsRepoList[scrRow,1]
  
  str7<-paste("select distinct D.p1,C.upeople_id as p2 from msr_eclipse_mailing_lists.people_upeople C natural join (select A.upeople_id as p1,B.p2 as people_id from msr_eclipse_mailing_lists.people_upeople A natural join (select p1 as people_id,p2 from (SELECT email_address as p1,type_of_recipient as t1,message_id FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"'))A natural join (SELECT email_address as p2,type_of_recipient as t2,message_id FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"'))B where (A.t1='To' and B.t2='From') or (A.t1='From' and B.t2='To'))B)D",sep='')
  
  
  rs<-executeQuery(conn,str7)
  upeopleWithupeople<- fetch(rs, n = -1)
  
  
  newSet<-data.frame()
  
  for(newsetRow in 1:nrow(upeopleWithupeople)){
    first<-upeopleWithupeople[newsetRow,1]
    second<-upeopleWithupeople[newsetRow,2]
    
    str8<-paste("SELECT domain_id FROM `upeople_domains` where upeople_id=",first,sep='')
    rs<-executeQuery(conn,str8)
    firstset<- fetch(rs, n = -1)
    
    
    str9<-paste("SELECT domain_id FROM `upeople_domains` where upeople_id=",second,sep='')
    rs<-executeQuery(conn,str9)
    secondset<- fetch(rs, n = -1)
    
    output1<-merge(firstset,secondset,by.x='domain_id',by.y='domain_id')[, c(2)]
    
    if(nrow(output1)==0){
      newSet<-rbind(newSet,c(first,second))
    }
    
  }
  
  colnames(newSet)<-c("p1","p2")
  
  tempstr<-paste("select p1 as upeople_id,count(*) as ",scrRow,"msldiffdomain from newSet group by p2",sep='')  
  upeopleWithCnt<- sqldf(tempstr)
  
  t<-upeopleWithCnt
  tempstr<-paste("select upeople_id,sum(",scrRow,"msldiffdomain) as ",scrRow,"msldiffdomain  from t group by upeople_id",sep='')
  t1<-sqldf(tempstr)
  t2<-upeopleList
  tempstr1<-paste("select t2.upeople_id,t1.",scrRow,"msldiffdomain  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  upeopleWithCnt<-sqldf(tempstr1)
  upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0


#-----------------------------------------9. The reviews they have been giving number of reviwes got merged.------------------------------------------->

str5<-paste("select id from msr_eclipse_reviews.trackers where url in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scr')",sep='')

rs<-executeQuery(conn,str5)
scrRepoList<- fetch(rs, n = -1)

str6<-paste("SELECT people_id FROM msr_eclipse_reviews.`people_upeople` where upeople_id in(",upeople,")",sep='')

rs<-executeQuery(conn,str6)
scrPeopleList<- fetch(rs, n = -1)

scrPeopleListstr<-paste(unique(scrPeopleList[,1]),collapse=",")

for(scrRow in 1:nrow(scrRepoList)){
  
  scrRepoName<-scrRepoList[scrRow,1]
  
  str7<-paste("select A.upeople_id,B.cnt as ",scrRepoName,"scrChangesMerged from msr_eclipse_reviews.people_upeople A natural join (select submitted_by as people_id,count(*) as cnt from msr_eclipse_reviews.comments A natural join (SELECT distinct id as issue_id FROM msr_eclipse_reviews.`issues` where tracker_id=",scrRepoName," and status='MERGED')B where submitted_by in(",scrPeopleListstr,") group by submitted_by)B",sep='')
  
  rs<-executeQuery(conn,str7)
  upeopleWithCnt<- fetch(rs, n = -1)
  
  
  
  t<-upeopleWithCnt
  tempstr<-paste("select upeople_id,sum(",scrRepoName,"scrChangesMerged) as ",scrRepoName,"scrChangesMerged  from t group by upeople_id",sep='')
  t1<-sqldf(tempstr)
  t2<-upeopleList
  tempstr1<-paste("select t2.upeople_id,t1.",scrRepoName,"scrChangesMerged  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  upeopleWithCnt<-sqldf(tempstr1)
  upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
  
  repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])      
  
  
}   

repoWiseFrame[is.na(repoWiseFrame)] <- 0


#------------------------------------------Writing in the file------------------------------------------------

filename<-paste("..\\Output\\Parameteres\\",repoName,".csv",sep='')
write.table(x=repoWiseFrame,filename,sep = ",")

}


closeConnection(conn)
