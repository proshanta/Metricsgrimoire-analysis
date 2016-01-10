setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#read csv file where all repo list are
repoList = read.csv("../Analysis/TCNrepoList.csv")  # read csv file

#read csv file where all owner list are
ownerList = read.csv("../Analysis/TCNownerList.csv")  # read csv file

#read csv file where all issue list are
issueList = read.csv("../Analysis/TCNissueList.csv")  # read csv file

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
    
    
    str1<-paste("select author1 as author_id1,author2 as author_id2,count(*) from (SELECT submitted_by as author1,issue_id FROM `comments` where issue_id in(",issues,") and submitted_by in (",authors,") and (DATE_FORMAT(submitted_on,'%Y-%m-%d') between '",initialDate,"' and '",endDate,"'))A natural join (SELECT submitted_by as author2,issue_id FROM `comments` where issue_id in(",issues,") and submitted_by in (",authors,") and (DATE_FORMAT(submitted_on,'%Y-%m-%d') between '",initialDate,"' and '",endDate,"'))B where author1 <> author2 group by author1,author2",sep='')
    
    rs<-executeQuery(conn,str1)
    edgeList<- fetch(rs, n = -1)
    
    
    #generating the pajek network file from vertices and edges
    finalAuthorList<-data.frame(id=1:nrow(authorList),authorList)
    
    
    output1<-merge(finalAuthorList,edgeList,by.x='author_id',by.y='author_id1')[, c(2,3,4)]
    output2<-merge(finalAuthorList,output1,by.x='author_id',by.y='author_id2')[, c(2,3,4)]
    
    
    fileConn<-file(paste("../Output/TCN/",repoName,".net",sep=''))
    vertices<-c(paste("*Vertices ",nrow(finalAuthorList)))
    
    for(lines in 1:nrow(finalAuthorList)){
      str3<-paste(finalAuthorList[lines,1],' "',finalAuthorList[lines,2],'" ','box',sep='')
      vertices<-c(vertices,str3)      
    }
    
    vertices<-c(vertices,"*Edges")
    
    for(lines in 1:nrow(edgeList)){
      str3<-paste(edgeList[lines,1],' ',edgeList[lines,2],' ',edgeList[lines,3],sep='')
      vertices<-c(vertices,str3)      
    }
    
    write(vertices, fileConn)
    close(fileConn)   
    
        
}
closeConnection(conn)
