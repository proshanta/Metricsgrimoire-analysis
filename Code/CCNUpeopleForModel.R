setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#read csv file where all repo list are
repoList = read.csv("../Analysis/CCNmodel/CCNrepoList.csv")  # read csv file

#read csv file where all owner list are
ownerList = read.csv("../Analysis/CCNmodel/CCNownerList.csv")  # read csv file

#read csv file where all issue list are
issueList = read.csv("../Analysis/CCNmodel/CCNissueList.csv")  # read csv file

if(nrow(ownerList)==0){
  ownerflag=0
}
if(nrow(issueList)==0){
  issueflag=0
}

#importing all supporting custom methods
source("Methods.R")

#Creating MySql connection for MSR_ECLIPSE_TICKETS
conn<-mySqlConnection(dbName="MSR_ECLIPSE_TICKETS")


#iterate each row/repo details in the file
for(row in 1:nrow(repoList)){
    
    #repo name, and date range
    repoName <- repoList[row,1]
    initialDate<- repoList[row,2]
    endDate<- repoList[row,3]

    print("working with")
    print(repoName)
    
    #Finding out the author list/vertices for the network
    str1<-paste("SELECT id,assigned_to FROM `issues` where tracker_id=",repoName,sep='')
    
    rs<-executeQuery(conn,str1)
    try(authorIssueList<- fetch(rs, n = -1))
    
    #Exception caught
    if(nrow(authorIssueList)==0){
        print("No author for the repo")
        print(repoName)
        next
    }
    
    
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
        
    
    #generating the pajek network file from vertices and edges
    finalAuthorList<-data.frame(id=1:nrow(authorList),authorList)
    
    finalAuthorStr<-paste(unique(finalAuthorList[,2]),collapse=",")
    
    str2<-paste("SELECT people_id ,upeople_id as author_id  FROM `people_upeople` where people_id in (",finalAuthorStr,")",sep='')
    
    rs<-executeQuery(conn,str2)
    try(upeopleList<- fetch(rs, n = -1))
    
    if(nrow(upeopleList)==0){
        print("No upeople mapping")
        print(repoName)
        next
    }    
    
    upeople<-paste(unique(upeopleList[,2]),collapse=",")
    
    
    
    finalAuthorList<-data.frame(id=1:nrow(unique(upeopleList[2])),unique(upeopleList[2]))
    #finalAuthorList<-finalAuthorList[c(1,3)]
 
    
    
    #-----------------------finding the edge list-----------------------------
    
    
    
    str2<-paste("select id from msr_eclipse_source_code.repositories where uri in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scm')",sep='')
    
    rs<-executeQuery(conn,str2)
    try(scmRepoList<- fetch(rs, n = -1))
    
    if(nrow(scmRepoList)==0){
        print("No scm repo list")
        print(repoName)
        next
    } 
    
	
    str3<-paste("SELECT people_id,upeople_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")",sep='')
    
    rs<-executeQuery(conn,str3)
    try(peopleupeople<- fetch(rs, n = -1))
    
    if(nrow(peopleupeople)==0){
        print("No people upeople mapping")
        next
    } 
        
    scmPeopleList<-peopleupeople[1]
    scmPeopleListstr<-paste(unique(scmPeopleList[,1]),collapse=",")
	
	
	####Newly added####
    
    scmRepoListWithMxMnDd<-data.frame()
	
	for(scmRow in 1:nrow(scmRepoList)){
	
		scmRepoName<-scmRepoList[scmRow,1]
		str8<-paste("SELECT repository_id as repo,LAST_DAY(DATE_FORMAT(max(author_date),'%Y-%m-%d')) as mx,DATE_FORMAT(min(author_date),'%Y-%m-%d') as mn, CEILING(DATEDIFF(LAST_DAY(DATE_FORMAT(max(author_date),'%Y-%m-%d')),LAST_DAY(DATE_FORMAT(min(author_date),'%Y-%m-%d')))/30) AS md FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in(",scmPeopleListstr,")",sep='')
      
		rs<-executeQuery(conn,str8)
        try(MxMnDd<- fetch(rs, n = -1))
      
        if(nrow(MxMnDd)==0){
            print("No MxMnDd for the repo")
            print(repoName)
            next
        }
        
		scmRepoListWithMxMnDd<-rbind(MxMnDd,scmRepoListWithMxMnDd)
	
	}
	
	
    colnames(scmRepoListWithMxMnDd)<- c("repo","mx","mn","md")
    scmRepoListWithMxMnDd<-na.omit(scmRepoListWithMxMnDd)
	
   
    for(scmRow in 1:nrow(scmRepoListWithMxMnDd)){
      
      scmRepoName<-scmRepoListWithMxMnDd[scmRow,1]
      mx<-scmRepoListWithMxMnDd[scmRow,2]
      mn<-scmRepoListWithMxMnDd[scmRow,3]
      md<-scmRepoListWithMxMnDd[scmRow,4]
      
      
      for(monthRow in 0:(md-1)){
          
              if(monthRow<0){
                  next
              }    
          
              str9<-paste("SELECT author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in(",scmPeopleListstr,") and DATE_FORMAT(author_date,'%Y-%m-%d')<=DATE_SUB(LAST_DAY('",mx,"'), INTERVAL ",monthRow," MONTH)",sep='')
              
              rs<-executeQuery(conn,str9)
              try(peopleListForScmSub<- fetch(rs, n = -1))
              
              if(nrow(peopleListForScmSub)==0){
                  print("No upeople mapping")
                  print(repoName)
                  next
              }   
              
              peopleListForSubScmStr<-paste(unique(peopleListForScmSub[,1]),collapse=",")
              
              
              #finalAuthorList<-data.frame(id=1:nrow(authorList),authorList)
              
              #finalAuthorStr<-paste(unique(finalAuthorList[,2]),collapse=",")
              
              str10<-paste("SELECT people_id ,upeople_id as author_id FROM msr_eclipse_source_code.people_upeople where people_id in (",peopleListForSubScmStr,")",sep='')
              
              rs<-executeQuery(conn,str10)
              try(upeopleListScmSub<- fetch(rs, n = -1))
              
              if(nrow(upeopleListScmSub)==0){
                  print("No upeople mapping")
                  print(repoName)
                  next
                }    
              
          
            finalAuthorListSub<-data.frame(id=1:nrow(unique(upeopleListScmSub[2])),unique(upeopleListScmSub[2]))
            
      
            str4<-paste("select author_id1,author_id2,count(*) as cnt from (select author_id as author_id1,file_id from msr_eclipse_source_code.actions A natural join (SELECT id as commit_id,author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in(",peopleListForSubScmStr,") and DATE_FORMAT(author_date,'%Y-%m-%d')<=DATE_SUB(LAST_DAY('",mx,"'), INTERVAL ",monthRow," MONTH))B)C natural join (select author_id as author_id2,file_id from msr_eclipse_source_code.actions A natural join (SELECT id as commit_id,author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in(",peopleListForSubScmStr,") and DATE_FORMAT(author_date,'%Y-%m-%d')<=DATE_SUB(LAST_DAY('",mx,"'), INTERVAL ",monthRow," MONTH))B)D where author_id1<>author_id2 group by author_id1,author_id2",sep='')
      
            rs<-executeQuery(conn,str4)
            try(edgeList<- fetch(rs, n = -1))
      
            if(nrow(edgeList)==0){
                print("No edge list for the repo")
                print(repoName)
                next
            } 
      
            edgeList<-merge(upeopleListScmSub,edgeList,by.x='people_id',by.y='author_id1')[, c(2,3,4)]
            edgeList<-merge(upeopleListScmSub,edgeList,by.x='people_id',by.y='author_id2')[, c(2,3,4)]
      
            colnames(edgeList)<-c("author_id1","author_id2","count")
      
      #----------------Edgelist to pajek------------------------
      
            output1<-merge(finalAuthorListSub,edgeList,by.x='author_id',by.y='author_id1')[, c(2,3,4)]
            output2<-merge(finalAuthorListSub,output1,by.x='author_id',by.y='author_id2')[, c(2,3,4)]
      
            colnames(output2)<-c("author_id1","author_id2","count")
      
      
            fileConn<-file(paste("../Output/CCNMODEL/",repoName,"_",scmRepoName,"_",monthRow,".net",sep=''))
            vertices<-c(paste("*Vertices ",nrow(finalAuthorListSub)))
      
      
            for(lines in 1:nrow(finalAuthorListSub)){
                str3<-paste(finalAuthorListSub[lines,1],' "',finalAuthorListSub[lines,2],'" ','box',sep='')
                vertices<-c(vertices,str3)      
            }
      
            vertices<-c(vertices,"*Edges")
      
            for(lines in 1:nrow(output2)){
                str3<-paste(output2[lines,1],' ',output2[lines,2],' ',output2[lines,3],sep='')
                vertices<-c(vertices,str3)      
            }
      
            write(vertices, fileConn)
            close(fileConn)
      }
      
      
    }   
    
            
}
closeConnection(conn)
