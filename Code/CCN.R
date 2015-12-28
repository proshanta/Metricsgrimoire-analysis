setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#read csv file where all inputs are
mydata = read.csv("../Analysis/CCNrepoList.csv")  # read csv file

#iterate each row/repo details in the file

for(row in 1:nrow(mydata)){
    
    #repo name, and date range
    repoName <- mydata[row,1]
    initialDate<- mydata[row,2]
    endDate<- mydata[row,3]
    
    #importing all supporting custom methods
    source("Methods.R")
    
    #Creating MySql connection for MSR_ECLIPSE_SOURCE_CODE
    conn<-mySqlConnection(dbName="MSR_ECLIPSE_SOURCE_CODE")
    
    #Finding out the author list/vertices for the network
    str1<-paste("SELECT distinct author_id FROM `scmlog` where repository_id=",repoName,sep='')
    
    rs<-executeQuery(conn,str1)
    authorList<- fetch(rs, n = -1)
    
    #find out the edge list for the network
    str1<-paste("select author_id1,author_id2,count(*) from (Select author_id as author_id1,file_id from (SELECT id as commit_id,author_id FROM `scmlog` where repository_id=",repoName," and (DATE_FORMAT(author_date,'%Y-%m-%d') between '",initialDate,"' and '",endDate,"'))A natural join actions B)C natural join (Select author_id as author_id2,file_id from (SELECT id as commit_id,author_id FROM `scmlog` where repository_id=",repoName," and (DATE_FORMAT(author_date,'%Y-%m-%d') between '",initialDate,"' and '",endDate,"'))A natural join actions B)D where author_id1 <> author_id2 group by author_id1,author_id2",sep='')
    
    rs<-executeQuery(conn,str1)
    edgeList<- fetch(rs, n = -1)
    
    #generating the pajek network file from vertices and edges
    finalAuthorList<-data.frame(id=1:nrow(authorList),authorList)
    
    
    output1<-merge(finalAuthorList,edgeList,by.x='author_id',by.y='author_id1')[, c(2,3,4)]
    output2<-merge(finalAuthorList,output1,by.x='author_id',by.y='author_id2')[, c(2,3,4)]
    
    
    fileConn<-file(paste("../Output/CCN/",repoName,".net",sep=''))
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
