setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

directory<-"../Output/CCN_REPO_ANALYSIS_OUTPUT/"
    
#read csv file where all repo list are
filenames <- list.files(path=directory,pattern="*.csv")

#importing all supporting custom methods
source("Methods.R")

#Creating MySql connection for MSR_ECLIPSE_TICKETS
conn<-mySqlConnection(dbName="MSR_ECLIPSE_TICKETS")

repoFamily<-data.frame()


#iterate each row/repo details in the file
for(row in 1:length(filenames)){
    
    #repo name, and date range
    repoName<-unlist(strsplit(toString(filenames[row]),"[.]"))[1]
    
    filePath<-paste(directory,filenames[row],sep='')
     
    file <- read.csv(filePath)
    
    #DataFrame where we will store all parameters for each repo
    repoWiseFrame<-data.frame()
    repoWiseFrameResolutionTime<-data.frame()
    
    for(rowForUpeople in 1:nrow(file[1])){
        
        upeopleId<-file[rowForUpeople,1]
        
        ##-------------------------------------no_of_bug count-------------------------------##
        str<-paste("SELECT people_id FROM `people_upeople` where upeople_id=",upeopleId,sep='')
        
        rs<-executeQuery(conn,str)
        try(peopleId<- fetch(rs, n = -1))
        
        if(nrow(peopleId)==0){
            repoWiseFrame<-rbind(repoWiseFrame,data.frame(upeople=upeopleId,count=0))
            repoWiseFrameResolutionTime<-rbind(repoWiseFrameResolutionTime,data.frame(upeople=upeopleId,count=0))
            next
        }
        
        authors<-paste(unique(peopleId[,1]),collapse=",")
                       
        str<-paste("SELECT count(*) as cnt FROM `issues` where tracker_id=",repoName," and status='RESOLVED' and assigned_to in(",authors,")",sep='')
        
        rs<-executeQuery(conn,str)
        try(cnt<- fetch(rs, n = -1))
        
        if(nrow(cnt)==0){
            cnt<-0
        }
        repoWiseFrame<-rbind(repoWiseFrame,data.frame(upeople=upeopleId,count=cnt))
        
        
        ##----------------------------------AVG_Resolution_Time----------------------------------------------------##
        
        str<-paste("select avg(datediff(mx,mn)) as count from (select issue_id,min(changed_on) as mn from (SELECT id as issue_id FROM `issues` where tracker_id=",repoName," and status='RESOLVED' and assigned_to in(",authors,"))A natural join changes B where old_value='NEW' group by issue_id) A natural join (select issue_id,max(changed_on) as mx from (SELECT id as issue_id FROM `issues` where tracker_id=",repoName," and status='RESOLVED' and assigned_to in(",authors,"))A natural join changes B where new_value='RESOLVED' group by issue_id) B",sep='')
        
        rs<-executeQuery(conn,str)
        try(cnt<- fetch(rs, n = -1))
        
        if(nrow(cnt)==0){
            repoWiseFrameResolutionTime<-rbind(repoWiseFrameResolutionTime,data.frame(upeople=upeopleId,count=0))
            next
        }
        repoWiseFrameResolutionTime<-rbind(repoWiseFrameResolutionTime,data.frame(upeople=upeopleId,count=cnt))       
        
        
    }
    colnames(repoWiseFrame)<-c("upeople_id","no_of_bug_owned")
    colnames(repoWiseFrameResolutionTime)<-c("upeople_id","avg_resolution")
    
    repoWiseFrame[is.na(repoWiseFrame)] <- 0    
    repoWiseFrameResolutionTime[is.na(repoWiseFrameResolutionTime)] <- 0
    
    upeopleWithNOBandART<-merge(repoWiseFrame,repoWiseFrameResolutionTime,by.x='upeople_id',by.y='upeople_id')
    
    repoWiseNoOfBug<-mean(repoWiseFrame[,2])
    repoWiseAvgReso<-mean(repoWiseFrameResolutionTime[,2])
    
    
    
    upeopleWithNOBandARTMain<-merge(upeopleWithNOBandART,file,by.x='upeople_id',by.y='upeople_id')
    
    file1<-paste("../Output/CCN_REPO_ANALYSIS_OUTPUT1/",filenames[row],sep='')
    
    write.csv(upeopleWithNOBandARTMain,file1,row.names= FALSE)    
    
    repoFamily<-rbind(repoFamily,data.frame(repoName=repoName,repoWiseNoOfBug=repoWiseNoOfBug,repoWiseAvgReso=repoWiseAvgReso))
    
            
}

file1<-paste("../Analysis/CCN_REPO_FOR_ANALYSIS/Network_Family-metrics.csv",sep='')
file <- read.csv(file1)

repoNameWithSubRepo<-data.frame()

for(i in 1:nrow(file[2])){
temp<-unlist(strsplit(toString(file[i,2]),"[_]"))[1]
s<-as.character(file[i,2])
a<-data.frame(a=s,b=temp)
repoNameWithSubRepo<-rbind(repoNameWithSubRepo,a)
}

colnames(repoNameWithSubRepo)<-c("temp","repo")
colnames(repoFamily)<-c("repo","avg_no_of_bugs_owned_by_author","avg_resolution_time")

repoNameWithSubRepo<-merge(repoNameWithSubRepo,repoFamily,by.x='repo',by.y='repo')

file<-merge(file,repoNameWithSubRepo,by.x='temp',by.y='temp')

file1<-paste("../Output/CCN_NW_METRICS/FAMILY_METRICS/newCCNNetwork_Family-metrics.csv",sep='')

write.csv(file,file1,row.names= FALSE)    

closeConnection(conn)
