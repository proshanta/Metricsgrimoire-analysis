#importing all supporting custom methods for DB connection and reading property value 
source("Methods.R")

#apply sql query on data.frame
library(sqldf)

#---------------------------------------------------------------#
#  generate the repo list with initial_date and end_date        #
#---------------------------------------------------------------#

# 
#     #Creating MySql connection for MSR_ECLIPSE_TICKETS
#     conn<-mySqlConnection(dbName="MSR_ECLIPSE_TICKETS")
# 
#     #Finding out repo list with initial_date and end_date
#     str<-paste("select id as repo_id,mn as initial_date,mx as end_date,cnt from (select D.tracker_id as id , count(*) as cnt,DATE_FORMAT(max(submitted_on),'%Y-%m-%d') as mx,DATE_FORMAT(min(submitted_on),'%Y-%m-%d') as mn from (select A.issue_id as id from (select distinct issue_id from msr_eclipse_tickets.changes where old_value='new')A natural join (select distinct issue_id from msr_eclipse_tickets.changes where new_value='resolved')B)C natural join msr_eclipse_tickets.issues D group by tracker_id)A natural join (select id from (SELECT distinct repository_name as url FROM msr_eclipse_source_code.`project_repositories` where data_source='its')A natural join msr_eclipse_tickets.trackers B)C where cnt>=50",sep='')
# 
#     rs<-executeQuery(conn,str)
#     repoInitialEndDateAndCount<- fetch(rs, n = -1)
#     
#     #as we don't need the count for our analysis
#     repoInitialEndDate<-repoInitialEndDateAndCount[1:3]
#     
#     #creating the file for repolist which will be the input
#     filename<-paste("..\\Analysis\\PARAMETER\\repoWithInitialAndEndDate.csv",sep='')
#     write.table(x=repoInitialEndDate,filename,sep = ",",row.names = F)
#     
# 
#     #code for generating owner list if we want further filtering, for now we manually copy ownerList in the PARAMETER directory
# 
#     #code for generating issue list if we want further filtering, for now we manually copy issueList in the PARAMETER directory
# 
#     #closing DB connections
#     closeConnection(conn)
#     


#--------------------------------------------------------------------------#
#  generate parameter list for repo list in above file we generated        #
#--------------------------------------------------------------------------#


    #read csv file where all repo list are
    repoList = read.csv("../Analysis/PARAMETER/repoWithInitialAndEndDate.csv") 

    #read csv file where all owner list are
    ownerList = read.csv("../Analysis/PARAMETER/ownerList.csv") 

    #read csv file where all issue list are
    issueList = read.csv("../Analysis/PARAMETER/issueList.csv") 

    #Creating MySql connection for MSR_ECLIPSE_TICKETS
    conn<-mySqlConnection(dbName="MSR_ECLIPSE_TICKETS")

    ownerflag=1
    issueflag=1

    #Flag for checking owner,issue filter available or not

    if(nrow(ownerList)==0){
        ownerflag=0
    }
    if(nrow(issueList)==0){
        issueflag=0
    }


    #iterate each repo 
    for(row in 1:nrow(repoList)){
  
        #repo name, and date range
        repoName <- repoList[row,1]
        initialDate<- repoList[row,2]
        endDate<- repoList[row,3]
  
  
        #DataFrame where we will store all parameters for each repo
        repoWiseFrame<-data.frame()
  
  
        #Finding out the issue with their owner for the repo
        str<-paste("SELECT id,assigned_to FROM `issues` where tracker_id=",repoName,sep='')
  
        rs<-executeQuery(conn,str)
        try(authorIssueList<- fetch(rs, n = -1))
        
        
        if(nrow(authorIssueList)==0){
            print("no author list for the repo")
            next
        }
        
  
        if(ownerflag!=0){
            authorIssueList<-merge(authorIssueList,ownerList,by.x='assigned_to',by.y='owner_id')[, c(1,2)]
    
        }
        if(issueflag!=0){
            authorIssueList<-merge(authorIssueList,issueList,by.x='id',by.y='issue_id')[, c(1,2)]
        }
        
        #author list after applying filter if there is any
        authorList<- unique(authorIssueList[2])
        colnames(authorList)<-c("author_id")
  
        #making issue and author list as string for using in sql query
        issues<-paste(unique(authorIssueList[,1]),collapse=",")
        authors<-paste(unique(authorIssueList[,2]),collapse=",")
        
        #mapping people with upeople and find out the upeople        
        str<-paste("SELECT distinct upeople_id FROM `people_upeople` where people_id in(",authors,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(upeopleList<- fetch(rs, n = -1))
        
        if(nrow(upeopleList)==0){
            print("no upeople list for the repo")
            next
        }
        
        #making it ascending ordered
        upeopleList<-upeopleList[with(upeopleList, order(upeople_id)), ]  
        upeopleList<-data.frame(upeople_id=c(upeopleList))  
        upeopleList<-unique(upeopleList)
        
        #string of upeople list
        upeople<-paste(unique(upeopleList[,1]),collapse=",")
        repoWiseFrame<-upeopleList
        
#--------------------------------------------------------------------------#
#  1. How many changes they have committed on source code.                 #
#--------------------------------------------------------------------------#
        
        #finding out corrsponding scm repos against its repo
        str<-paste("select id from msr_eclipse_source_code.repositories where uri in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scm')",sep='')
  
        rs<-executeQuery(conn,str)
        try(scmRepoList<- fetch(rs, n = -1))
        
        if(nrow(scmRepoList)==0){
            print("no scm repo for the repo")
            next
        }

        if(nrow(scmRepoList)!=0){
  
        #finding out the list of owner for scm for the repo
        str<-paste("SELECT people_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(scmPeopleList<- fetch(rs, n = -1))
        
        if(nrow(scmPeopleList)==0){
            print("no people list for the scm repo")
            next
        }
  
        scmPeopleListstr<-paste(unique(scmPeopleList[,1]),collapse=",")
  
        
        #iterate each scm repo and find out parameter for that repo
        for(scmRow in 1:nrow(scmRepoList)){
    
            scmRepoName<-scmRepoList[scmRow,1]
            
            str<-paste("select D.author_id,cnt as ",scmRepoName,"scmChanges from (SELECT people_id as author_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")) D left join (select author_id,count(*) as cnt from msr_eclipse_source_code.scmlog B natural join (SELECT people_id as author_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,"))A where repository_id=",scmRepoName," group by author_id )C on C.author_id=D.author_id",sep='')
    
            rs<-executeQuery(conn,str)
            try(scmAuthorCnt<- fetch(rs, n = -1))
            
            if(nrow(scmAuthorCnt)==0){
                print("no scm author count for the repo")
                next
            }
            
            
            #mapping people to upeople 
            temp<-paste(unique(scmAuthorCnt[,1]),collapse=",") 
            
            tempstr<-paste("SELECT people_id,upeople_id FROM msr_eclipse_source_code.`people_upeople` where people_id in(",temp,")",sep='')
            rs<-executeQuery(conn,tempstr)
            tempPeopleUpeopleMapping<- fetch(rs, n = -1)
            
            #people are mapping to upeople
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
        }

#if we want to sum up all we will un comment this
#         if(ncol(repoWiseFrame)>2){
#             totalChangesMadeInSCM<-rowSums(repoWiseFrame[,2:ncol(repoWiseFrame)])
#             repoWiseFrame<-cbind(repoWiseFrame,totalChangesMadeInSCM)
#         } else {
#             totalChangesMadeInSCM<-repoWiseFrame[2]
#             colnames(totalChangesMadeInSCM)<-c("totalChangesMadeInSCM")
#             repoWiseFrame<-cbind(repoWiseFrame,totalChangesMadeInSCM)
#         }


#--------------------------------------------------------------------------#
#  2. How many reviews they have made for the repo                         #
#--------------------------------------------------------------------------#

        #finding out the repo list for the reviews 
        str<-paste("select id from msr_eclipse_reviews.trackers where url in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scr')",sep='')
  
        rs<-executeQuery(conn,str)
        try(scrRepoList<- fetch(rs, n = -1))

        if(nrow(scrRepoList)==0){
            print("no scr repo list for the repo")
            next
        }

        
        if(nrow(scrRepoList)!=0){
  
        #finding out the reviewer list 
        str<-paste("SELECT people_id FROM msr_eclipse_reviews.`people_upeople` where upeople_id in(",upeople,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(scrPeopleList<- fetch(rs, n = -1))
        
        if(nrow(scrPeopleList)==0){
            print("no people for the SCR repo")
            next
            
        }
  
        scrPeopleListstr<-paste(unique(scrPeopleList[,1]),collapse=",")
  
        for(scrRow in 1:nrow(scrRepoList)){
    
            scrRepoName<-scrRepoList[scrRow,1]
            
            #upeople with corressponding review count
            str<-paste("select A.upeople_id,B.cnt as ",scrRepoName,"scrChanges from msr_eclipse_reviews.people_upeople A natural join (select submitted_by as people_id,count(*) as cnt from msr_eclipse_reviews.comments A natural join (SELECT distinct id as issue_id FROM msr_eclipse_reviews.`issues` where tracker_id=",scrRepoName,")B where submitted_by in(",scrPeopleListstr,") group by submitted_by)B",sep='')
    
            rs<-executeQuery(conn,str)
            try(upeopleWithCnt<- fetch(rs, n = -1))
            
            if(nrow(upeopleWithCnt)==0){
                print("no people upeople mapping  for the scr repo")
                next
            }
            
    
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

        }
#--------------------------------------------------------------------------#
#  3. How many mail they have exchanged                                    #
#--------------------------------------------------------------------------#
 
        #list of repos for mail system mls
        str<-paste("select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='mls'",sep='')
  
        rs<-executeQuery(conn,str)
        try(mlsRepoList<- fetch(rs, n = -1))

        if(nrow(mlsRepoList)==0){
            print("no mls repo for the repo")
            next
        }

    
        if(nrow(mlsRepoList)!=0){
        
        #list of mailer
        str<-paste("SELECT people_id FROM msr_eclipse_mailing_lists.`people_upeople` where upeople_id in(",upeople,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(mlsPeopleList<- fetch(rs, n = -1))
        
        if(nrow(mlsPeopleList)==0){
            print("no people upeople mapping  for the mls repo")
            next
        }
        
        
  
        mlsPeopleListstr<-paste(unique(mlsPeopleList[,1]),collapse="','")
  
        for(scrRow in 1:nrow(mlsRepoList)){
    
            mlsRepoName<-mlsRepoList[scrRow,1]
            
            #upeople with mail count
            str<-paste("select upeople_id,cnt as ",scrRow,"mslChanges from msr_eclipse_mailing_lists.people_upeople A natural join (SELECT email_address as people_id,count(*) as cnt FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"') group by email_address)B",sep='')
    
            rs<-executeQuery(conn,str)
            try(upeopleWithCnt<- fetch(rs, n = -1))
            
            if(nrow(upeopleWithCnt)==0){
                print("no upeople with count for the mls repo")
                next
            }
            
    
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
        }
#--------------------------------------------------------------------------#
#  4. How many domain they have been working on                            #
#--------------------------------------------------------------------------#
 
        #upeople with domain count
        str<-paste("SELECT upeople_id,count(*) as number_of_domain FROM msr_eclipse_source_code.`upeople_domains` where upeople_id in(",upeople,") group by upeople_id",sep='')
  
        rs<-executeQuery(conn,str)
        try(upeopleWithCnt<- fetch(rs, n = -1))

        if(nrow(upeopleWithCnt)==0){
            print("no upeople with count for the scr repo")
            next
        }


        t1<-upeopleWithCnt
        t2<-upeopleList
        tempstr1<-paste("select t2.upeople_id,t1.number_of_domain  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
        upeopleWithCnt<-sqldf(tempstr1)
        upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  
        upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
        repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2]) 

        repoWiseFrame[is.na(repoWiseFrame)] <- 0

#--------------------------------------------------------------------------#
#  5. Their country id                                                     #
#--------------------------------------------------------------------------#

        #upeople with their country id
        str<-paste("SELECT upeople_id,country_id as country FROM msr_eclipse_source_code.`upeople_countries` where upeople_id in(",upeople,") ",sep='')
  
        rs<-executeQuery(conn,str)
        try(upeopleWithCnt<- fetch(rs, n = -1))

        if(nrow(upeopleWithCnt)==0){
            print("no upeople with country for the scr repo")
            next
        }

  
        t1<-upeopleWithCnt
        t2<-upeopleList
        tempstr1<-paste("select t2.upeople_id,t1.country  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
        upeopleWithCnt<-sqldf(tempstr1)
        upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  
        upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  
        repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])    
        
        repoWiseFrame[is.na(repoWiseFrame)] <- 0

#--------------------------------------------------------------------------#
#  6. How many lines they have committed                                   #
#--------------------------------------------------------------------------#
    
        #finding out the repo listfor scm
        str<-paste("select id from msr_eclipse_source_code.repositories where uri in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scm')",sep='')
  
        rs<-executeQuery(conn,str)
        try(scmRepoList<- fetch(rs, n = -1))

        if(nrow(scmRepoList)==0){
            print("no repo list for the scm repo")
            next
        }



        if(nrow(scmRepoList)!=0){
        
        #finding out the owner list
        str<-paste("SELECT people_id FROM msr_eclipse_source_code.`people_upeople` where upeople_id in(",upeople,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(scmPeopleList<- fetch(rs, n = -1))
        
        if(nrow(scmPeopleList)==0){
            print("no repo list for the scm repo")
            next
        }
        
  
        scmPeopleListstr<-paste(unique(scmPeopleList[,1]),collapse=",")
  
  
        for(scmRow in 1:nrow(scmRepoList)){
    
            scmRepoName<-scmRepoList[scmRow,1]
            str4<-paste("select upeople_id,sum(added) as ",scmRepoName,"linesadded,sum(removed) as ",scmRepoName,"linesremoved from msr_eclipse_source_code.people_upeople natural join (select author_id as people_id,sum(added) as added,sum(removed) as removed from msr_eclipse_source_code.commits_lines A natural join (SELECT id as commit_id,author_id FROM msr_eclipse_source_code.`scmlog` where repository_id=",scmRepoName," and author_id in (",scmPeopleListstr,"))B group by author_id)C group by upeople_id",sep='')
    
            rs<-executeQuery(conn,str4)
            try(scmAuthorCnt<- fetch(rs, n = -1))
            
            if(nrow(scmAuthorCnt)==0){
                print("no scm repo with count for the scm repo")
                next
            }
            
    
            t1<-scmAuthorCnt
            t2<-upeopleList
            tempstr1<-paste("select t2.upeople_id,t1.",scmRepoName,"linesadded ,t1.",scmRepoName,"linesremoved  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
            upeopleWithCnt<-sqldf(tempstr1)
            upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
            upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
    
    
    
            repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])
            repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[3])  
        } 
        
        repoWiseFrame[is.na(repoWiseFrame)] <- 0
    }
  #------------------------------------------7. How many non domain people they are interacting with.------------------------------------------------
  
  
  # 
  # str5<-paste("select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='mls'",sep='')
  # 
  # rs<-executeQuery(conn,str5)
  # mlsRepoList<- fetch(rs, n = -1)
  # 
  # str6<-paste("SELECT people_id FROM msr_eclipse_mailing_lists.`people_upeople` where upeople_id in(",upeople,")",sep='')
  # 
  # rs<-executeQuery(conn,str6)
  # mlsPeopleList<- fetch(rs, n = -1)
  # 
  # mlsPeopleListstr<-paste(unique(mlsPeopleList[,1]),collapse="','")
  # 
  # for(scrRow in 1:nrow(mlsRepoList)){
  #   
  #   mlsRepoName<-mlsRepoList[scrRow,1]
  #   
  #   str7<-paste("select distinct D.p1,C.upeople_id as p2 from msr_eclipse_mailing_lists.people_upeople C natural join (select A.upeople_id as p1,B.p2 as people_id from msr_eclipse_mailing_lists.people_upeople A natural join (select p1 as people_id,p2 from (SELECT email_address as p1,type_of_recipient as t1,message_id FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"'))A natural join (SELECT email_address as p2,type_of_recipient as t2,message_id FROM msr_eclipse_mailing_lists.`messages_people` where mailing_list_url='",mlsRepoName,"' and email_address in('",mlsPeopleListstr,"'))B where (A.t1='To' and B.t2='From') or (A.t1='From' and B.t2='To'))B)D",sep='')
  #   
  #   
  #   rs<-executeQuery(conn,str7)
  #   upeopleWithupeople<- fetch(rs, n = -1)
  #   
  #   
  #   newSet<-data.frame()
  #   
  #   for(newsetRow in 1:nrow(upeopleWithupeople)){
  #     first<-upeopleWithupeople[newsetRow,1]
  #     second<-upeopleWithupeople[newsetRow,2]
  #     
  #     str8<-paste("SELECT domain_id FROM msr_eclipse_source_code.`upeople_domains` where upeople_id=",first,sep='')
  #     rs<-executeQuery(conn,str8)
  #     firstset<- fetch(rs, n = -1)
  #     
  #     
  #     str9<-paste("SELECT domain_id FROM msr_eclipse_source_code.`upeople_domains` where upeople_id=",second,sep='')
  #     rs<-executeQuery(conn,str9)
  #     secondset<- fetch(rs, n = -1)
  #     
  #     output1<-merge(firstset,secondset,by.x='domain_id',by.y='domain_id')[, c(1)]
  #     
  #     if(length(output1)==0){
  #       newSet<-rbind(newSet,c(first,second))
  #     }
  #     
  #   }
  #   
  #   colnames(newSet)<-c("p1","p2")
  #   
  #   tempstr<-paste("select p1 as upeople_id,count(*) as ",scrRow,"msldiffdomain from newSet group by p2",sep='')  
  #   upeopleWithCnt<- sqldf(tempstr)
  #   
  #   t<-upeopleWithCnt
  #   tempstr<-paste("select upeople_id,sum(",scrRow,"msldiffdomain) as ",scrRow,"msldiffdomain  from t group by upeople_id",sep='')
  #   t1<-sqldf(tempstr)
  #   t2<-upeopleList
  #   tempstr1<-paste("select t2.upeople_id,t1.",scrRow,"msldiffdomain  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
  #   upeopleWithCnt<-sqldf(tempstr1)
  #   upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
  #   upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
  #   repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])
  #    
  #         
  #   
  #   
  # }   
  # 
  # repoWiseFrame[is.na(repoWiseFrame)] <- 0
  # 
  
#--------------------------------------------------------------------------#
#  8. Among all reviews total number of reviews got merged                 #
#--------------------------------------------------------------------------#
  
        #finding out the repos for reviews
        str<-paste("select id from msr_eclipse_reviews.trackers where url in (select distinct repository_name from msr_eclipse_source_code.`project_repositories` where project_id in(SELECT distinct project_id FROM msr_eclipse_source_code.`project_repositories` where repository_name=(SELECT url FROM msr_eclipse_tickets.`trackers` where id=",repoName,")) and data_source='scr')",sep='')
  
        rs<-executeQuery(conn,str)
        try(scrRepoList<- fetch(rs, n = -1))

        if(nrow(scrRepoList)==0){
            print("no scr repo with count for the scr repo")
            next
        }


        if(nrow(scrRepoList)!=0){
  
        #list of reviewres
        str<-paste("SELECT people_id FROM msr_eclipse_reviews.`people_upeople` where upeople_id in(",upeople,")",sep='')
  
        rs<-executeQuery(conn,str)
        try(scrPeopleList<- fetch(rs, n = -1))
        
        if(nrow(scrPeopleList)==0){
            print("no scr people upeople mapping for the scr repo")
            next
        }
        
  
        scrPeopleListstr<-paste(unique(scrPeopleList[,1]),collapse=",")
  
        for(scrRow in 1:nrow(scrRepoList)){
    
            scrRepoName<-scrRepoList[scrRow,1]
    
            str<-paste("select A.upeople_id,B.cnt as ",scrRepoName,"scrChangesMerged from msr_eclipse_reviews.people_upeople A natural join (select submitted_by as people_id,count(*) as cnt from msr_eclipse_reviews.comments A natural join (SELECT distinct id as issue_id FROM msr_eclipse_reviews.`issues` where tracker_id=",scrRepoName," and status='MERGED')B where submitted_by in(",scrPeopleListstr,") group by submitted_by)B",sep='')
    
            rs<-executeQuery(conn,str)
            try(upeopleWithCnt<- fetch(rs, n = -1))
            
            if(nrow(upeopleWithCnt)==0){
                print("no people with count for the scr repo")
                next
            }
            
    
            t<-upeopleWithCnt
            tempstr<-paste("select upeople_id,sum(",scrRepoName,"scrChangesMerged) as ",scrRepoName,"scrChangesMerged  from t group by upeople_id",sep='')
            t1<-sqldf(tempstr)
            t2<-upeopleList
            tempstr1<-paste("select t2.upeople_id,t1.",scrRepoName,"scrChangesMerged  from t2  left outer join t1  on t2.upeople_id=t1.upeople_id ",sep='')
            upeopleWithCnt<-sqldf(tempstr1)
            upeopleWithCnt[is.na(upeopleWithCnt)] <- 0
            upeopleWithCnt<-upeopleWithCnt[with(upeopleWithCnt, order(upeople_id)), ]
    
            colnames(upeopleWithCnt)<-c("upeople_id",paste(scrRepoName,"ReviewGotMerged",sep=''))
    
            repoWiseFrame<-cbind(repoWiseFrame,upeopleWithCnt[2])   
    }   
  
    repoWiseFrame[is.na(repoWiseFrame)] <- 0
        }
  #------------------------------------------Writing in the file------------------------------------------------
  
  filename<-paste("..\\Output\\Parameters\\",repoName,".csv",sep='')
  write.table(x=repoWiseFrame,filename,sep = ",",row.names = F)
  
}


closeConnection(conn)
