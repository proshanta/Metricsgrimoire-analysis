setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#read csv file where all repo list are
repoList = read.csv("../Analysis/CCN_REPO_FOR_ANALYSIS/Network_Family-metrics.csv")  # read csv file

#iterate each row/repo details in the file
for(row in 1:nrow(repoList)){
    
    #repo name, and date range
    repoName <- repoList[row,2]
    temp<-unlist(strsplit(toString(repoName),"_"))
 
    print("working with")
    print(repoName)
    
    virtexMetricsFileName<-paste("../Output/CCN_NW_METRICS/VERTEX_METRICS/",repoName,"-metrics.csv",sep='')
    parameterFileName<-paste("../Output/Parameters/",temp[1],".csv",sep='')
    
    
    if(!file.exists(virtexMetricsFileName)){
        print("File not exist for")
        print(repoName)
        next
    }
    
    if(!file.exists(parameterFileName)){
        print("File not exist for")
        print(temp[1])
        next
    }
    
    
    
    
    
    #read csv file where all repo list are
    virtexMetricsFile = read.csv(virtexMetricsFileName)  # read csv file
    parameterFile = read.csv(parameterFileName)
    
    repoVirtexMetricsParameter<-merge(parameterFile,virtexMetricsFile,by.x='upeople_id',by.y='labels')
    
    filename<-paste("../Output/CCN_REPO_ANALYSIS_OUTPUT/",temp[1],".csv",sep='')
    
    write.csv(repoVirtexMetricsParameter, file = filename,row.names = FALSE)
            
}
