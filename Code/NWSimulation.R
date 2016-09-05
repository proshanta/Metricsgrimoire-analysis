setwd("C:/Users/IBM_ADMIN/Desktop/Subhajit Sir/Grimoire/Metricsgrimoire-analysis/Code")

#read csv file for nw details
nwList = read.csv("../Analysis/NWsimulation/SimulationDetails.csv")  # read csv file


#iterate each row/repo details in the file
for(row in 1:nrow(nwList)){
    
    #nw clique, vertices and probability
    nwId <- nwList[row,1]
    noOfCliques <- nwList[row,2]
    noOfVerticesPerClique<- nwList[row,3]
    probability<- nwList[row,4]
    totalNoOfVertices<-noOfCliques*noOfVerticesPerClique
    
    print("working with")
    print(nwId)
    
    ringVertices<-data.frame()
    clique<-data.frame()
    edgeList<-data.frame()
    
    for(i in 1:totalNoOfVertices){
                
        if(i%%noOfVerticesPerClique==0){
            clique<-rbind(i,clique)
            ringVertices<-rbind(i,ringVertices)
            colnames(clique)<-c("clique")
            #generating complete graph
            newClique<-combn(clique[,1],2)
            for(j in 1:ncol(newClique)){
                newedge<-newClique[,j]
                edgeList<-rbind(c(newedge,1),edgeList)
            }
            colnames(edgeList)<-c("id1","id2","weight")            
            clique<-data.frame()            
            
        }else{
            clique<-rbind(i,clique)
        }
        
        
    }
    
    ringedges<-data.frame()
    
    ##generating ring nw
    for(i in 1:nrow(ringVertices)){
        if(i==nrow(ringVertices)){
            edgeList<-rbind(c(ringVertices[1,1],ringVertices[i,1],1),edgeList)
            ringedges<-rbind(c(ringVertices[1,1],ringVertices[i,1]),ringedges)
        }else{
            edgeList<-rbind(c(ringVertices[i,1],ringVertices[i+1,1],1),edgeList)
            ringedges<-rbind(c(ringVertices[i,1],ringVertices[i+1,1]),ringedges)
        }
        
    }
    
    colnames(ringedges)<-c("x","y")
    
    ringclique<-combn(ringVertices[,1],2)
    ringclique<-as.data.frame.matrix(t(ringclique))
    colnames(ringclique)<-c("x","y")
    require(sqldf)
    source("methods.R")
    a<-ringedges
    EdgesLeft<-sqldf("select * from ringclique left join a on ringclique.x=a.x and ringclique.y=a.y")
    EdgesLeft<-EdgesLeft[is.na(EdgesLeft[3]),1:2]
    
    noOfMoreConnection<-floor(nrow(EdgesLeft)*probability)
    
    if(noOfMoreConnection>0){
        for(i in 1:noOfMoreConnection){
            edgeList<-rbind(c(EdgesLeft[i,1],EdgesLeft[i,2],1),edgeList)
        }
    }
        
    #d<-as.data.frame.matrix(t(b))
    #colnames(d)<-c("x","y")
    #merge(c,d,df1[!duplicated(rbind(df2, df1))[-seq_len(nrow(df2))], ]
    #require(sqldf)
    #source("methods.R")
    #a<-sqldf("select * from d left join c on d.x=c.x and d.y=c.y")
    #a[is.na(a[3]),1:2]
    
    
    
    
    
    fileConn<-file(paste("../Output/NWsimulation/",nwId,"_",noOfCliques,"_",noOfVerticesPerClique,"_",probability,".net",sep=''))
    vertices<-c(paste("*Vertices ",totalNoOfVertices))
    
    
    for(i in 1:totalNoOfVertices){
        str3<-paste(i,' "',i,'" ','box',sep='')
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
