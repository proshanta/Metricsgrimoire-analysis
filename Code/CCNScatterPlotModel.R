##change dir accordingly

setwd("C:\\Users\\IBM_ADMIN\\Desktop\\Tett\\Working on\\CCMODEL\\outputDirectory\\")
familyModel<-read.csv("C:\\Users\\IBM_ADMIN\\Desktop\\Tett\\Working on\\CCMODEL\\outputDirectory\\Network_Family-metrics57Model.csv")

repolist<-unique(familyModel["Repo"])

pdf(file = "Vertices.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
        
    plot(temp[,"SubRepo"], temp[,"vertices"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="Vertices", pch=19)   
    
}

dev.off()


pdf(file = "Edges.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"edges"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="edges", pch=19)  
    
}

dev.off()


pdf(file = "avg_path_length.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"avg_path_length"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="avg_path_length", pch=19) 
    
}

dev.off()


pdf(file = "global_clustcoeff.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"global_clustcoeff"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="global_clustcoeff", pch=19) 
    
}

dev.off()


pdf(file = "degree_centralization.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"degree_centralization"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="degree_centralization", pch=19) 
    
}

dev.off()


pdf(file = "betweenness_centralization.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"betweenness_centralization"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="betweenness_centralization", pch=19) 
    
}

dev.off()

pdf(file = "closeness_centralization.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"closeness_centralization"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="closeness_centralization", pch=19) 
    
}

dev.off()


dev.off()

pdf(file = "eigenvector_centralization.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"eigenvector_centralization"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="eigenvector_centralization", pch=19) 
    
}

dev.off()

pdf(file = "assortativity.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"assortativity"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="assortativity", pch=19) 
    
}

dev.off()



pdf(file = "diameter.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"diameter"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="diameter", pch=19) 
    
}

dev.off()



pdf(file = "avg_degree.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"avg_degree"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="avg_degree", pch=19) 
    
}

dev.off()



pdf(file = "modularity.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"modularity"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="modularity", pch=19) 
    
}

dev.off()


pdf(file = "density.pdf",onefile=TRUE)

for(i in 1:nrow(repolist)){
    repo<-repolist[i,1]
    temp<-familyModel[familyModel[,"Repo"]==repolist[i,1],]
    temp["SubRepo"]<-(nrow(temp)-1)-temp["SubRepo"]
    
    plot(temp[,"SubRepo"], temp[,"density"], main=paste("Repo:",repo,collapse=NULL),xlab="Months", ylab="density", pch=19) 
    
}

dev.off()











