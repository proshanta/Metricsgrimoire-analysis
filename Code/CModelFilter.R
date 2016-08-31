#####Please change the directory accordingly
setwd("C:/Users/IBM_ADMIN/Desktop/Tett/Working on")

directory<-"C:/Users/IBM_ADMIN/Desktop/Tett/Working on/CCMODEL/"
resultdirectory<-"../CCMODEL/result.csv"

mainDir <- "C:/Users/IBM_ADMIN/Desktop/Tett/Working on/CCMODEL"
subDir <- "outputDirectory"



#read csv file where all repo list are
filenames <- list.files(path=directory,pattern="*.net")

repoSubRepo<-read.csv("C:/Users/IBM_ADMIN/Desktop/Tett/Working on/CCMODEL/RepoSubRepoMapping.csv")

files<-data.frame(Subrepofile=filenames)

repoDataFrame<-data.frame()

for(i in 1:nrow(repoSubRepo)){
    a<-files[grepl(repoSubRepo[i,2],files[,1]),]
    aFrame<-data.frame(a)
    repoDataFrame<-rbind(aFrame,repoDataFrame)
    
}

colnames(repoDataFrame)<-c("repofiles")
repoDataFrame<-unique(repoDataFrame)

dir.create(file.path(mainDir, subDir))
setwd("C:/Users/IBM_ADMIN/Desktop/Tett/Working on/CCMODEL")

for(i in 1:nrow(repoDataFrame)){
    
    file.copy(repoDataFrame[i,1],"outputDirectory")
    
}

#=LEFT(B2,SUM(FIND("_",B2),-1))