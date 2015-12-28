#Reading the property file
myProp <- read.table("EclipseOpenstackProperties.txt", header=FALSE, sep="=", row.names=1, strip.white=TRUE, na.strings="NA", stringsAsFactors=FALSE)

#getting the property value for the specific key
getProperty <- function(key){
myProp[key, 1]
}

#Trying to connect MySql
mySqlConnection<- function(dbName="MSR_ECLIPSE_SOURCE_CODE"){
library(RMySQL)
library(DBI)
user<-getProperty("user")
password<-getProperty("password")
host<-getProperty("host")
dbname<-getProperty(dbName)

mydb = dbConnect(MySQL(), user=user, password=password, dbname=dbname, host=host)
}

executeQuery<-function(mydb, query){
rs = dbSendQuery(mydb, query)
}

closeConnection<-function(conn){
dbDisconnect(conn)
}
