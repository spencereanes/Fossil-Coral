sourceCpp("~/School/Fossil Coral/scripts/distFunc.cpp")
coral.df <- read.csv("~/School/Fossil Coral/data/Coral_AllCols.csv") %>% select(-Temperature, -Salinity, -pH, -OmegaA, -TCO2, -PI_TCO2,-oxygen,-TAlk, -X.1, -X)

#ITERATE OVER THESE COLUMNS
to_compute <- c("Temp","Salinity","pH","TAlk","TCO2","OmegaA","oxygen")

#BEGIN FOR LOOP
for(file in to_compute){
  print(file)
  
  folder <- "~/FossilCoral/Data/New Columns/"
  
  filepath <- paste(folder,file,".csv",sep="")
  read.df <- read_csv(filepath)
  
  colnames(read.df)[3] <- file
  head(read.df)
  
  #Creating matrices that we will use in the for loop and fill with distances
  longsT <- as.matrix(read.df[,"lon"])
  latsT <- as.matrix(read.df[,"lat"])
  allDists <- matrix(nrow=nrow(coral.df),ncol=nrow(read.df)) 
  
  #using distance function to match sites to sites
  for(i in 1:nrow(coral.df)){ 
    res <- distFuncOneMany(as.double(unlist(coral.df[i,"Lon"])),
                           as.double(unlist(coral.df[i,"Lat"])),
                           longsT,
                           latsT, nrow(read.df))
    allDists[i,]<-res
  }
  
  computedDistMat <- as.matrix(read.df[,file])
  
  #taking the weighted average of the three closest locations
  closest <- matrix(nrow=nrow(coral.df),ncol=nrow(read.df))
  nclosest <- 3
  arr <- array(dim=nrow(coral.df))
  for(i in 1:nrow(coral.df)){
    ords <- order(allDists[i,])
    ordtemp <- computedDistMat[ords][1:nclosest]
    #this is a weight determined by distance
    orddist <- 1/allDists[i,ords][1:nclosest]
    
    #MODIFY THIS STEP TO CHANGE AVERAGING TECHNIQUE
    #carb2<- mean(ordtemp)
    carb2 <- sum(orddist*ordtemp)/sum(orddist)
    arr[i] <- carb2 #rename to n closest temps
  }
  
  old_coral <- coral.df
  coral.df <- cbind(coral.df,arr)
  colnames(coral.df) <- c(colnames(old_coral),file)
  
  print(paste(file, "complete.",sep=" "))
}

write_csv(coral.df,"../data/coral_3weighted.csv")

