sourceCpp("~/FossilCoral/Archives/distFunc.cpp")
coral.df <- read.csv("~/FossilCoral/Data/Coral_AllCols.csv") %>% select(-Temperature, -Salinity, -pH, -OmegaA, -TCO2, -PI_TCO2,-oxygen,-TAlk, -X.1, -X)

#ITERATE OVER THESE COLUMNS
to_compute <- c("Temp","Salinity","pH","TAlk","TCO2","OmegaA","oxygen")
#to_compute <- c("Temp")

#BEGIN FOR LOOP
for(file in to_compute){
  print(file)
  
  folder <- "~/FossilCoral/Data/New Columns/"
  
  filepath <- paste(folder,file,".csv",sep="")
  carb.df <- read_csv(filepath)
  
  #this line could break since it was formatted lat then long, contrary to all others.
  #colnames(carb.df) <- c("lon","lat",file)
  colnames(carb.df)[3] <- file
  head(carb.df)
  
  #Creating matrices that we will use in the for loop and fill with distances
  longsT <- as.matrix(carb.df[,"lon"])
  latsT <- as.matrix(carb.df[,"lat"])
  allDists <- matrix(nrow=nrow(coral.df),ncol=nrow(carb.df)) 
  
  #using distance function to match sites to sites
  for(i in 1:nrow(coral.df)){ 
    res <- distFuncOneMany(as.double(unlist(coral.df[i,"Lon"])),
                           as.double(unlist(coral.df[i,"Lat"])),
                           longsT,
                           latsT, nrow(carb.df))
    allDists[i,]<-res
  }
  
  allCarbonate <- as.matrix(carb.df[,file])
  
  #taking the weighted average of the four closest locations
  closest <- matrix(nrow=nrow(coral.df),ncol=nrow(carb.df))
  nclosest <- 4
  arr <- array(dim=nrow(coral.df))
  for(i in 1:nrow(coral.df)){
    ords <- order(allDists[i,])
    ordtemp <- allCarbonate[ords][1:nclosest]
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

write_csv(coral.df,"~/FossilCoral/Data/Coral_Recompute_4Weighted.csv")

