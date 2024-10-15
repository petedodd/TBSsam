## utility to read and reshape the cost data
parsecosts <- function(fn){
  MCD.countries <- unlist(fread(fn)[1,])
  MCD <- fread(fn,skip=1)
  who <- which(!is.na(MCD.countries) & MCD.countries!='') #labels
  cnz <- MCD.countries[who]
  nmz <- names(MCD)
  nmz[who] <- paste0(cnz, ".", nmz[who])
  nmz[who + 1] <- paste0(cnz, ".", nmz[who + 1])
  names(MCD) <- nmz
  drop <- grep("^V", names(MCD))
  nmz <- nmz[-c(drop)]
  MCD <- MCD[, ..nmz]
  allnas <- MCD[,lapply(.SD,function(x)all(is.na(x)))] #which cols are all NA
  changetype <- names(MCD)[unlist(allnas)] #the cols names
  MCD[,c(changetype):=lapply(.SD,as.numeric),.SDcols=changetype] #change to nums
  CD <- melt(MCD, id = c("COSTS", "NAME", "DESCRIPTION"))
  CD[, c("country", "hilo") := tstrsplit(variable, "\\.")]
  dcast(CD[, .(NAME, DESCRIPTION, country, hilo, value)],
    NAME + DESCRIPTION + country ~ hilo,
    value.var = "value"
  )
}

