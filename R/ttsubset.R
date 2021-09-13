ttsubset <- function(mydata, subset_days){
  if(missing(subset_days)) {
    subset_days = "all"
  }

  if(exists("mydata$mydata_45")){
    mydata_45 = mydata$mydata_45
  }
  if(exists("mydata$mydata_4D")){
    mydata_4D = mydata$mydata_4D
  }
  if(exists("mydata$mydata_49")){
    mydata_49 = mydata$mydata_49
  }
  if(exists("mydata$mydata_4B")){
    mydata_4B = mydata$mydata_4B
  }
  if(exists("mydata$mydata_4C")){
    mydata_4C = mydata$mydata_4C
  }



  if (subset_days != "all"){
    tt_begin <- mydata_4B$Timestamp[length(mydata_4B$Timestamp)] - (24*60*60*subset_days)
    #tt_end <- mydata_4B$Timestamp[length(mydata_4B$Timestamp)]
    mydata_4B <- mydata_4B[mydata_4B$Timestamp > tt_begin,]
    mydata_4B <- mydata_4B[is.na(mydata_4B$Timestamp) == FALSE,] #remove NAs
  } else {mydata_4B <- mydata_4B}
  mydata_4B <- mydata_4B %>% drop_na(Timestamp, TT_ID)
  mydata_4B <<- mydata_4B %>% distinct(TT_ID, Timestamp, .keep_all = TRUE) #remove duplicates


  if (subset_days != "all"){
    tt_begin <- mydata_4D$Timestamp[length(mydata_4D$Timestamp)] - (24*60*60*subset_days)
    #tt_end <- mydata_4D$Timestamp[length(mydata_4D$Timestamp)]
    mydata_4D <- mydata_4D[mydata_4D$Timestamp > tt_begin,]
    mydata_4D <<- mydata_4D[is.na(mydata_4D$Timestamp) == FALSE,] #remove NAs
  } else {mydata_4D <<- mydata_4D}
  mydata_4D <- mydata_4D %>% drop_na(Timestamp, TT_ID)
  mydata_4D <<- mydata_4D %>% distinct(TT_ID, Timestamp, .keep_all = TRUE)#remove duplicates

  if (subset_days != "all"){
    tt_begin <- mydata_49$Timestamp[length(mydata_49$Timestamp)] - (24*60*60*subset_days)
    #tt_end <- mydata_49$Timestamp[length(mydata_49$Timestamp)]
    mydata_49 <- mydata_49[mydata_49$Timestamp > tt_begin,]
    mydata_49 <- mydata_49[is.na(mydata_49$Timestamp) == FALSE,] #remove NAs
  } else {mydata_49 <- mydata_49}
  mydata_49 <- mydata_49 %>% drop_na(Timestamp, TT_ID)
  mydata_49 <<- mydata_49 %>% distinct(TT_ID, Timestamp, .keep_all = TRUE)#remove duplicates

  if (subset_days != "all"){
    tt_begin <- mydata_4C$Timestamp[length(mydata_4C$Timestamp)] - (24*60*60*subset_days)
    #tt_end <- mydata_4C$Timestamp[length(mydata_4C$Timestamp)]
    mydata_4C <- mydata_4C[mydata_4C$Timestamp > tt_begin,]
    mydata_4C <- mydata_4C[is.na(mydata_4C$Timestamp) == FALSE,] #remove NAs
  } else {mydata_4C <- mydata_4C}
  mydata_4C <- mydata_4C %>% drop_na(Timestamp, TT_ID)
  mydata_4C <<- mydata_4C %>% distinct(Timestamp, .keep_all = TRUE)#remove duplicates


  return(list(mydata = mydata$mydata,
              mydata_45 = mydata_45,
              mydata_4D = mydata_4D,
              mydata_49 = mydata_49,
              mydata_4B = mydata_4B,
              mydata_4C = mydata_4C))
}


