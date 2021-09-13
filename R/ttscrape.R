ttscrape <- function(IDs,
                     servers_list=c("http://naturetalkers.altervista.org/",
                                    "http://ittn.altervista.org/"),
                     custom_url = NA
                      ) {



  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(signal)
  library(zoo)
  library(prospectr)
  library(RCurl)
  library(bit64)

  #Specifying the urls for desired websites to be scraped
  if(!missing(custom_url)){
    urls  = custom_url
  } else {
    urls  =
      paste0(servers_list,
            rep(IDs,each = length(servers_list)),
            "/ttcloud.txt")
  }



  for( url in urls){
    if (RCurl::url.exists(url) == T){
      mydata_part <- data.table::fread(url,
                                   sep = ";",
                                   header = FALSE,
                                   fill = TRUE,
                                   integer64 = "numeric")
    } else {
      next()
    }

    if(exists(mydata_full)){

      varnum_full <- dim(mydata_full)[2]
      varnum_part <- dim(mydata_part)[2]
      if(varnum_full > varnum_part){
        mydata_part = cbind(mydata_part,
                            matrix(NA,
                                   nrow = dim(mydata_part)[1],
                                   ncol = varnum_full - varnum_part) %>%
                            as.data.frame()
                            )
        mydata_full = rbind(mydata_full,mydata_part)
      } else {
        mydata_full = cbind(mydata_full,
                            matrix(NA,
                                   nrow = dim(mydata_full)[1],
                                   ncol = varnum_part - varnum_full) %>%
                              as.data.frame()
        )
        mydata_full = rbind(mydata_full,mydata_part)
      }


    } else {
      mydata_full = mydata_part
    }
  }


  mydata = mydata[mydata$V1 != 0,]
  mydata_sep =
    separate(mydata,
             V1,
             into = c("SDate", "STime", "TT_ID"),
             sep = "[ ,]")

  #split the dataset
  mydata_4D <- mydata_sep[mydata_sep$V3 == "4D", ]
  mydata_4D <- Filter(function(x)!all(is.na(x)), mydata_4D)
  #convert possible integer64 to integer
  mydata_4D <- mydata_4D %>% mutate_if(bit64::is.integer64, as.integer)


  mydata_45 <- mydata_sep[mydata_sep$V3 == "45", ]
  mydata_45 <- Filter(function(x)!all(is.na(x)), mydata_45)
  #convert possible integer64 to integer
  mydata_45 <- mydata_45 %>% mutate_if(bit64::is.integer64, as.integer)



  mydata_49 <- mydata_sep[mydata_sep$V3 == "49", ]
  mydata_49 <- Filter(function(x)!all(is.na(x)), mydata_49)

  #mydata_49 <- mydata_49[mydata_49$SN>52000000,]#TreeTalkers v3.2 have and ID higher than 52000000
  #remove those columns with only NAs

  #mydata_49[mydata_49>999999999] <- NA
  #conditional filtering for integer64
  #mydata_49 %>%
  #  filter(if (bit64::is.integer64) mydata_49>999999999)

  #convert possible integer64 to integer
  #mydata_49 <- mydata_49 %>% mutate_if(bit64::is.integer64, as.integer)
  #convert all the bands to numeric

  #TODO Check what will give as.numerci, since it is already double?

  # mydata_49$V5 <- as.numeric(mydata_49$V5)
  # mydata_49$V6 <- as.numeric(mydata_49$V6)
  # mydata_49$V7 <- as.numeric(mydata_49$V7)
  # mydata_49$V8 <- as.numeric(mydata_49$V8)
  # mydata_49$V9 <- as.numeric(mydata_49$V9)
  # mydata_49$V10 <- as.numeric(mydata_49$V10)
  # mydata_49$V11 <- as.numeric(mydata_49$V11)
  # mydata_49$V12 <- as.numeric(mydata_49$V12)
  # mydata_49$V13 <- as.numeric(mydata_49$V13)
  # mydata_49$V14 <- as.numeric(mydata_49$V14)
  # mydata_49$V15 <- as.numeric(mydata_49$V15)
  # mydata_49$V16 <- as.numeric(mydata_49$V16)

  #the string 4B and 4C contain only TTcloud data
  mydata_4B <- mydata_sep[mydata_sep$V3 == "4B", ]
  mydata_4B <- Filter(function(x)!all(is.na(x)), mydata_4B)
  mydata_4C <- mydata_sep[mydata_sep$V3 == "4C", ]
  mydata_4C <- Filter(function(x)!all(is.na(x)), mydata_4C)


   #remove thos columns with only NAs

  #convert possible integer64 to integer
  #mydata_4B <- mydata_4B %>% mutate_if(bit64::is.integer64, as.integer)


  #the string 4B and 4C contain only TTcloud data
  #remove thos columns with only NAs



  header_4D <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Tref_0",
      "Theat_0",
      "growt_sens",
      "adc_bandgap",
      "Bits",
      "RH",
      "Tair",
      "gz_mean",
      "gz_sd",
      "gy_mean",
      "gy_sd",
      "gx_mean",
      "gx_sd",
      "Tref_1",
      "Theat_1",
      "StWC",
      "adc_Vbat"
    )
  header_45 <-
    c(
      "SDate",
      "STime",
      "IT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Tref_0",
      "Theat_0",
      "growt_sens",
      "adc_bandgap",
      "adc_Vbat",
      "RH",
      "Tair",
      "gx_mean",
      "gx_sd",
      "gy_mean",
      "gy_sd",
      "gz_mean",
      "gz_sd",
      "Tref_1",
      "Theat_1",
      "StWC"
    )


  header_49 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "AS7263_610",
      "AS7263_680",
      "AS7263_730",
      "AS7263_760",
      "AS7263_810",
      "AS7263_860",
      "AS7262_450",
      "AS7262_500",
      "AS7262_550",
      "AS7262_570",
      "AS7262_600",
      "AS7262_650",
      "integration_T",
      "gain"
    )

  header_4B <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Acc_Recs",
      "Recs_to_be_sent",
      "MCC_tel_op",
      "MNC_tel_op",
      "GSM_reg",
      "GSM_field",
      "Battery",
      "Firmware_ver"
    )

  header_4C <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "TBL_locked",
      "n_first_sens",
      "RSSI_TT1",
      "RSSI_TT2",
      "RSSI_TT3",
      "RSSI_TT4",
      "RSSI_TT5",
      "RSSI_TT6",
      "RSSI_TT7",
      "RSSI_TT8",
      "RSSI_TT9",
      "RSSI_TT10",
      "RSSI_TT11",
      "RSSI_TT12",
      "RSSI_TT13",
      "RSSI_TT14",
      "RSSI_TT15",
      "RSSI_TT16",
      "RSSI_TT17",
      "RSSI_TT18",
      "RSSI_TT19",
      "RSSI_TT20",
      "RSSI_TT21",
      "RSSI_TT22",
      "RSSI_TT23",
      "RSSI_TT24"
    )


  colnames(mydata_4D) <- header_4D
  colnames(mydata_45) <- header_45
  colnames(mydata_49) <- header_49
  colnames(mydata_4B) <- header_4B
  # Length of the 4C string is variable so we need to cut it
  header_4C <- header_4C[1:(dim(mydata_4C)[2])]
  colnames(mydata_4C) <- header_4C

  #TODO actualy id's a hexadecimal, so it make no sense
  #convert the ids to integers
  #mydata_4D$TT_ID <- as.integer(mydata_4D$TT_ID)
  #mydata_49$TT_ID <- as.integer(mydata_49$TT_ID)
  #mydata_4B$TT_ID <- substr(mydata_4B$TT_ID, 2, 8) #remove the "C" from the IDs
  #mydata_4B$TT_ID <- as.integer(mydata_4B$TT_ID);
  #mydata_4C$TT_ID <- substr(mydata_4C$TT_ID, 2, 8) #remove the "C" from the IDs
  #mydata_4C$TT_ID <- as.integer(mydata_4C$TT_ID);

  #timestamp conversion
  #Timestamp of the router when the individual device send the data string. The timestamp is the number of seconds since 1st January 1970.
  mydata_4D$Timestamp <-
    as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01", tz="GMT")
  mydata_4B$Timestamp <-
    as.POSIXct(mydata_4B$Timestamp, origin = "1970-01-01", tz="GMT")
  mydata_49$Timestamp <-
    as.POSIXct(mydata_49$Timestamp, origin = "1970-01-01", tz="GMT")
  mydata_4C$Timestamp <-
    as.POSIXct(mydata_4C$Timestamp, origin = "1970-01-01", tz="GMT")


  #create a color index
  #id_col <- mydata_4D$TT_ID
  #id_col[id_col == max(id_col, na.rm = T)] <- 21
  #id_col[id_col != 21] <- id_col[id_col != 21] - max(id_col, na.rm = T)
  #mydata_4D$id_col <- abs(id_col)



  return(list(mydata = mydata,
              mydata_45 = mydata_45,
              mydata_4D = mydata_4D,
              mydata_49 = mydata_49,
              mydata_4B = mydata_4B,
              mydata_4C = mydata_4C))

}

#TODO
# get subset_days outside function, to separate scrapping and subsetting








