ttbattery <- function(mydata_4B, mydata_4D){

  # Should work also for mydata_45
  #load required packages
  HR_Timestamp_4D <- mydata_4D$Timestamp
  HR_Timestamp_4B <- mydata_4B$Timestamp


  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }

  #4.8.Battery Voltage
  Bat_mV <- 2*1100*(mydata_4D$adc_Vbat/mydata_4D$adc_bandgap)


  #create a data frame for plotting
  df <- data.frame(HR_Timestamp_4D, Bat_mV, mydata_4D$id_col_ind)
  df1 <- data.frame(HR_Timestamp_4B, mydata_4B$Battery)
  colnames(df1) <- c("HR_Timestamp_4B", "Bat_mV")
  #create a data frame for output
  df_ttbattery <- data.frame(mydata_4D$Timestamp, Bat_mV, mydata_4D$TT_ID)
  colnames(df_ttbattery) <- c("Timestamp", "Bat_mV", "TT_ID")
  return(df_ttbattery)

}
