ttStWC <- function(mydata_4D, species, plot_label){
  #example call ttStWC(mydata_4B, "beech", "split")

  #load required packages
  library(ggplot2)


  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col)));
  colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind,
                                    mydata_4D$id_col_ind==id_col_ind$TT_ID[i],
                                    id_col_ind$ID[i])
  }



  #Asgharinia, S., Belelli Marchesini, L., Gianelle, D., and Valentini, R.:
  #Design and Performance Evaluation of Internet of Things (IoT) Based
  #Multifunctional Device for Plant Ecophysiology & Hydrology: Toward Stem
  #Water Content & Sap Flow , EGU General Assembly 2020, Online, 4–8 May 2020,
  #EGU2020-17021, https://doi.org/10.5194/egusphere-egu2020-17021, 2020


  if (species == "spruce"){
    #calibration for spruce
    m <- -5E-6
    b <- 0.2
  }
  if (species == "beech"){
    #calibration for beech
    m <- -4E-5
    b <- 0.6
  }
  if (species == "pine"){
    #calibration for pine
    m <- -8E-6
    b <- 0.3
  }
  if (species == "poplar"){
    #calibration for poplar
    m <- -0.0001
    b <- 1.76
  }

  #4.6.Ref & Heat Probes Temperature
  Tref_0C <-
    127.6 - 0.006045 * mydata_4D$Tref_0 + 1.26E-7 * mydata_4D$Tref_0 ^ 2 -
    1.15E-12 * mydata_4D$Tref_0 ^ 3
  #apply a Savitzky-Golay smoothing
  #dfn['Tref_0f'] = savgol_filter(dfn['Tref_0c'], 11, 2,mode='nearest') # window size 5, polynomial order 3


  Tref_0C[Tref_0C < -20] <- NA
  Tref_0C[Tref_0C > 40] <- NA
  #apply a Savitzky-Golay smoothing
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Tref_0C[mydata_4D$TT_ID == ID[j]]

    if (length(na.omit(ts)) < 11) {next()}

    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    Tref_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }




  NTC1ref <- 29 #(°C)
  ECf_Tref <- mydata_4D$StWC-7.3*(Tref_0C-NTC1ref)
  StWC <- m*ECf_Tref+b


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, StWC, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "StWC", "id_col_ind")
  #d


  #create a data frame for output
  df_ttStWC <- data.frame(mydata_4D$Timestamp, StWC, mydata_4D$TT_ID)
  colnames(df_ttStWC) <- c("Timestamp", "StWC", "TT_ID")
  return(df_ttStWC)
}

