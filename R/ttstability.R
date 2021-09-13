ttstability <- function(mydata_4D){

  #Treetalker is able to record oscillation of tree due to gravity with
  #Spherical Coordinate System. With basic trigonometry, φ, the angle between
  #the gravity vector and the z-axis can be assessed by using equation 10
  #(Fisher, 2010).This capability will improve the monitoring of forest
  #ecosystem resilience against wind impact.

  #Accelerometer (± 0.01°) Manufacturer:NXP/Freescale. Model: Si7006

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


  #4.11.Tree Stability
  Ax <- mydata_4D$gx_mean
  Ay <- mydata_4D$gy_mean
  Az <- mydata_4D$gz_mean
  phi <- tanh((sqrt(Ax^2+Ay^2))/Az)
  #plot(phi, ylim=c(-0.5, -0.2))
  phi[phi < -0.8] <- NA

  rad2deg <- function(rad) {(rad * 180) / (pi)}
  deg2rad <- function(deg) {(deg * pi) / (180)}

  tiltangle <- 20 # in degree
  phi <- rad2deg(tanh((sqrt(Ax^2+Ay^2))/Az)) + tiltangle


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, phi, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "phi", "id_col_ind")



  #create a data frame for output
  df_ttstability <- data.frame(mydata_4D$Timestamp, phi, mydata_4D$TT_ID)
  colnames(df_ttstability) <- c("Timestamp", "phi", "TT_ID")
  return(df_ttstability)

}



