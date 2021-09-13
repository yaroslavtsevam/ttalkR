


Bat_mV <- 2*1100*(mydata_4D$adc_Vbat/mydata_4D$adc_bandgap)


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











x <- mydata_4D$growt_sens
#conversion range according to the manual (September 2020)
x[x > 85000] <- NA
x[x < 30000] <- NA
#f=y0+a*x+b*x^2+c*x^3
a <- 0.000000008
b <- -0.0016
c <- 89.032
f=a*x^2+b*x+c



ID <- unique(mydata_4D$TT_ID)
for (j in 1:length(ID)) {
  myDendro_data_L0 <- data.frame(mydata_4D$TT_ID, mydata_4D$Timestamp, f)#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), f)
  colnames(myDendro_data_L0) <- c("series", "ts", "value")

  # Subset dataset for TT_IDs
  myDendro_data_L0 <- myDendro_data_L0 %>%
    dplyr::filter(series == ID[j])
  if (length(myDendro_data_L0$value)<100){next}




  #remove outliers
  t_05 <- quantile(myDendro_data_L0$value, p=0.05, na.rm=T)
  t_95 <- quantile(myDendro_data_L0$value, p=0.95, na.rm=T)
  myDendro_data_L0$value[myDendro_data_L0$value<t_05] <- NA
  myDendro_data_L0$value[myDendro_data_L0$value>t_95] <- NA

  #remove missisng values
  #myDendro_data_L0 <- na.omit(subset(myDendro_data_L0, select=-series))
  # Missing value: NA is not allowed in the data as changepoint methods are only sensible for regularly spaced data.
  #Replace missing values in time-series data by interpolation (max gap = 2 weeks).
  myDendro_data_L1 <- myDendro_data_L0
  ts <-
    baytrends::fillMissing(myDendro_data_L0$value,
                           span = 24,
                           Dates = NULL,
                           max.fill = 24*7)
  ts[is.na(ts==T)] <- median(ts, na.rm=T) #replace remaining gaps with ts median

  myDendro_data_L1$value <- ts
  #get nighttime data
  #myDendro_data_L0 <- myDendro_data_L0[as.POSIXlt(myDendro_data_L0$ts)$hour<5,]


  #apply a hampel filter
  #myDendro_data_L0$value

  myDendro_data_MAD <- hampel(myDendro_data_L1$value, 24*7, 2) #weekly time window

  myDendro_data_L1$value <- myDendro_data_MAD$y

  #plot(myDendro_data_L1$value, typ="l")


  #convert sharp distance into growth
  myDendro_data_L2 <- myDendro_data_L1
  #myDendro_data_L1$value <- max(a$y) - a$y
  #myDendro_data_L1$value <- max(myDendro_data_L0$value) - myDendro_data_L0$value



  m_binseg <- cpt.mean(myDendro_data_L2$value, penalty = "BIC", method = "BinSeg", Q = 100)
  bkpnts <- cpts(m_binseg)
  for (k in 1:length(bkpnts)){
    if((k-(24*14))<1){next()}
    ref1 <- median(myDendro_data_L2$value[(bkpnts[k]-(24*14)):(bkpnts[k]-(24*7))])
    ref2 <- median(myDendro_data_L2$value[(bkpnts[k]+(24*7)):(bkpnts[k]+(24*14))])
    myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L1$value)] <- myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L2$value)] - (ref2-ref1)
  }

  mydata_4D$dendro[mydata_4D$TT_ID == ID[j]] <- myDendro_data_L2$value
  #mydata_4D$dendro[mydata_4D$TT_ID == ID[j]] <- myDendro_data_L2_spl$y
}


#create a data frame for plotting
df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$dendro, mydata_4D$id_col_ind)
colnames(df1) <- c("Timestamp", "dendrometer", "id_col_ind")



#load required packages
library(suncalc)
library(lubridate)

ID <- unique(mydata_49$IT_ID)

#filter_ttlight <- function(mydata_49){
ID <- unique(mydata_49$IT_ID)
for (j in 1:length(ID)) {
  ts <- mydata_49$AS7263_610[mydata_49$IT_ID == ID[j]]
  if (length(ts) < 11) {
    next()
  }
  ts_filt <- savitzkyGolay(ts, 0, 1, 11)
  #mydata_49$AS7263_610[mydata_49$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
}


#the gain correction seems to occur directly in the ams firmware
mydata_49$gain_factor[mydata_49$gain == 0] <- 1
mydata_49$gain_factor[mydata_49$gain == 1] <- 1
mydata_49$gain_factor[mydata_49$gain == 2] <- 1
mydata_49$gain_factor[mydata_49$gain == 3] <- 1

mydata_49$integration_factor <- mydata_49$integration_T/50

mydata_49$AS7263_610 <- as.numeric(mydata_49$AS7263_610);
mydata_49$AS7263_610[mydata_49$AS7263_610 > 65000] <- NA
mydata_49$AS7263_680 <- as.numeric(mydata_49$AS7263_680);
mydata_49$AS7263_680[mydata_49$AS7263_680 > 65000] <- NA
mydata_49$AS7263_730 <- as.numeric(mydata_49$AS7263_730);
mydata_49$AS7263_730[mydata_49$AS7263_730 > 65000] <- NA
mydata_49$AS7263_760 <- as.numeric(mydata_49$AS7263_760);
mydata_49$AS7263_760[mydata_49$AS7263_760 > 65000] <- NA
mydata_49$AS7263_810 <- as.numeric(mydata_49$AS7263_810);
mydata_49$AS7263_810[mydata_49$AS7263_810 > 65000] <- NA
mydata_49$AS7263_860 <- as.numeric(mydata_49$AS7263_860);
mydata_49$AS7263_860[mydata_49$AS7263_860 > 65000] <- NA
mydata_49$AS7262_450 <- as.numeric(mydata_49$AS7262_450);
mydata_49$AS7262_450[mydata_49$AS7262_450 > 65000] <- NA
mydata_49$AS7262_500 <- as.numeric(mydata_49$AS7262_500);
mydata_49$AS7262_500[mydata_49$AS7262_500 > 65000] <- NA
mydata_49$AS7262_550 <- as.numeric(mydata_49$AS7262_550);
mydata_49$AS7262_550[mydata_49$AS7262_550 > 65000] <- NA
mydata_49$AS7262_570 <- as.numeric(mydata_49$AS7262_570);
mydata_49$AS7262_570[mydata_49$AS7262_570 > 65000] <- NA
mydata_49$AS7262_600 <- as.numeric(mydata_49$AS7262_600);
mydata_49$AS7262_600[mydata_49$AS7262_600 > 65000] <- NA
mydata_49$AS7262_650 <- as.numeric(mydata_49$AS7262_650);
mydata_49$AS7262_650[mydata_49$AS7262_650 > 65000] <- NA

#Near Infrared
AS7263_610_R <- mydata_49$AS7263_610*mydata_49$integration_factor*mydata_49$gain_factor
#-312.45+(1.6699* (mydata_49$AS7263_610*mydata_49$integration_factor*mydata_49$gain_factor))
AS7263_680_R <- mydata_49$AS7263_680*mydata_49$integration_factor*mydata_49$gain_factor
#-561.56+(1.5199* (mydata_49$AS7263_680*mydata_49$integration_factor*mydata_49$gain_factor))
AS7263_730_R <- mydata_49$AS7263_730*mydata_49$integration_factor*mydata_49$gain_factor
#-1511.2+(1.6209* (mydata_49$AS7263_730*mydata_49$integration_factor*mydata_49$gain_factor))
AS7263_760_R <- mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor
#-1012.5+(1.4549* (mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor))
AS7263_810_R <- mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor
#91.58+(0.8414* (mydata_49$AS7263_810*mydata_49$integration_factor*mydata_49$gain_factor))
AS7263_860_R <- mydata_49$AS7263_810*mydata_49$integration_factor*mydata_49$gain_factor
#334.88+(0.531* (mydata_49$AS7263_860*mydata_49$integration_factor*mydata_49$gain_factor))

#Visible Light Spectrum
AS7262_450_R <- mydata_49$AS7262_450*mydata_49$integration_factor*mydata_49$gain_factor
#-212.62+(0.4562* (mydata_49$AS7262_450*mydata_49$integration_factor*mydata_49$gain_factor))
AS7262_500_R <- mydata_49$AS7262_500*mydata_49$integration_factor*mydata_49$gain_factor
#-232.13+(0.6257 * (mydata_49$AS7262_500*mydata_49$integration_factor*mydata_49$gain_factor))
AS7262_550_R <- mydata_49$AS7262_550*mydata_49$integration_factor*mydata_49$gain_factor
#-842.1+(1.0546 * (mydata_49$AS7262_550*mydata_49$integration_factor*mydata_49$gain_factor))
AS7262_570_R <- mydata_49$AS7262_570*mydata_49$integration_factor*mydata_49$gain_factor
#-666.72+(1.0462 * (mydata_49$AS7262_570*mydata_49$integration_factor*mydata_49$gain_factor))
AS7262_600_R <- mydata_49$AS7262_600*mydata_49$integration_factor*mydata_49$gain_factor
#-328.08+(0.8654 * (mydata_49$AS7262_600*mydata_49$integration_factor*mydata_49$gain_factor))
AS7262_650_R <- mydata_49$AS7262_650*mydata_49$integration_factor*mydata_49$gain_factor
#202.77+(0.7829* (mydata_49$AS7262_650*mydata_49$integration_factor*mydata_49$gain_factor))

#solar geometry
#"altitude" : sun altitude above the horizon in radians, e.g. 0 at the horizon and PI/2 at the zenith (straight over your head)
#"azimuth": sun azimuth in radians (direction along the horizon,measured from south to west), e.g. 0 is south and Math.PI * 3/4 is northwest

solarGeom <-  getSunlightPosition(date= mydata_49$Timestamp, lat = lat, lon = lon)
solarGeom$altitude <- (solarGeom$altitude * 180) / (pi)
solarGeom$azimuth <-  (solarGeom$azimuth * 180) / (pi)

TT_ID <- mydata_49$TT_ID
Timestamp <- mydata_49$Timestamp

specttRal <- data.frame(TT_ID,
                        Timestamp,
                        subset(solarGeom, select =-date),
                        AS7262_450_R,
                        AS7262_500_R,
                        AS7262_550_R,
                        AS7262_570_R,
                        AS7262_600_R,
                        AS7262_650_R,
                        AS7263_610_R,
                        AS7263_680_R,
                        AS7263_730_R,
                        AS7263_760_R,
                        AS7263_810_R,
                        AS7263_860_R
)
#out[out<0] <- NA

#it is possible to find dates out of range
specttRal <- specttRal[format(specttRal$Timestamp, format="%Y")>2010,]

#keep data with with sun in +/-30 degrees from solar noon
specttRal <- specttRal[(specttRal$azimuth > -30) & (specttRal$azimuth < 30),]


datalist = list() #initialize the list
ID <- unique(specttRal$TT_ID)
for (j in 1:length(ID)) {

  # Subset dataset for TT_IDs
  specttRal_L0 <- specttRal %>%
    dplyr::filter(TT_ID == ID[j])

  if (length(specttRal_L0$Timestamp)<10){next}

  #######aggregate to daily
  specttRal_L0$Day <- floor_date(specttRal_L0$Timestamp, "day")

  specttRal_L1 <- specttRal_L0 %>%
    group_by(Day)  %>%
    summarize(L450_R = median(AS7262_450_R, na.rm = TRUE),
              L500_R = median(AS7262_500_R, na.rm = TRUE),
              L550_R = median(AS7262_550_R, na.rm = TRUE),
              L570_R = median(AS7262_570_R, na.rm = TRUE),
              L600_R = median(AS7262_600_R, na.rm = TRUE),
              L650_R = median(AS7262_650_R, na.rm = TRUE),
              L610_R = median(AS7263_610_R, na.rm = TRUE),
              L680_R = median(AS7263_680_R, na.rm = TRUE),
              L730_R = median(AS7263_730_R, na.rm = TRUE),
              L760_R = median(AS7263_760_R, na.rm = TRUE),
              L810_R = median(AS7263_810_R, na.rm = TRUE),
              L860_R = median(AS7263_860_R, na.rm = TRUE)
    )
  specttRal_L1$TT_ID <- rep(ID[j], length(specttRal_L1$Day))
  #colnames(mydata_daily) <- c("date", "phi")
  datalist[[j]] <- specttRal_L1 # add it to your list

}
specttRal_L1_all <- dplyr::bind_rows(datalist)


#create a color index
id_col <- specttRal_L1_all$TT_ID
id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
#create index for color scale
specttRal_L1_all$id_col_ind <- specttRal_L1_all$TT_ID
for (i in 1:length(id_col_ind$ID)){
  specttRal_L1_all$id_col_ind <- replace(specttRal_L1_all$id_col_ind, specttRal_L1_all$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
}



if (wavelength == 450){spectRal <- specttRal_L1_all$L450_R}
if (wavelength == 500){spectRal <- specttRal_L1_all$L500_R}
if (wavelength == 550){spectRal <- specttRal_L1_all$L550_R}
if (wavelength == 570){spectRal <- specttRal_L1_all$L570_R}
if (wavelength == 600){spectRal <- specttRal_L1_all$L600_R}
if (wavelength == 650){spectRal <- specttRal_L1_all$L650_R}
if (wavelength == 610){spectRal <- specttRal_L1_all$L610_R}
if (wavelength == 680){spectRal <- specttRal_L1_all$L680_R}
if (wavelength == 730){spectRal <- specttRal_L1_all$L730_R}
if (wavelength == 760){spectRal <- specttRal_L1_all$L760_R}
if (wavelength == 810){spectRal <- specttRal_L1_all$L810_R}
if (wavelength == 860){spectRal <- specttRal_L1_all$L860_R}


df1 <- data.frame(specttRal_L1_all$Day, spectRal, specttRal_L1_all$id_col_ind)
colnames(df1) <- c("Timestamp", "wavelength", "id_col_ind")


