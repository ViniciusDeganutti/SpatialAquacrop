Date_count <- function (Year, month, Day){
  Y_1901 <- if(as.numeric(Year) < 1901){ print (" Year is in the wrong place or not in the range of 1901 to 2099 ")
  } else if(as.numeric(Year)> 2099){ print (" Year is in the wrong place or not in the range of 1901 to 2099 ")
  }else {Y_1901 <- as.numeric(Year)-1901}
  Yx365 <- Y_1901*365.25
  Ymonth <-  if (as.numeric(month) > 12) { print (" months are from 01 until 12 only")}
  else if (as.numeric(month)== 01){Ymonth <- Yx365+0
  }else if (as.numeric(month)==02){Ymonth <- Yx365+31
  }else if (as.numeric(month)== 03){Ymonth <- Yx365+59.25
  }else if (as.numeric(month)== 04){Ymonth <- Yx365+90.25
  }else if (as.numeric(month)== 05){Ymonth <- Yx365+120.25
  }else if (as.numeric(month)== 06){Ymonth <- Yx365+151.25
  }else if (as.numeric(month)== 07){Ymonth <- Yx365+181.25
  }else if (as.numeric(month)==08){Ymonth <- Yx365 + 212.25
  }else if (as.numeric(month)== 09){Ymonth <- Yx365+243.25
  }else if (as.numeric(month)== 10){Ymonth <- Yx365+273.25
  }else if (as.numeric(month)== 11){Ymonth <- Yx365+304.25
  }else {Ymonth <- Yx365+334.25}
  YmonthD <- if (as.numeric(Day)>31){print (" Days are wrong, it should be from 01 until 31")}
  else {YmonthD <- as.numeric(Ymonth)+as.numeric(Day)
  Yinteger <- as.integer(YmonthD)
  return (Yinteger) }
}

Control_AQC <- function(pathA){
  setwd(pathA)
  library(sp)
  library(raster)
  library(ncdf4)
  general <- read.table("Data_Fill.csv",sep=";")
  nbcrops <- as.numeric(general[54,2])
  ir1 <- (general[58,2])
  fm1 <- (general[59,2])
  gt1 <- (general[60,2])
  
  if (ir1==0) {
    message("The model will run considering rainfed condition")
  }
  else if (file.exists("IRRI.IRR")){
    print("Irrigation file exists")
  }
  else {
    irri1 <- read.table("Data_Irri1.csv",sep=";")
    irri2 <- read.table("Data_Irri2.csv",sep=";")

    irrifile <- file.create("IRRI.IRR")
    irrifile[1] <- paste("An Irrigation Schedule")
    irrifile[2] <- paste("   6.0      : AquaCrop Version")
    irrifile[3] <- paste("  ",irri1[1,1],"     :Irrigation Type")
    irrifile[4] <- paste(" ",irri1[2,1],"     : Percentage of soil surface wetted by irrigation")
    irrifile[5] <- paste("   1     : Irrigation schedule")
    irrifile[6] <- paste("")
    irrifile[7] <- paste("   Day    Depth (mm)   ECw (dS/m)")
    irrifile[8] <- paste("====================================")
    for (i in 1:nrow(irri2)){
      l <- 8+i
      irrifile[l] <-paste("    ",irri2[i,1],"       ",irri2[i,2],"          ",irri2[i,3])

    }
    writeLines(irrifile,"IRRI.IRR")
  }
  if (file.exists("MANE.MAN")){
    print("Management file exists")
  }
  else if (fm1 == 0) {
    message("The model will run without considering field management")
  }
  else {
    fieldm <- read.table("Data_Man.csv",sep=";")
    fieldmfile <- file.create("MANE.MAN")
    fieldmfile [1] <- paste(" ")
    fieldmfile [2] <- paste("     6.1       : AquaCrop Version ")
    fieldmfile [3] <- paste ("    ",fieldm[1,1],"        : percentage (%) of ground surface covered by mulches IN growing period")
    fieldmfile [4] <- paste("    ",fieldm[2,1],"         : effect (%) of mulches on reduction of soil evaporation")
    fieldmfile [5] <- paste("    ",fieldm[3,1],"         : Degree of soil fertility stress (%) - Effect is crop specific")
    fieldmfile [6] <- paste("    ",fieldm[4,1],"        : height (m) of soil bunds")
    fieldmfile [7] <- paste ("    ",fieldm[5,1],"         : surface runoff NOT affected by field surface practices")
    fieldmfile [8] <- paste ("    ",fieldm[6,1],"         : % increase/decrease of soil profile CN value (0 if Not Applicable)")
    fieldmfile [9] <- paste ("    ",fieldm[7,1],"          : relative cover of weeds at canopy closure (%)")
    fieldmfile [10] <- paste ("    ",fieldm[8,1],"        : Increase/decrease of relative cover of weeds in mid season (%)")
    fieldmfile [11] <- paste("    ",fieldm[9,1],"         : Shape factor of the CC expansion function in a weed infested field")
    writeLines(fieldmfile, "MANE.MAN")
  }
  if(file.exists("GWTR.GWT")){
    print("Groundwater file exists")
  }
  else if (gt1== 0) {
    message("The model will consider there is no groundwater table")
  }
  else {
    gwt <- read.table("Data_GWT.csv",sep=";")

    grdfile <- file.create("GWTR.GWT")
    grdfile [1] <- paste(" ")
    grdfile [2] <- paste("     6.1       : AquaCrop Version ")
    grdfile [3] <- paste("     1     : groundwater table at fixed depth and with constant salinity")
    grdfile [4] <- paste("")
    grdfile [5] <- paste("   Day    Depth (m)    ECw (dS/m)")
    grdfile [6] <- paste("====================================")
    grdfile [7] <- paste("     ",gwt[2,1],"      ",gwt[3,1],"          ",gwt[4,1])
    writeLines(grdfile, "GWTR.GWT")
  }

  if(file.exists("CROP1.CRO")){
    print("Crop file exists")
  }
  else{
    cropfun <- read.table("Data_Crop.csv",sep=";")
    crop <- rapply(cropfun,function(y) ifelse(y=="NA",-9,y), how = "replace")

    for (j in 1:nbcrops){

      cropf <-file.create(paste("CROP",j,".CRO"))
      cropf[1] <- paste(" ")
      cropf[2] <- paste("     6.0       : AquaCrop Version (March 2017)")
      cropf[3] <- paste("     0         : File protected")
      cropf[4] <- paste("     ",crop[1,j],"         : Crop type")
      cropf[5] <- paste("     ",crop[2,j],"         : Cropping")
      cropf[6] <- paste("     ",crop[3,j],"         : Crop cycle")
      cropf[7] <- paste("     1         : Soil water depletion factors (p) are adjusted by ETo")
      cropf[8] <- paste("     ",crop[4,j],"         : Base temperature")
      cropf[9] <- paste("     ",crop[5,j],"         : Upper temperature")
      cropf[10] <- paste("     ",crop[6,j],"         : Total length of crop cycle")
      cropf[11] <- paste("     ",crop[7,j],"         : Soil water depletion factor - upper")
      cropf[12] <- paste("     ",crop[8,j],"         : Soil water depletion factor - lower")
      cropf[13] <- paste("     ",crop[9,j],"       : Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)")
      cropf[14] <- paste("     ",crop[10,j],"      : Soil water depletion fraction for stomatal control (p - sto) - Upper threshold")
      cropf[15] <- paste("     ",crop[11,j],"       : Shape factor for water stress coefficient for stomatal control (0.0 = straight line)")
      cropf[16] <- paste("     ",crop[12,j],"      : Soil water depletion factor for canopy senescence (p - sen) - Upper threshold")
      cropf[17] <- paste("     ",crop[13,j],"       : Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)")
      cropf[18] <- paste("     ",crop[14,j],"         : Sum(ETo) during stress period to be exceeded before senescence is triggered")
      cropf[19] <- paste("     ",crop[15,j],"      : Soil water depletion factor for pollination (p - pol) - Upper threshold")
      cropf[20] <- paste("     ",crop[16,j],"         : Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)")
      cropf[21] <- paste("    ",crop[17,j],"         : Considered soil fertility stress for calibration of stress response (%)")
      cropf[22] <- paste("    ",crop[18,j],"      : Response of canopy expansion is not considered")
      cropf[23] <- paste("    ",crop[19,j],"      : Response of maximum canopy cover is not considered")
      cropf[24] <- paste("    ",crop[20,j],"      : Response of crop Water Productivity is not considered")
      cropf[25] <- paste("    ",crop[21,j],"      : Response of decline of canopy cover is not considered")
      cropf[26] <- paste("    -9         : dummy - Parameter no Longer required")
      cropf[27] <- paste("    ",crop[22,j],"         : Minimum air temperature below which pollination starts to fail (cold stress) (°C)")
      cropf[28] <- paste("    ",crop[23,j],"         : Maximum air temperature above which pollination starts to fail (heat stress) (°C)")
      cropf[29] <- paste("    ",crop[24,j],"       : Minimum growing degrees required for full crop transpiration (°C - day)")
      cropf[30] <- paste("     ",crop[25,j],"         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)")
      cropf[31] <- paste("    ",crop[26,j],"         : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)")
      cropf[32] <- paste("    -9         : Dummy - no longer applicable")
      cropf[33] <- paste("    ",crop[27,j],"         : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))")
      cropf[34] <- paste("   ",crop[28,j],"         : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))")
      cropf[35] <- paste("     ",crop[29,j],"      : Crop coefficient when canopy is complete but prior to senescence (KcTr,x)")
      cropf[36] <- paste("     ",crop[30,j],"     : Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.")
      cropf[37] <- paste("     ",crop[31,j],"      : Minimum effective rooting depth (m)")
      cropf[38] <- paste("     ",crop[32,j],"      : Maximum effective rooting depth (m)")
      cropf[39] <- paste("    ",crop[33,j],"         : Shape factor describing root zone expansion")
      cropf[40] <- paste("     ",crop[34,j],"     : Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone")
      cropf[41] <- paste("     ",crop[35,j],"     : Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone")
      cropf[42] <- paste("    ",crop[36,j],"         : Effect of canopy cover in reducing soil evaporation in late season stage")
      cropf[43] <- paste ("    ",crop[37,j],"        : Soil surface covered by an individual seedling at 90 % emergence (cm2)")
      cropf[44] <- paste ("    ",crop[38,j],"        : Canopy size of individual plant (re-growth) at 1st day (cm2)")
      cropf[45] <- paste ("    ",crop[39,j],"        : Number of plants per hectare")
      cropf[46] <- paste ("    ",crop[40,j],"        : Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)")
      cropf[47] <- paste ("    ",crop[41,j],"        : Maximum decrease of Canopy Growth Coefficient in and between seasons Growth Coefficient is reached")
      cropf[48] <- paste ("    ",crop[42,j],"        : Number of seasons at which maximum decrease of Canopy Growth Coefficient is reached")
      cropf[49] <- paste ("    ",crop[43,j],"        : Shape factor for decrease Canopy Growth Coefficient")
      cropf[50] <- paste ("    ",crop[44,j],"        : Maximum canopy cover (CCx) in fraction soil cover")
      cropf[51] <- paste ("    ",crop[45,j],"        : Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)")
      cropf[52] <- paste ("    ",crop[46,j],"        : Calendar Days: from sowing to emergence")
      cropf[53] <- paste ("    ",crop[47,j],"        : Calendar Days: from sowing to maximum rooting depth")
      cropf[54] <- paste ("    ",crop[48,j],"        : Calendar Days: from sowing to start senescence")
      cropf[55] <- paste ("    ",crop[49,j],"        : Calendar Days: from sowing to maturity (length of crop cycle)")
      cropf[56] <- paste ("    ",crop[50,j],"        : Calendar Days: from sowing to flowering")
      cropf[57] <- paste ("    ",crop[51,j],"        : Length of the flowering stage (days)")
      cropf[58] <- paste ("    ",crop[52,j],"        : Crop determinancy (liked = 1, 0 = unlinked with flowering)")
      cropf[59] <- paste ("    ",crop[53,j],"        : Excess of potential fruits (%)")
      cropf[60] <- paste ("    ",crop[54,j],"        : Building up of Harvest Index starting at flowering (days)")
      cropf[61] <- paste ("    ",crop[55,j],"        : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)")
      cropf[62] <- paste ("    ",crop[56,j],"        : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)")
      cropf[63] <- paste ("    ",crop[57,j],"        : Crop performance under elevated atmospheric CO2 concentration (%)")
      cropf[64] <- paste ("    ",crop[58,j],"        : Reference Harvest Index (HIo) (%)")
      cropf[65] <- paste ("    ",crop[59,j],"        : Possible increase (%) of HI due to water stress before flowering")
      cropf[66] <- paste ("    ",crop[60,j],"        : Coefficient describing positive impact on HI of stomatal closure during yield formation")
      cropf[67] <- paste ("    ",crop[61,j],"        : Coefficient describing negative impact on HI of stomatal closure during yield formation")
      cropf[68] <- paste ("    ",crop[62,j],"        : Allowable maximum increase (%) of specified HI")
      cropf[69] <- paste ("    ",crop[63,j],"        : GDDays: from sowing to emergence")
      cropf[70] <- paste ("    ",crop[64,j],"        : GDDays: from sowing to maximum rooting depth")
      cropf[71] <- paste ("    ",crop[65,j],"        : GDDays: from sowing to start senescence")
      cropf[72] <- paste ("    ",crop[66,j],"        : GDDays: from sowing to maturity (length of crop cycle)")
      cropf[73] <- paste ("    ",crop[67,j],"        : GDDays: from sowing to flowering")
      cropf[74] <- paste("    ",crop[68,j],"         : Length of the flowering stage (growing degree days)")
      cropf[75] <- paste ("    ",crop[69,j],"  : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)")
      cropf[76] <- paste("    ",crop[70,j],"   : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)")
      cropf[77] <- paste ("    ",crop[71,j],"       : GDDays: building-up of Harvest Index during yield formation")
      writeLines(cropf, paste("CROP",j,".CRO"))

      if (j==1){
        file.rename(paste("CROP",j,".CRO"),"CROP1.CRO")
      }
      else if (j==2){
        file.rename(paste("CROP",j,".CRO"),"CROP2.CRO")
      }
      else if (j==3){
        file.rename(paste("CROP",j,".CRO"),"CROP3.CRO")
      }
      else if (j==4){
        file.rename(paste("CROP",j,".CRO"),"CROP4.CRO")
      }
      else if (j==5){
        file.rename(paste("CROP",j,".CRO"),"CROP5.CRO")
      }
      else {
        file.rename(paste("CROP",j,".CRO"),"CROP6.CRO")
      }
    }
  }

Mauna_loa <- file.create("MaunaLoa.CO2")  # Further version for the user to imput their own
Mauna_loa[1] <- paste ("Default atmospheric CO2 concentration from 1902 to 2099")
Mauna_loa[2] <- paste("Year     CO2 (ppm by volume)")
Mauna_loa[3] <- paste("============================")
dataf1 <- c(1902,
            1905,
            1912  ,
            1915,
            1924,
            1926,
            1929,
            1932,
            1934,
            1936,
            1938,
            1939,
            1940,
            1944,
            1948,
            1953,
            1954,
            1958,
            1959,
            1960,
            1961,
            1962,
            1963,
            1964,
            1965,
            1966,
            1967,
            1968,
            1969,
            1970,
            1971,
            1972,
            1973,
            1974,
            1975,
            1976,
            1977,
            1978,
            1979,
            1980,
            1981,
            1982,
            1983,
            1984,
            1985,
            1986,
            1987,
            1988,
            1989,
            1990,
            1991,
            1992,
            1993,
            1994,
            1995,
            1996,
            1997,
            1998,
            1999,
            2000,
            2001,
            2002,
            2003,
            2004,
            2005,
            2006,
            2007,
            2008,
            2009,
            2010,
            2011,
            2012,
            2013,
            2014,
            2015,
            2016,
            2020,
            2099)
dataf2 <- c(  297.4,
              298.2,
              300.7,
              301.3,
              304.5,
              305.0,
              305.2,
              307.8,
              309.2,
              307.9,
              310.5,
              310.1,
              310.5,
              309.7,
              310.7,
              311.9,
              314.1,
              315.29,
              315.97,
              316.91,
              317.64,
              318.45,
              318.99,
              319.62,
              320.04,
              321.38,
              322.16,
              323.04,
              324.62,
              325.68,
              326.32,
              327.45,
              329.68,
              330.18,
              331.11,
              332.04,
              333.83,
              335.40,
              336.84,
              338.75,
              340.11,
              341.45,
              343.05,
              344.65,
              346.12,
              347.42,
              349.19,
              351.57,
              352.12,
              354.39,
              355.61,
              356.45,
              356.10,
              358.83,
              360.82,
              362.61,
              363.73,
              366.70,
              368.38,
              369.55,
              371.14,
              373.28,
              375.80,
              377.52,
              379.80,
              381.90,
              383.79,
              385.60,
              387.43,
              389.90,
              391.65,
              393.85,
              396.52,
              398.65,
              400.83,
              404.21,
              412.21,
              570.21)
for (x in 1:length(dataf1)){
  z=3+x
  Mauna_loa[z] <- paste(" ",dataf1[x],"  ",dataf2[x])
}
writeLines(Mauna_loa,"MaunaLoa.CO2")

  simul1 <- Date_count(as.numeric(general[3,2]),as.numeric(general[2,2]),as.numeric(general[1,2]))
  simul2 <- Date_count(as.numeric(general[6,2]),as.numeric(general[5,2]),as.numeric(general[4,2]))
  crop1 <- Date_count(as.numeric(general[9,2]),as.numeric(general[8,2]),as.numeric(general[7,2]))
  crop2 <- Date_count(as.numeric(general[12,2]),as.numeric(general[11,2]),as.numeric(general[10,2]))

  Msim <-  if (as.numeric(general[2,2])== 01){Msim <- "January"
  }else if (as.numeric(general[2,2])==02){Msim <- "February"
  }else if (as.numeric(general[2,2])== 03){Msim <- "March"
  }else if (as.numeric(general[2,2])== 04){Msim <- "April"
  }else if (as.numeric(general[2,2])== 05){Msim <- "May"
  }else if (as.numeric(general[2,2])== 06){Msim <- "June"
  }else if (as.numeric(general[2,2])== 07){Msim <- "July"
  }else if (as.numeric(general[2,2])==08){Msim <- "August"
  }else if (as.numeric(general[2,2])== 09){Msim <- "September"
  }else if (as.numeric(general[2,2])== 10){Msim<- "October"
  }else if (as.numeric(general[2,2])== 11){Msim <- "November"
  }else {Msim<- "December"}

  Msim_fin <-  if (as.numeric(general[5,2])== 01){Msim_fin <- "January"
  }else if (as.numeric(general[5,2])==02){Msim_fin <- "February"
  }else if (as.numeric(general[5,2])== 03){Msim_fin <- "March"
  }else if (as.numeric(general[5,2])== 04){Msim_fin <- "April"
  }else if (as.numeric(general[5,2])== 05){Msim_fin <- "May"
  }else if (as.numeric(general[5,2])== 06){Msim_fin <- "June"
  }else if (as.numeric(general[5,2])== 07){Msim_fin <- "July"
  }else if (as.numeric(general[5,2])==08){Msim_fin<- "August"
  }else if (as.numeric(general[5,2])== 09){Msim_fin <- "September"
  }else if (as.numeric(general[5,2])== 10){Msim_fin<- "October"
  }else if (as.numeric(general[5,2])== 11){Msim_fin <- "November"
  }else {Msim_fin<- "December"}

  Mcrop <-  if (as.numeric(general[8,2])== 01){Mcrop <- "January"
  }else if (as.numeric(general[8,2])==02){Mcrop <- "February"
  }else if (as.numeric(general[8,2])== 03){Mcrop <- "March"
  }else if (as.numeric(general[8,2])== 04){Mcrop <- "April"
  }else if (as.numeric(general[8,2])== 05){Mcrop <- "May"
  }else if (as.numeric(general[8,2])== 06){Mcrop<- "June"
  }else if (as.numeric(general[8,2])== 07){Mcrop <- "July"
  }else if (as.numeric(general[8,2])==08){Mcrop <- "August"
  }else if (as.numeric(general[8,2])== 09){Mcrop <- "September"
  }else if (as.numeric(general[8,2])== 10){Mcrop<- "October"
  }else if (as.numeric(general[8,2])== 11){Mcrop <- "November"
  }else {Mcrop<- "December"}

  Mcrop_fin <-  if (as.numeric(general[11,2])== 01){Mcrop_fin <- "January"
  }else if (as.numeric(general[11,2])==02){Mcrop_fin <- "February"
  }else if (as.numeric(general[11,2])== 03){Mcrop_fin <- "March"
  }else if (as.numeric(general[11,2])== 04){Mcrop_fin <- "April"
  }else if (as.numeric(general[11,2])== 05){Mcrop_fin <- "May"
  }else if (as.numeric(general[11,2])== 06){Mcrop_fin <- "June"
  }else if (as.numeric(general[11,2])== 07){Mcrop_fin <- "July"
  }else if (as.numeric(general[11,2])==08){Mcrop_fin<- "August"
  }else if (as.numeric(general[11,2])== 09){Mcrop_fin <- "September"
  }else if (as.numeric(general[11,2])== 10){Mcrop_fin<- "October"
  }else if (as.numeric(general[11,2])== 11){Mcrop_fin <- "November"
  }else {Mcrop_fin<- "December"}


  for (v in 1:nbcrops){
   controlfile <- file.create("Input_File.PRO")

  controlfile[1] <- paste(" ")
  controlfile[2] <- paste("      6.0       : AquaCrop Version (May 2018)")
  controlfile[3] <- paste(" ", simul1,"        : First day of simulation period - ", general[1,2],Msim,general[3,2] )
  controlfile[4] <- paste(" ", simul2,"        : Last day of simulation period - ", general[4,2],Msim_fin,general[6,2] )
  controlfile[5] <- paste(" ", crop1,"        : First day of cropping period - ", general[7,2],Mcrop,general[9,2])
  controlfile[6] <- paste(" ", crop2,"        : Last day of cropping period - ", general[10,2],Mcrop_fin,general[12,2])
  controlfile[7] <- paste("      4         : Evaporation decline factor for stage II")
  controlfile[8] <- paste("      1.10      : Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface")
  controlfile[9] <- paste("      5         : Threshold for green CC below which HI can no longer increase (% cover)")
  controlfile[10] <- paste("     70         : Starting depth of root zone expansion curve (% of Zmin)")
  controlfile[11] <- paste("      5.00      : Maximum allowable root zone expansion (fixed at 5 cm/day)")
  controlfile[12] <- paste("     -6         : Shape factor for effect water stress on root zone expansion")
  controlfile[13] <- paste("     20         : Required soil water content in top soil for germination (% TAW)")
  controlfile[14] <- paste("      1.0       : Adjustment factor for FAO-adjustment soil water depletion (p) by ETo")
  controlfile[15] <- paste("      3         : Number of days after which deficient aeration is fully effective")
  controlfile[16] <- paste("      1.00      : Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop")
  controlfile[17] <- paste("     12         : Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))")
  controlfile[18] <- paste("     10         : Thickness top soil (cm) in which soil water depletion has to be determined")
  controlfile[19] <- paste("     30         : Depth [cm] of soil profile affected by water extraction by soil evaporation")
  controlfile[20] <- paste("      0.30      : Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment")
  controlfile[21] <- paste("      1         : CN is adjusted to Antecedent Moisture Class")
  controlfile[22] <- paste("     20         : salt diffusion factor (capacity for salt diffusion in micro pores) [%]")
  controlfile[23] <- paste("    100         : salt solubility [g/liter]")
  controlfile[24] <- paste("     16         : shape factor for effect of soil water content gradient on capillary rise")
  controlfile[25] <- paste("     12.0       : Default minimum temperature (?C) if no temperature file is specified")
  controlfile[26] <- paste("     28.0       : Default maximum temperature (?C) if no temperature file is specified")
  controlfile[27] <- paste("      3         : Default method for the calculation of growing degree days")
  controlfile[28] <- paste("-- 1. Climate (CLI) file")
  controlfile[29] <- paste("   climate.CLI")
  controlfile[30] <- paste("  ",general[57,2])
  controlfile[31] <- paste("   1.1 Temperature (Tnx or TMP) file")
  controlfile[32] <- paste("   temp.Tnx")
  controlfile[33] <- paste("  ",general[57,2])
  controlfile[34] <- paste("   1.2 Reference ET (ETo) file")
  controlfile[35] <- paste("   etref.ETo")
  controlfile[36] <- paste("  ",general[57,2])
  controlfile[37] <- paste("   1.3 Rain (PLU) file")
  controlfile[38] <- paste("   precipitation.PLU")
  controlfile[39] <- paste("  ",general[57,2])
  controlfile[40] <- paste("   1.4 Atmospheric CO2 concentration (CO2) file")
  controlfile[41] <- paste("   MaunaLoa.CO2")
  controlfile[42] <- paste("  ",general[57,2])
  controlfile[43] <- paste("-- 2. Crop (CRO) file")

 if (v==1){
   controlfile[44] <- paste("   CROP1.CRO")
 }
  else if (v==2) {
    controlfile[44] <- paste("   CROP2.CRO")
  }
  else if (v==3){
    controlfile[44] <- paste("   CROP3.CRO")
  }
  else if (v==4){
    controlfile[44] <- paste("   CROP4.CRO")
  }
  else if (v==5){
    controlfile[44] <- paste("   CROP5.CRO")
  }
  else {
    controlfile[44] <- paste("   CROP6.CRO")
  }

  controlfile[45] <- paste("  ",general[57,2])
  if (file.exists("IRRI.IRR")){
    controlfile[46] <- paste("-- 3. Irrigation management (IRR) file")
    controlfile[47] <- paste("   IRRI.IRR")
    controlfile[48] <- paste("  ",general[57,2])
  }
  else {
    controlfile[46] <- paste("-- 3. Irrigation management (IRR) file")
    controlfile[47] <- paste("   (None)")
    controlfile[48] <- paste("   (None)")
  }
  if (file.exists("MANE.MAN")){
    controlfile[49] <- paste("-- 4. Field management (MAN) file")
    controlfile[50] <- paste("   MANE.MAN")
    controlfile[51] <- paste("  ",general[57,2])
  }
  else {
    controlfile[49] <- paste("-- 4. Field management (MAN) file")
    controlfile[50] <- paste("   (None)")
    controlfile[51] <- paste("   (None)")
  }

  controlfile[52] <- paste("-- 5. Soil profile (SOL) file")
  controlfile[53] <- paste("   soil.SOL")
  controlfile[54] <- paste("  ",general[57,2])
  if (file.exists("GWTR.GWT")){
    controlfile[55] <- paste("-- 6. Groundwater table (GWT) file")
    controlfile[56] <- paste("   GWTR.GWT")
    controlfile[57] <- paste("  ",general[57,2])
  }
  else{
    controlfile[55] <- paste("-- 6. Groundwater table (GWT) file")
    controlfile[56] <- paste("   (None)")
    controlfile[57] <- paste("   (None")
  }

  controlfile[58] <- paste("-- 7. Initial conditions (SW0) file")
  controlfile[59] <- paste("   (None)")
  controlfile[60] <- paste("   (None)")
  controlfile[61] <- paste("-- 8. Off-season conditions (OFF) file")
  controlfile[62] <- paste("   (None)")
  controlfile[63] <- paste("   (None)")

  writeLines(controlfile,paste("Input",v,".PRO"))
  file.copy(paste("Input",v,".PRO"),general[52,2])

  }

    precin <- general[45,2]
    precip <- general[46,2]
    etn <- general[50,2]
    etp <- general[51,2]
    soiltext <- general[38,2]
    soiltextv <- general[39,2]
    soiltextp1 <- general[40,2]
    soiltextp2 <- general[61,2]
    soiltextp3 <- general[62,2]
    soiltextp4 <- general[63,2]
    soiltextp5 <- general[64,2]
    ksatv <- general[41,2]
    ksatp1 <- general[42,2]
    ksatp2 <- general[65,2]
    ksatp3 <- general[66,2]
    ksatp4 <- general[67,2]
    ksatp5 <- general[68,2]
    temp <- general[47,2]
    tempmaxp <- general[48,2]
    tempminp <- general[49,2]
    nbh <- general[15,2]

 
    if(precin=="RASTER"){
      precipr <- raster(precip)
      preci_vec <- as.vector(precipr) 
    }
    else if(precin=="TXT"){
      preci_vec <- read.csv2(precip) 

    }
    else {
      precipr <- nc_open(precip)
      preci <- ncvar_get(precipr)
      preci_vec <- as.vector(preci)
      nc_close(precipr)                
    }
    if(etn=="RASTER"){
      etr <- raster(etp)
      et_vec <- as.vector(etr)
    }
    else if(etn=="TXT"){
      et_vec <- read.csv2(etp)                 
      
    }
    else {
      etr <- nc_open(etp)
      etn <- ncvar_get(etr)
      et_vec <- as.vector(etn)
      nc_close(etr)
    }
    if(temp=="RASTER"){
      tempmaxr <- raster(tempmaxp)
      tempmax_vec <- as.vector(tempmaxr)
      tempminr <- raster(tempminp)
      tempmin_vec <- as.vector(tempminr)
    }
    else if(temp=="TXT"){
      tempmax_vec <- read.csv2(tempmaxp)           
      tempmin_vec <- read.csv2(tempminp)
    }
    else {
      tempmaxr <- nc_open(tempmaxp)
      tempmax <- ncvar_get(tempmaxr)
      tempmax_vec <- as.vector(tempmax)
      nc_close(tempmaxr)
      tempminr <- nc_open(tempminp)
      tempmin <- ncvar_get(tempminr)
      tempmin_vec <- as.vector(tempmin)
      nc_close(tempminr)
    }
    if(soiltext=="RASTER"){                                      
      soiltextr1 <- raster(soiltextp1)  
      soiltext_vec1 <- as.vector(soiltextr1)
      
      soiltextr2 <- raster(soiltextp2)  
      soiltext_vec2 <- as.vector(soiltextr2)
      
      soiltextr3 <- raster(soiltextp3)  
      soiltext_vec3 <- as.vector(soiltextr3)
      
      soiltextr4 <- raster(soiltextp4)  
      soiltext_vec4 <- as.vector(soiltextr4)
      
      soiltextr5 <- raster(soiltextp5)  
      soiltext_vec5 <- as.vector(soiltextr5)
    }else {
      soiltext_vec <- soiltextv
    }

    if(is.na(ksatv)){
      ksatr1 <- raster(ksatp1)
      ksat_vec1 <- as.vector(ksatr1)
      
      ksatr2 <- raster(ksatp2)
      ksat_vec2 <- as.vector(ksatr2)
      
      ksatr3 <- raster(ksatp3)
      ksat_vec3 <- as.vector(ksatr3)
      
      ksatr4 <- raster(ksatp4)
      ksat_vec4 <- as.vector(ksatr4)
      
      ksatr5 <- raster(ksatp5)
      ksat_vec5 <- as.vector(ksatr5)
    }else {
      ksat_vec <- as.numeric(ksatv)
    }

    CN1 <- 1
    CRa1 <- 1
    CRb1 <- 1
    CN2 <- 1
    CRa2 <- 1
    CRb2 <- 1
    CN3 <- 1
    CRa3 <- 1
    CRb3 <- 1
    CN4 <- 1
    CRa4 <- 1
    CRb4 <- 1
    CN5 <- 1
    CRa5 <- 1
    CRb5 <- 1
    
    for (n in 1:length(ksat_vec1)){
      if(is.na(ksat_vec1[n])){CN1[n]=NA
      }else if(ksat_vec1[n]>864){CN1[n]=(46)
      }else if(ksat_vec1[n]>36 & ksat_vec1[n]<346){CN1[n]=(72)
      }else if (ksat_vec1[n]>347 & ksat_vec1[n]<864){CN1[n]= (61)
      }else {CN1[n]=(77)}
      if(is.na(ksat_vec1[n])){CRa1[n]=NA
      }else if(ksat_vec1[n]>200 & ksat_vec1[n]<2000){CRa1[n]=(-0.3112-((ksat_vec1[n])*(10^(-5))))
      }else if(ksat_vec1[n]>100 & ksat_vec1[n]<750){CRa1[n]=(-0.4986+(9*(ksat_vec1[n])*(10^(-5))))
      }else if (ksat_vec1[n]>5 & ksat_vec1[n]<150){CRa1[n]=(-0.5677-(4*(ksat_vec1[n])*(10^(-5))))
      }else {CRa1[n]=(-0.6366+(8*(ksat_vec1[n])*(10^(-4))))}
      if(is.na(ksat_vec1[n])){CRb1[n]=NA
      }else if(ksat_vec1[n]>200 & ksat_vec1[n]<2000){CRb1[n]=(-1.4936+(log10(ksat_vec1[n])*(0.2416)))
      }else if(ksat_vec1[n]>100 & ksat_vec1[n]<750){CRb1[n]=(-2.1320+(log10(ksat_vec1[n])*(0.4778)))
      }else if (ksat_vec1[n]>5 & ksat_vec1[n]<150){CRb1[n]=(-3.7189+(log10(ksat_vec1[n])*(0.5922)))
      }else {CRb1[n]=(-1.9165+(log10(ksat_vec1[n])*(0.7063)))}}

    for (n in 1:length(ksat_vec2)){
      if(is.na(ksat_vec2[n])){CN2[n]=NA
      }else if(ksat_vec2[n]>864){CN2[n]=(46)
      }else if(ksat_vec2[n]>36 & ksat_vec2[n]<346){CN2[n]=(72)
      }else if (ksat_vec2[n]>347 & ksat_vec2[n]<864){CN2[n]= (61)
      }else {CN2[n]=(77)}
      if(is.na(ksat_vec2[n])){CRa2[n]=NA
      }else if(ksat_vec2[n]>200 & ksat_vec2[n]<2000){CRa2[n]=(-0.3112-((ksat_vec2[n])*(10^(-5))))
      }else if(ksat_vec2[n]>100 & ksat_vec2[n]<750){CRa2[n]=(-0.4986+(9*(ksat_vec2[n])*(10^(-5))))
      }else if (ksat_vec2[n]>5 & ksat_vec2[n]<150){CRa2[n]=(-0.5677-(4*(ksat_vec2[n])*(10^(-5))))
      }else {CRa2[n]=(-0.6366+(8*(ksat_vec2[n])*(10^(-4))))}
      if(is.na(ksat_vec2[n])){CRb2[n]=NA
      }else if(ksat_vec2[n]>200 & ksat_vec2[n]<2000){CRb2[n]=(-1.4936+(log10(ksat_vec2[n])*(0.2416)))
      }else if(ksat_vec2[n]>100 & ksat_vec2[n]<750){CRb2[n]=(-2.1320+(log10(ksat_vec2[n])*(0.4778)))
      }else if (ksat_vec2[n]>5 & ksat_vec2[n]<150){CRb2[n]=(-3.7189+(log10(ksat_vec2[n])*(0.5922)))
      }else {CRb2[n]=(-1.9165+(log10(ksat_vec2[n])*(0.7063)))}}
    
    for (n in 1:length(ksat_vec3)){
      if(is.na(ksat_vec3[n])){CN3[n]=NA
      }else if(ksat_vec3[n]>864){CN3[n]=(46)
      }else if(ksat_vec3[n]>36 & ksat_vec3[n]<346){CN3[n]=(72)
      }else if (ksat_vec3[n]>347 & ksat_vec3[n]<864){CN3[n]= (61)
      }else {CN3[n]=(77)}
      if(is.na(ksat_vec3[n])){CRa3[n]=NA
      }else if(ksat_vec3[n]>200 & ksat_vec3[n]<2000){CRa3[n]=(-0.3112-((ksat_vec3[n])*(10^(-5))))
      }else if(ksat_vec3[n]>100 & ksat_vec3[n]<750){CRa3[n]=(-0.4986+(9*(ksat_vec3[n])*(10^(-5))))
      }else if (ksat_vec3[n]>5 & ksat_vec3[n]<150){CRa3[n]=(-0.5677-(4*(ksat_vec3[n])*(10^(-5))))
      }else {CRa3[n]=(-0.6366+(8*(ksat_vec3[n])*(10^(-4))))}
      if(is.na(ksat_vec3[n])){CRb3[n]=NA
      }else if(ksat_vec3[n]>200 & ksat_vec3[n]<2000){CRb3[n]=(-1.4936+(log10(ksat_vec3[n])*(0.2416)))
      }else if(ksat_vec3[n]>100 & ksat_vec3[n]<750){CRb3[n]=(-2.1320+(log10(ksat_vec3[n])*(0.4778)))
      }else if (ksat_vec3[n]>5 & ksat_vec3[n]<150){CRb3[n]=(-3.7189+(log10(ksat_vec3[n])*(0.5922)))
      }else {CRb3[n]=(-1.9165+(log10(ksat_vec3[n])*(0.7063)))}}
    
    for (n in 1:length(ksat_vec4)){
      if(is.na(ksat_vec4[n])){CN4[n]=NA
      }else if(ksat_vec4[n]>864){CN4[n]=(46)
      }else if(ksat_vec4[n]>36 & ksat_vec4[n]<346){CN4[n]=(72)
      }else if (ksat_vec4[n]>347 & ksat_vec4[n]<864){CN4[n]= (61)
      }else {CN4[n]=(77)}
      if(is.na(ksat_vec4[n])){CRa4[n]=NA
      }else if(ksat_vec4[n]>200 & ksat_vec4[n]<2000){CRa4[n]=(-0.3112-((ksat_vec4[n])*(10^(-5))))
      }else if(ksat_vec4[n]>100 & ksat_vec4[n]<750){CRa4[n]=(-0.4986+(9*(ksat_vec4[n])*(10^(-5))))
      }else if (ksat_vec4[n]>5 & ksat_vec4[n]<150){CRa4[n]=(-0.5677-(4*(ksat_vec4[n])*(10^(-5))))
      }else {CRa4[n]=(-0.6366+(8*(ksat_vec4[n])*(10^(-4))))}
      if(is.na(ksat_vec4[n])){CRb4[n]=NA
      }else if(ksat_vec4[n]>200 & ksat_vec4[n]<2000){CRb4[n]=(-1.4936+(log10(ksat_vec4[n])*(0.2416)))
      }else if(ksat_vec4[n]>100 & ksat_vec4[n]<750){CRb4[n]=(-2.1320+(log10(ksat_vec4[n])*(0.4778)))
      }else if (ksat_vec4[n]>5 & ksat_vec4[n]<150){CRb4[n]=(-3.7189+(log10(ksat_vec4[n])*(0.5922)))
      }else {CRb4[n]=(-1.9165+(log10(ksat_vec4[n])*(0.7063)))}}
    
    for (n in 1:length(ksat_vec5)){
      if(is.na(ksat_vec5[n])){CN5[n]=NA
      }else if(ksat_vec5[n]>864){CN5[n]=(46)
      }else if(ksat_vec5[n]>36 & ksat_vec5[n]<346){CN5[n]=(72)
      }else if (ksat_vec5[n]>347 & ksat_vec5[n]<864){CN5[n]= (61)
      }else {CN5[n]=(77)}
      if(is.na(ksat_vec5[n])){CRa5[n]=NA
      }else if(ksat_vec5[n]>200 & ksat_vec5[n]<2000){CRa5[n]=(-0.3112-((ksat_vec5[n])*(10^(-5))))
      }else if(ksat_vec5[n]>100 & ksat_vec5[n]<750){CRa5[n]=(-0.4986+(9*(ksat_vec5[n])*(10^(-5))))
      }else if (ksat_vec5[n]>5 & ksat_vec5[n]<150){CRa5[n]=(-0.5677-(4*(ksat_vec5[n])*(10^(-5))))
      }else {CRa5[n]=(-0.6366+(8*(ksat_vec5[n])*(10^(-4))))}
      if(is.na(ksat_vec5[n])){CRb5[n]=NA
      }else if(ksat_vec5[n]>200 & ksat_vec5[n]<2000){CRb5[n]=(-1.4936+(log10(ksat_vec5[n])*(0.2416)))
      }else if(ksat_vec5[n]>100 & ksat_vec5[n]<750){CRb5[n]=(-2.1320+(log10(ksat_vec5[n])*(0.4778)))
      }else if (ksat_vec5[n]>5 & ksat_vec5[n]<150){CRb5[n]=(-3.7189+(log10(ksat_vec5[n])*(0.5922)))
      }else {CRb5[n]=(-1.9165+(log10(ksat_vec5[n])*(0.7063)))}}
    
    fd1 <- general[16,2]
    fd2 <- general[17,2]
    fd3 <- general[18,2]
    fd4 <- general[19,2]
    fd5 <- general[20,2]
    stp1 <- general[21,2]
    stp2 <- general[22,2]
    stp3 <- general[23,2]
    stp4 <- general[24,2]
    stp5 <- general[25,2]
    fcp1 <- general[26,2]
    fcp2 <- general[27,2]
    fcp3 <- general[28,2]
    fcp4 <- general[29,2]
    fcp5 <- general[30,2]
    wpp1 <- general[31,2]
    wpp2 <- general[32,2]
    wpp3 <- general[33,2]
    wpp4 <- general[34,2]
    wpp5 <- general[35,2]

   cc1 <- raster(stp1)
   cc <- raster(xmn=xmin(cc1),xmx=xmax(cc1),ymn=ymin(cc1),ymx=ymax(cc1),ncol=ncol(cc1),nrow=nrow(cc1),crs=crs(cc1))
   writeRaster(cc,"cc.tif")

  
 if (nbh==1) {
   stp1r <- raster(stp1)
   stp1_vec <- as.vector(stp1r)
   fcp1r <- raster(fcp1)
   fcp1_vec <- as.vector(fcp1r)
   wpp1r <- raster(wpp1)
   wpp1_vec <- as.vector(wpp1r)
   soildata <- data.frame(stp1_vec,fcp1_vec,wpp1_vec)
}else if (nbh==2){
   stp1r <- raster(stp1)
   stp1_vec <- as.vector(stp1r)
   fcp1r <- raster(fcp1)
   fcp1_vec <- as.vector(fcp1r)
   wpp1r <- raster(wpp1)
   wpp1_vec <- as.vector(wpp1r)
   stp2r <- raster(stp2)
   stp2_vec <- as.vector(stp2r)
   fcp2r <- raster(fcp2)
   fcp2_vec <- as.vector(fcp2r)
   wpp2r <- raster(wpp2)
   wpp2_vec <- as.vector(wpp2r)
   soildata <- data.frame(stp1_vec,fcp1_vec,wpp1_vec,stp2_vec,fcp2_vec,wpp2_vec)
 }else if (nbh==3){
      stp1r <- raster(stp1)
      stp1_vec <- as.vector(stp1r)
      fcp1r <- raster(fcp1)
      fcp1_vec <- as.vector(fcp1r)
      wpp1r <- raster(wpp1)
      wpp1_vec <- as.vector(wpp1r)
      stp2r <- raster(stp2)
      stp2_vec <- as.vector(stp2r)
      fcp2r <- raster(fcp2)
      fcp2_vec <- as.vector(fcp2r)
      wpp2r <- raster(wpp2)
      wpp2_vec <- as.vector(wpp2r)
      stp3r <- raster(stp3)
      stp3_vec <- as.vector(stp3r)
      fcp3r <- raster(fcp3)
      fcp3_vec <- as.vector(fcp3r)
      wpp3r <- raster(wpp3)
      wpp3_vec <- as.vector(wpp3r)
      soildata <- data.frame(stp1_vec,fcp1_vec,wpp1_vec,stp2_vec,fcp2_vec,wpp2_vec,stp3_vec,fcp3_vec,wpp3_vec)
    }else if (nbh==4){
      stp1r <- raster(stp1)
      stp1_vec <- as.vector(stp1r)
      fcp1r <- raster(fcp1)
      fcp1_vec <- as.vector(fcp1r)
      wpp1r <- raster(wpp1)
      wpp1_vec <- as.vector(wpp1r)
      stp2r <- raster(stp2)
      stp2_vec <- as.vector(stp2r)
      fcp2r <- raster(fcp2)
      fcp2_vec <- as.vector(fcp2r)
      wpp2r <- raster(wpp2)
      wpp2_vec <- as.vector(wpp2r)
      stp3r <- raster(stp3)
      stp3_vec <- as.vector(stp3r)
      fcp3r <- raster(fcp3)
      fcp3_vec <- as.vector(fcp3r)
      wpp3r <- raster(wpp3)
      wpp3_vec <- as.vector(wpp3r)
      stp4r <- raster(stp4)
      stp4_vec <- as.vector(stp4r)
      fcp4r <- raster(fcp4)
      fcp4_vec <- as.vector(fcp4r)
      wpp4r <- raster(wpp4)
      wpp4_vec <- as.vector(wpp4r)
      soildata <- data.frame(stp1_vec,fcp1_vec,wpp1_vec,stp2_vec,fcp2_vec,wpp2_vec,stp3_vec,fcp3_vec,wpp3_vec,stp4_vec,fcp4_vec,wpp4_vec)
    }else {
      stp1r <- raster(stp1)
      stp1_vec <- as.vector(stp1r)
      fcp1r <- raster(fcp1)
      fcp1_vec <- as.vector(fcp1r)
      wpp1r <- raster(wpp1)
      wpp1_vec <- as.vector(wpp1r)
      stp2r <- raster(stp2)
      stp2_vec <- as.vector(stp2r)
      fcp2r <- raster(fcp2)
      fcp2_vec <- as.vector(fcp2r)
      wpp2r <- raster(wpp2)
      wpp2_vec <- as.vector(wpp2r)
      stp3r <- raster(stp3)
      stp3_vec <- as.vector(stp3r)
      fcp3r <- raster(fcp3)
      fcp3_vec <- as.vector(fcp3r)
      wpp3r <- raster(wpp3)
      wpp3_vec <- as.vector(wpp3r)
      stp4r <- raster(stp4)
      stp4_vec <- as.vector(stp4r)
      fcp4r <- raster(fcp4)
      fcp4_vec <- as.vector(fcp4r)
      wpp4r <- raster(wpp4)
      wpp4_vec <- as.vector(wpp4r)
      stp5r <- raster(stp5)
      stp5_vec <- as.vector(stp5r)
      fcp5r <- raster(fcp5)
      fcp5_vec <- as.vector(fcp5r)
      wpp5r <- raster(wpp5)
      wpp5_vec <- as.vector(wpp5r)
     
    }

     soildata1 <- data.frame(soiltext_vec1,soiltext_vec2,soiltext_vec3,soiltext_vec4,soiltext_vec5,ksat_vec1,ksat_vec2,ksat_vec3,ksat_vec4,ksat_vec5,CN1,CN2,CN3,CN4,CN5,CRa1,CRa2,CRa3,CRa4,CRa5,CRb1,CRb2,CRb3,CRb4,CRb5,stp1_vec,fcp1_vec,wpp1_vec,stp2_vec,fcp2_vec,wpp2_vec,stp3_vec,fcp3_vec,wpp3_vec,stp4_vec,fcp4_vec,wpp4_vec,stp5_vec,fcp5_vec,wpp5_vec)
     write.csv2(soildata1,"Soil_data.csv")
     climate1 <- data.frame(preci_vec)
     climate2 <- data.frame(et_vec)
     climate3 <- data.frame(tempmin_vec)
     climate4 <- data.frame(tempmax_vec)
     write.csv2(climate1,"Climate_Precipitation.csv")
     write.csv2(climate2,"Climate_ET.csv")
     write.csv2(climate3,"Climate_Tempmin.csv")
     write.csv2(climate4,"Climate_Tempmax.csv")

    }



