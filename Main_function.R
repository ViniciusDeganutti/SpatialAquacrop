#CLIMATE FUNCTION
climate_fun <- function(p,precic,tempminc,tempmac,etc,prec,temp,et){
  for (r in 1:nrow(precic)){
    a <- p+1
    s=8+r
    prec[s] <- paste("      ",precic[r,a])
    temp[s] <- paste("    ",tempminc[r,a],"      ",tempmac[r,a])
    et[s] <- paste("    ",etc[r,a])
  }
  writeLines(prec,"precipitation.PLU")
  writeLines(temp, "temp.Tnx")
  writeLines(et,"etref.ETO")
}

#OUTPUT FUNCTION
Output_AQR <- function (cc_inf,nbcrops,infiltr1,infiltr2,infiltr3,infiltr4,infiltr5,infiltr6,
                        RO1,RO2,RO3,RO4,RO5,RO6,
                        ETP1,ETP2,ETP3,ETP4,ETP5,ETP6,
                        BioM1,BioM2, BioM3,BioM4, BioM5,BioM6,
                        Yld1, Yld2,Yld3, Yld4, Yld5, Yld6,
                        Dr1, Dr2, Dr3, Dr4, Dr5, Dr6,
                        NetIrrig1, NetIrrig2, NetIrrig3, NetIrrig4, NetIrrig5, NetIrrig6){
  if (nbcrops==1){
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Infiltration_crop1.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
  }
  else if (nbcrops==2){
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Inf1.tif")
    cc2 <- setValues(cc_inf,infiltr2)
    writeRaster(cc2,"Inf2.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Runoff2 <- setValues(cc_inf,RO2)
    writeRaster(Runoff2,"Runoff_crop2.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Evtp2 <- setValues(cc_inf,ETP2)
    writeRaster(Evtp2,"ETP_crop2.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Biomass2 <- setValues(cc_inf,BioM2)
    writeRaster(Biomass2,"Biomass_crop2.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Yield2 <- setValues(cc_inf,Yld2)
    writeRaster(Yield2,"Yield_crop2.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Draing2 <- setValues(cc_inf,Dr2)
    writeRaster(Draing2,"Drainage_crop2.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
    Irrig2 <- setValues(cc_inf, NetIrrig2)
    writeRaster(Irrig2 ,"Irrigation_crop2.tif")
  }
  else if (nbcrops==3){
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Inf1.tif")
    cc2 <- setValues(cc_inf,infiltr2)
    writeRaster(cc2,"Inf2.tif")
    cc3 <- setValues(cc_inf,infiltr3)
    writeRaster(cc3,"Inf3.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Runoff2 <- setValues(cc_inf,RO2)
    writeRaster(Runoff2,"Runoff_crop2.tif")
    Runoff3 <- setValues(cc_inf,RO3)
    writeRaster(Runoff3,"Runoff_crop3.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Evtp2 <- setValues(cc_inf,ETP2)
    writeRaster(Evtp2,"ETP_crop2.tif")
    Evtp3 <- setValues(cc_inf,ETP3)
    writeRaster(Evtp3,"ETP_crop3.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Biomass2 <- setValues(cc_inf,BioM2)
    writeRaster(Biomass2,"Biomass_crop2.tif")
    Biomass3 <- setValues(cc_inf,BioM3)
    writeRaster(Biomass3,"Biomass_crop3.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Yield2 <- setValues(cc_inf,Yld2)
    writeRaster(Yield2,"Yield_crop2.tif")
    Yield3 <- setValues(cc_inf,Yld3)
    writeRaster(Yield3,"Yield_crop3.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Draing2 <- setValues(cc_inf,Dr2)
    writeRaster(Draing2,"Drainage_crop2.tif")
    Draing3 <- setValues(cc_inf,Dr3)
    writeRaster(Draing3,"Drainage_crop3.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
    Irrig2 <- setValues(cc_inf, NetIrrig2)
    writeRaster(Irrig2 ,"Irrigation_crop2.tif")
    Irrig3 <- setValues(cc_inf, NetIrrig3)
    writeRaster(Irrig3 ,"Irrigation_crop3.tif")
  }
  else if (nbcrops==4){
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Inf1.tif")
    cc2 <- setValues(cc_inf,infiltr2)
    writeRaster(cc2,"Inf2.tif")
    cc3 <- setValues(cc_inf,infiltr3)
    writeRaster(cc3,"Inf3.tif")
    cc4 <- setValues(cc_inf,infiltr4)
    writeRaster(cc4,"Inf4.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Runoff2 <- setValues(cc_inf,RO2)
    writeRaster(Runoff2,"Runoff_crop2.tif")
    Runoff3 <- setValues(cc_inf,RO3)
    writeRaster(Runoff3,"Runoff_crop3.tif")
    Runoff4 <- setValues(cc_inf,RO4)
    writeRaster(Runoff4,"Runoff_crop4.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Evtp2 <- setValues(cc_inf,ETP2)
    writeRaster(Evtp2,"ETP_crop2.tif")
    Evtp3 <- setValues(cc_inf,ETP3)
    writeRaster(Evtp3,"ETP_crop3.tif")
    Evtp4 <- setValues(cc_inf,ETP4)
    writeRaster(Evtp4,"ETP_crop4.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Biomass2 <- setValues(cc_inf,BioM2)
    writeRaster(Biomass2,"Biomass_crop2.tif")
    Biomass3 <- setValues(cc_inf,BioM3)
    writeRaster(Biomass3,"Biomass_crop3.tif")
    Biomass4 <- setValues(cc_inf,BioM4)
    writeRaster(Biomass4,"Biomass_crop4.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Yield2 <- setValues(cc_inf,Yld2)
    writeRaster(Yield2,"Yield_crop2.tif")
    Yield3 <- setValues(cc_inf,Yld3)
    writeRaster(Yield3,"Yield_crop3.tif")
    Yield4 <- setValues(cc_inf,Yld4)
    writeRaster(Yield4,"Yield_crop4.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Draing2 <- setValues(cc_inf,Dr2)
    writeRaster(Draing2,"Drainage_crop2.tif")
    Draing3 <- setValues(cc_inf,Dr3)
    writeRaster(Draing3,"Drainage_crop3.tif")
    Draing4 <- setValues(cc_inf,Dr4)
    writeRaster(Draing4,"Drainage_crop4.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
    Irrig2 <- setValues(cc_inf, NetIrrig2)
    writeRaster(Irrig2 ,"Irrigation_crop2.tif")
    Irrig3 <- setValues(cc_inf, NetIrrig3)
    writeRaster(Irrig3 ,"Irrigation_crop3.tif")
    Irrig4 <- setValues(cc_inf, NetIrrig4)
    writeRaster(Irrig4 ,"Irrigation_crop4.tif")
  }
  else if (nbcrops==5){
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Inf1.tif")
    cc2 <- setValues(cc_inf,infiltr2)
    writeRaster(cc2,"Inf2.tif")
    cc3 <- setValues(cc_inf,infiltr3)
    writeRaster(cc3,"Inf3.tif")
    cc4 <- setValues(cc_inf,infiltr4)
    writeRaster(cc4,"Inf4.tif")
    cc5 <- setValues(cc_inf,infiltr5)
    writeRaster(cc5,"Inf5.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Runoff2 <- setValues(cc_inf,RO2)
    writeRaster(Runoff2,"Runoff_crop2.tif")
    Runoff3 <- setValues(cc_inf,RO3)
    writeRaster(Runoff3,"Runoff_crop3.tif")
    Runoff4 <- setValues(cc_inf,RO4)
    writeRaster(Runoff4,"Runoff_crop4.tif")
    Runoff5 <- setValues(cc_inf,RO5)
    writeRaster(Runoff5,"Runoff_crop5.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Evtp2 <- setValues(cc_inf,ETP2)
    writeRaster(Evtp2,"ETP_crop2.tif")
    Evtp3 <- setValues(cc_inf,ETP3)
    writeRaster(Evtp3,"ETP_crop3.tif")
    Evtp4 <- setValues(cc_inf,ETP4)
    writeRaster(Evtp4,"ETP_crop4.tif")
    Evtp5 <- setValues(cc_inf,ETP5)
    writeRaster(Evtp5,"ETP_crop5.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Biomass2 <- setValues(cc_inf,BioM2)
    writeRaster(Biomass2,"Biomass_crop2.tif")
    Biomass3 <- setValues(cc_inf,BioM3)
    writeRaster(Biomass3,"Biomass_crop3.tif")
    Biomass4 <- setValues(cc_inf,BioM4)
    writeRaster(Biomass4,"Biomass_crop4.tif")
    Biomass5 <- setValues(cc_inf,BioM5)
    writeRaster(Biomass5,"Biomass_crop5.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Yield2 <- setValues(cc_inf,Yld2)
    writeRaster(Yield2,"Yield_crop2.tif")
    Yield3 <- setValues(cc_inf,Yld3)
    writeRaster(Yield3,"Yield_crop3.tif")
    Yield4 <- setValues(cc_inf,Yld4)
    writeRaster(Yield4,"Yield_crop4.tif")
    Yield5 <- setValues(cc_inf,Yld5)
    writeRaster(Yield5,"Yield_crop5.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Draing2 <- setValues(cc_inf,Dr2)
    writeRaster(Draing2,"Drainage_crop2.tif")
    Draing3 <- setValues(cc_inf,Dr3)
    writeRaster(Draing3,"Drainage_crop3.tif")
    Draing4 <- setValues(cc_inf,Dr4)
    writeRaster(Draing4,"Drainage_crop4.tif")
    Draing5 <- setValues(cc_inf,Dr5)
    writeRaster(Draing5,"Drainage_crop5.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
    Irrig2 <- setValues(cc_inf, NetIrrig2)
    writeRaster(Irrig2 ,"Irrigation_crop2.tif")
    Irrig3 <- setValues(cc_inf, NetIrrig3)
    writeRaster(Irrig3 ,"Irrigation_crop3.tif")
    Irrig4 <- setValues(cc_inf, NetIrrig4)
    writeRaster(Irrig4 ,"Irrigation_crop4.tif")
    Irrig5 <- setValues(cc_inf, NetIrrig5)
    writeRaster(Irrig5 ,"Irrigation_crop5.tif")
  }
  else {
    cc1 <- setValues(cc_inf,infiltr1)
    writeRaster(cc1,"Inf1.tif")
    cc2 <- setValues(cc_inf,infiltr2)
    writeRaster(cc2,"Inf2.tif")
    cc3 <- setValues(cc_inf,infiltr3)
    writeRaster(cc3,"Inf3.tif")
    cc4 <- setValues(cc_inf,infiltr4)
    writeRaster(cc4,"Inf4.tif")
    cc5 <- setValues(cc_inf,infiltr5)
    writeRaster(cc5,"Inf5.tif")
    cc6 <- setValues(cc_inf,infiltr5)
    writeRaster(cc6,"Inf6.tif")
    Runoff1 <- setValues(cc_inf,RO1)
    writeRaster(Runoff1,"Runoff_crop1.tif")
    Runoff2 <- setValues(cc_inf,RO2)
    writeRaster(Runoff2,"Runoff_crop2.tif")
    Runoff3 <- setValues(cc_inf,RO3)
    writeRaster(Runoff3,"Runoff_crop3.tif")
    Runoff4 <- setValues(cc_inf,RO4)
    writeRaster(Runoff4,"Runoff_crop4.tif")
    Runoff5 <- setValues(cc_inf,RO5)
    writeRaster(Runoff5,"Runoff_crop5.tif")
    Runoff6 <- setValues(cc_inf,RO6)
    writeRaster(Runoff6,"Runoff_crop6.tif")
    Evtp1 <- setValues(cc_inf,ETP1)
    writeRaster(Evtp1,"ETP_crop1.tif")
    Evtp2 <- setValues(cc_inf,ETP2)
    writeRaster(Evtp2,"ETP_crop2.tif")
    Evtp3 <- setValues(cc_inf,ETP3)
    writeRaster(Evtp3,"ETP_crop3.tif")
    Evtp4 <- setValues(cc_inf,ETP4)
    writeRaster(Evtp4,"ETP_crop4.tif")
    Evtp5 <- setValues(cc_inf,ETP5)
    writeRaster(Evtp5,"ETP_crop5.tif")
    Evtp6 <- setValues(cc_inf,ETP6)
    writeRaster(Evtp6,"ETP_crop6.tif")
    Biomass1 <- setValues(cc_inf,BioM1)
    writeRaster(Biomass1,"Biomass_crop1.tif")
    Biomass2 <- setValues(cc_inf,BioM2)
    writeRaster(Biomass2,"Biomass_crop2.tif")
    Biomass3 <- setValues(cc_inf,BioM3)
    writeRaster(Biomass3,"Biomass_crop3.tif")
    Biomass4 <- setValues(cc_inf,BioM4)
    writeRaster(Biomass4,"Biomass_crop4.tif")
    Biomass5 <- setValues(cc_inf,BioM5)
    writeRaster(Biomass5,"Biomass_crop5.tif")
    Biomass6 <- setValues(cc_inf,BioM6)
    writeRaster(Biomass6,"Biomass_crop6.tif")
    Yield1 <- setValues(cc_inf,Yld1)
    writeRaster(Yield1,"Yield_crop1.tif")
    Yield2 <- setValues(cc_inf,Yld2)
    writeRaster(Yield2,"Yield_crop2.tif")
    Yield3 <- setValues(cc_inf,Yld3)
    writeRaster(Yield3,"Yield_crop3.tif")
    Yield4 <- setValues(cc_inf,Yld4)
    writeRaster(Yield4,"Yield_crop4.tif")
    Yield5 <- setValues(cc_inf,Yld5)
    writeRaster(Yield5,"Yield_crop5.tif")
    Yield6 <- setValues(cc_inf,Yld6)
    writeRaster(Yield6,"Yield_crop6.tif")
    Draing1 <- setValues(cc_inf,Dr1)
    writeRaster(Draing1,"Drainage_crop1.tif")
    Draing2 <- setValues(cc_inf,Dr2)
    writeRaster(Draing2,"Drainage_crop2.tif")
    Draing3 <- setValues(cc_inf,Dr3)
    writeRaster(Draing3,"Drainage_crop3.tif")
    Draing4 <- setValues(cc_inf,Dr4)
    writeRaster(Draing4,"Drainage_crop4.tif")
    Draing5 <- setValues(cc_inf,Dr5)
    writeRaster(Draing5,"Drainage_crop5.tif")
    Draing6 <- setValues(cc_inf,Dr6)
    writeRaster(Draing6,"Drainage_crop6.tif")
    Irrig1 <- setValues(cc_inf, NetIrrig1)
    writeRaster(Irrig1 ,"Irrigation_crop1.tif")
    Irrig2 <- setValues(cc_inf, NetIrrig2)
    writeRaster(Irrig2 ,"Irrigation_crop2.tif")
    Irrig3 <- setValues(cc_inf, NetIrrig3)
    writeRaster(Irrig3 ,"Irrigation_crop3.tif")
    Irrig4 <- setValues(cc_inf, NetIrrig4)
    writeRaster(Irrig4 ,"Irrigation_crop4.tif")
    Irrig5 <- setValues(cc_inf, NetIrrig5)
    writeRaster(Irrig5 ,"Irrigation_crop5.tif")
    Irrig6 <- setValues(cc_inf, NetIrrig6)
    writeRaster(Irrig6 ,"Irrigation_crop6.tif")
  }
}
#WF CALCULATION
WF <- function (ETPpath, Yieldpath,ID){
  ET <- raster(ETPpath)
  Yield <- raster(Yieldpath)
  WFrast <- ET
  rowR <- 0
  colR <- 0
  for (z in 1:nrow(ET)){
    rowR = rowR + 1
    column = 0
    for (x in 1:ncol(ET)){
      colR = colR + 1
      count= 1
      if (is.na(ET[rowR, colR]== TRUE)){print("NA")
      }else {WFrast[rowR, colR] <- ((10*ET[rowR, colR])/(Yield[rowR,colR]))}
    }
  }
  writeRaster(WFrast, paste("WaterFootprint",ID,".tif"))
}

#MAIN FUNCTION
Spatial_AQC<- function(AQCroot){
  setwd(AQCroot)
  library(sp)
  library(raster)

  datasoil <- read.csv2("Soil_data.csv")
  precic <- read.csv2("Climate_Precipitation.csv")
  etc <- read.csv2("Climate_ET.csv")
  tempmac <- read.csv2("Climate_Tempmax.csv")
  tempminc <- read.csv2("Climate_Tempmin.csv")
  count_vec <- datasoil[2]
  countt <- nrow(count_vec)
  general1 <- read.csv2("Data_Fill.csv", header=FALSE)
  general <- general1[2]
  nbcrops <- general[54,1]
  sth1 <- general[16,1]
  sth2 <- general[17,1]
  sth3 <- general[18,1]
  sth4 <- general[19,1]
  sth5 <- general[20,1]
  sth <- c(sth1, sth2,sth3,sth4,sth5)
  pen <- general[36,1]
  grav <- general[37,1]
  yearrecord <- general[43,1]
  records <- general[44,1]
  yearrecord <- general[43,1]
  exe <- general[56,1]
  outrun <- general[53,1]
  cc_inf <- raster("cc.tif")
  nbhori <- general[15,1]
 
  soil <- file.create("soil.SOl")
  soil[1] <- paste ("soilprofile")
  soil[2] <- paste("        6.0                 : AquaCrop Version (March 2017)")
  soil[4] <- paste ("        9                   : Readily evaporable water from top layer (mm)")
  soil[5] <- paste ("       ",nbhori,"                   : number of soil horizons")
  soil[6] <- paste("       -9                   : variable no longer applicable")
  soil[7] <- paste ("  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description")
  soil[8] <- paste ("  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------")
  cli <- file.create("cli.CLI")
  cli[1] <- paste(" ")
  cli[2] <- paste(" 6.1   : AquaCrop Version (May 2018)")
  cli[3] <- paste("temp.Tnx")
  cli[4] <- paste("etref.ETO")
  cli[5] <- paste("precipitation.PLU")
  cli[6] <- paste("MaunaLoa.CO2")
  writeLines(cli, "climate.CLI") 
  prec <- file.create("precipitation.PLU")
  prec[1] <- paste ("   input  : monthly data from CARPATCLIM")
  prec[2] <- paste("    1  : Daily records (1=daily, 2=10-daily and 3=monthly data)")
  prec[3] <- paste("    1     : First day of record (1, 11 or 21 for 10-day or 1 for months)")
  prec[4] <- paste("    1     : First month of record")
  prec[5] <- paste("  ",yearrecord,"  : First year of record (1901 if not linked to a specific year)")
  prec[6] <- paste(" ")
  prec[7] <- paste("  Total Rain (mm)")
  prec[8] <- paste("=======================")
  temp <- file.create("temp.tnx")
  temp [1:6] <- prec[1:6]
  temp[7] <- paste("  Tmin (C)   TMax (C)")
  temp[8] <- paste("=======================")
  et <- file.create("etref.ETO")
  et[1:6] <- prec[1:6]
  et[7] <- paste("  Average ETo (mm/day)")
  et[8] <- paste("=======================")

  title_oup <- c("Day", "Month"
                 ,"Year","DAP","Stage","WC","Rain","Irri","Surf","Infilt",
                 "RO","Drain","CR","Zgwt","Ex","E", "E/Ex",
                 "Trx", "Tr",  "Tr/Trx",    "ETx",      "ET",  "ET/ETx",
                 "GD",       "Z",     "StExp",  "StSto", "StSen",
                 "StSalt", "StWeed",   "CC",      "CCw",
                 "StTr",  "KcTr",     "Trx",      "Tr", "TrW",
                 "Tr/Trx",   "WP",    "Biomass",     "HI",
                 "YieldPart",  "Brelative",    "WPet",  "WC",
                 "Wrtot",      "Z",       "Wr",    "Wr(SAT)",
                 "Wr(FC)",   "Wr(exp)",   "Wr(sto)", "Wr(sen)",
                 "Wr(PWP)",    "SaltIn" ,   "SaltOut" ,
                 "SaltUp",   "Salt", "SaltZ",     "Z" , "ECe",
                 "ECsw",   "StSalt",  "Zgwt",    "ECgw",
                 "WC01",       "WC2",       "WC3", "WC4",
                 "WC5",       "WC6",       "WC7",
                 "WC8",       "WC9",       "WC10" , "WC11",
                 "WC12" ,     "ECe01",
                 "ECe2",      "ECe3",      "ECe4", "ECe5",
                 "ECe6",      "ECe7",      "ECe8",
                 "ECe9",      "ECe10",      "ECe11"  , "ECe12"
                 ,   "Rain"   ,   "ETo"   ,   "Tmin"   ,
                 "Tavg"  ,    "Tmax"    ,  "CO2")


  title_seasonal <- c("RunNr",     "Day1",   "Month1", "Year1",
                      "Rain",      "ETo",       "GD", "CO2",
                      "Irri",   "Infilt",   "Runoff", "Drain",
                      "Upflow",       "E",      "E/Ex", "Tr",
                      "TrW",   "Tr/Trx",    "SaltIn", "SaltOut",
                      "SaltUp",  "SaltProf",     "Cycle", "SaltStr",
                      "FertStr",  "WeedStr",  "TempStr", "ExpStr",
                      "StoStr",  "BioMass",  "Brelative", "HI",
                      "Yield",     "WPet",     "DayN",
                      "MonthN",    "YearN", "name")
  infiltr1 <- rep (NA, times=countt)
  infiltr2 <- rep (NA, times=countt)
  infiltr3 <- rep (NA, times=countt)
  infiltr4 <- rep (NA, times=countt)
  infiltr5 <- rep (NA, times=countt)
  infiltr6 <- rep (NA, times=countt)
  RO1 <- rep (NA, times=countt)
  RO2 <- rep (NA, times=countt)
  RO3 <- rep (NA, times=countt)
  RO4 <- rep (NA, times=countt)
  RO5 <- rep (NA, times=countt)
  RO6 <- rep (NA, times=countt)
  ETP1 <- rep (NA, times=countt)
  ETP2 <- rep (NA, times=countt)
  ETP3 <- rep (NA, times=countt)
  ETP4 <- rep (NA, times=countt)
  ETP5 <- rep (NA, times=countt)
  ETP6 <- rep (NA, times=countt)
  BioM1 <- rep (NA, times=countt)
  BioM2 <- rep (NA, times=countt)
  BioM3 <- rep (NA, times=countt)
  BioM4 <- rep (NA, times=countt)
  BioM5 <- rep (NA, times=countt)
  BioM6 <- rep (NA, times=countt)
  Yld1 <- rep (NA, times=countt)
  Yld2 <- rep (NA, times=countt)
  Yld3 <- rep (NA, times=countt)
  Yld4 <- rep (NA, times=countt)
  Yld5 <- rep (NA, times=countt)
  Yld6 <- rep (NA, times=countt)
  Dr1 <- rep (NA, times=countt)
  Dr2 <- rep (NA, times=countt)
  Dr3 <- rep (NA, times=countt)
  Dr4 <- rep (NA, times=countt)
  Dr5 <- rep (NA, times=countt)
  Dr6 <- rep (NA, times=countt)
  NetIrrig1 <- rep (NA, times=countt)
  NetIrrig2 <- rep (NA, times=countt)
  NetIrrig3 <- rep (NA, times=countt)
  NetIrrig4 <- rep (NA, times=countt)
  NetIrrig5 <- rep (NA, times=countt)
  NetIrrig6 <- rep (NA, times=countt)

  
  nbhori_vec <- rep(general[15,1],times=countt)
  

  for (p in 1:nrow(count_vec)){
    setwd(AQCroot)
    count <- p
    print(count)
    nbhori <- nbhori_vec[p]
    if (is.na(datasoil$stp1_vec[p])){print("NA")
    }else if (nbhori==1){
      soil[3] <- paste ("       ",datasoil$CN1[p],"                  : CN(Curve Number)")
      soil[9] <- paste("     ",sth1,"      ",datasoil$stp1_vec[p],"    ",datasoil$fcp1_vec[p],"    " ,datasoil$wpp1_vec[p],"      "  ,datasoil$ksat_vec1[p], "          ",pen,"           ",grav,"       ",datasoil$CRa1[p],"    ",datasoil$CRb1[p],"               ",datasoil$soiltext_vec1[p])
      writeLines(soil, "soil.SOL")

      climate_fun(p,precic,tempminc,tempmac,etc,prec,temp,et)

      system(exe,invisible = FALSE, wait= TRUE)

      setwd(outrun)

      for (v in 1:nbcrops){
        outp <- read.table(paste("Input",v,"PROday.OUT"),header=FALSE, skip=4, col.names= title_oup, fill=TRUE)
        outp_season <-read.table(paste("Input",v,"PROseason.OUT"),header = FALSE, skip=4, col.names=title_seasonal, fill=TRUE)
        if (v==1) {
          infiltr1[p] <- outp_season$Infilt
          RO1[p] <- outp_season$Runoff
          ETP1[p] <- sum(outp$ET, na.rm=FALSE)
          BioM1[p] <- outp_season$BioMass
          Yld1[p] <- outp_season$Yield
          Dr1[p] <- outp_season$Drain
          NetIrrig1[p] <- outp_season$Irri
        }
        else if (v==2){
          infiltr2[p] <- outp_season$Infilt
          RO2[p] <- outp_season$Runoff
          ETP2[p] <- sum(outp$ET, na.rm=FALSE)
          BioM2[p] <- outp_season$BioMass
          Yld2[p] <- outp_season$Yield
          Dr2[p] <- outp_season$Drain
          NetIrrig2[p] <- outp_season$Irri
        }
        else if (v==3){
          infiltr3[p] <- outp_season$Infilt
          RO3[p] <- outp_season$Runoff
          ETP3[p] <- sum(outp$ET, na.rm=FALSE)
          BioM3[p] <- outp_season$BioMass
          Yld3[p] <- outp_season$Yield
          Dr3[p] <- outp_season$Drain
          NetIrrig3[p] <- outp_season$Irri
        }
        else if (v==4){
          infiltr4[p] <- outp_season$Infilt
          RO4[p] <- outp_season$Runoff
          ETP4[p] <- sum(outp$ET, na.rm=FALSE)
          BioM4[p] <- outp_season$BioMass
          Yld4[p] <- outp_season$Yield
          Dr4[p] <- outp_season$Drain
          NetIrrig4[p] <- outp_season$Irri
        }
        else if (v==5){
          infiltr5[p] <- outp_season$Infilt
          RO5[p] <- outp_season$Runoff
          ETP5[p] <- sum(outp$ET, na.rm=FALSE)
          BioM5[p] <- outp_season$BioMass
          Yld5[p] <- outp_season$Yield
          Dr5[p] <- outp_season$Drain
          NetIrrig5[p] <- outp_season$Irri
        }
        else {
          infiltr6[p] <- outp_season$Infilt
          RO6[p] <- outp_season$Runoff
          ETP6[p] <- sum(outp$ET, na.rm=FALSE)
          BioM6[p] <- outp_season$BioMass
          Yld6[p] <- outp_season$Yield
          Dr6[p] <- outp_season$Drain
          NetIrrig6[p] <- outp_season$Irri
        }
     }

    }else if (nbhori==2) {
      
      soil[3] <- paste ("       ",datasoil$CN2[p],"                  : CN(Curve Number)") 
      soil[9] <- paste("     ",sth1,"      ",datasoil$stp1_vec[p],"    ",datasoil$fcp1_vec[p],"    " ,datasoil$wpp1_vec[p],"      "  ,datasoil$ksat_vec1[p], "          ",pen,"           ",grav,"       ",datasoil$CRa1[p],"    ",datasoil$CRb1[p],"               ",datasoil$soiltext_vec1[p])
      soil[10] <- paste("     ",sth2,"      ",datasoil$stp2_vec[p],"    ",datasoil$fcp2_vec[p],"    " ,datasoil$wpp2_vec[p],"      "  ,datasoil$ksat_vec2[p], "          ",pen,"           ",grav,"       ",datasoil$CRa2[p],"    ",datasoil$CRb2[p],"               ",datasoil$soiltext_vec2[p])
      writeLines(soil, "soil.SOL")

      climate_fun(p,precic,tempminc,tempmac,etc,prec,temp,et)

      system(exe,invisible = FALSE, wait= TRUE)

      setwd(outrun)

      for (v in 1:nbcrops){
        outp <- read.table(paste("Input",v,"PROday.OUT"),header=FALSE, skip=4, col.names= title_oup, fill=TRUE)
        outp_season <-read.table(paste("Input",v,"PROseason.OUT"),header = FALSE, skip=4, col.names=title_seasonal, fill=TRUE)
        if (v==1) {
          infiltr1[p] <- outp_season$Infilt
          RO1[p] <- outp_season$Runoff
          ETP1[p] <- sum(outp$ET, na.rm=FALSE)
          BioM1[p] <- outp_season$BioMass
          Yld1[p] <- outp_season$Yield
          Dr1[p] <- outp_season$Drain
          NetIrrig1[p] <- outp_season$Irri
        }
        else if (v==2){
          infiltr2[p] <- outp_season$Infilt
          RO2[p] <- outp_season$Runoff
          ETP2[p] <- sum(outp$ET, na.rm=FALSE)
          BioM2[p] <- outp_season$BioMass
          Yld2[p] <- outp_season$Yield
          Dr2[p] <- outp_season$Drain
          NetIrrig2[p] <- outp_season$Irri
        }
        else if (v==3){
          infiltr3[p] <- outp_season$Infilt
          RO3[p] <- outp_season$Runoff
          ETP3[p] <- sum(outp$ET, na.rm=FALSE)
          BioM3[p] <- outp_season$BioMass
          Yld3[p] <- outp_season$Yield
          Dr3[p] <- outp_season$Drain
          NetIrrig3[p] <- outp_season$Irri
        }
        else if (v==4){
          infiltr4[p] <- outp_season$Infilt
          RO4[p] <- outp_season$Runoff
          ETP4[p] <- sum(outp$ET, na.rm=FALSE)
          BioM4[p] <- outp_season$BioMass
          Yld4[p] <- outp_season$Yield
          Dr4[p] <- outp_season$Drain
          NetIrrig4[p] <- outp_season$Irri
        }
        else if (v==5){
          infiltr5[p] <- outp_season$Infilt
          RO5[p] <- outp_season$Runoff
          ETP5[p] <- sum(outp$ET, na.rm=FALSE)
          BioM5[p] <- outp_season$BioMass
          Yld5[p] <- outp_season$Yield
          Dr5[p] <- outp_season$Drain
          NetIrrig5[p] <- outp_season$Irri
        }
        else {
          infiltr6[p] <- outp_season$Infilt
          RO6[p] <- outp_season$Runoff
          ETP6[p] <- sum(outp$ET, na.rm=FALSE)
          BioM6[p] <- outp_season$BioMass
          Yld6[p] <- outp_season$Yield
          Dr6[p] <- outp_season$Drain
          NetIrrig6[p] <- outp_season$Irri
        }
      }
    }else if (nbhori==3){
      
      soil[3] <- paste ("       ",datasoil$CN3[p],"                  : CN(Curve Number)")
      soil[9] <-  paste("     ",sth1,"      ",datasoil$stp1_vec[p],"    ",datasoil$fcp1_vec[p],"    " ,datasoil$wpp1_vec[p],"      "  ,datasoil$ksat_vec1[p], "          ",pen,"           ",grav,"       ",datasoil$CRa1[p],"    ",datasoil$CRb1[p],"               ",datasoil$soiltext_vec1[p])
      soil[10] <- paste("     ",sth2,"      ",datasoil$stp2_vec[p],"    ",datasoil$fcp2_vec[p],"    " ,datasoil$wpp2_vec[p],"      "  ,datasoil$ksat_vec2[p], "          ",pen,"           ",grav,"       ",datasoil$CRa2[p],"    ",datasoil$CRb2[p],"               ",datasoil$soiltext_vec2[p])
      soil[11] <- paste("     ",sth3,"      ",datasoil$stp3_vec[p],"    ",datasoil$fcp3_vec[p],"    " ,datasoil$wpp3_vec[p],"      "  ,datasoil$ksat_vec3[p], "          ",pen,"           ",grav,"       ",datasoil$CRa3[p],"    ",datasoil$CRb3[p],"               ",datasoil$soiltext_vec3[p])
      writeLines(soil, "soil.SOL")

      climate_fun(p,precic,tempminc,tempmac,etc,prec,temp,et)

      system(exe,invisible = FALSE, wait= TRUE)

      setwd(outrun)

      for (v in 1:nbcrops){
        outp <- read.table(paste("Input",v,"PROday.OUT"),header=FALSE, skip=4, col.names= title_oup, fill=TRUE)
        outp_season <-read.table(paste("Input",v,"PROseason.OUT"),header = FALSE, skip=4, col.names=title_seasonal, fill=TRUE)
        if (v==1) {
          infiltr1[p] <- outp_season$Infilt
          RO1[p] <- outp_season$Runoff
          ETP1[p] <- sum(outp$ET, na.rm=FALSE)
          BioM1[p] <- outp_season$BioMass
          Yld1[p] <- outp_season$Yield
          Dr1[p] <- outp_season$Drain
          NetIrrig1[p] <- outp_season$Irri
        }
        else if (v==2){
          infiltr2[p] <- outp_season$Infilt
          RO2[p] <- outp_season$Runoff
          ETP2[p] <- sum(outp$ET, na.rm=FALSE)
          BioM2[p] <- outp_season$BioMass
          Yld2[p] <- outp_season$Yield
          Dr2[p] <- outp_season$Drain
          NetIrrig2[p] <- outp_season$Irri
        }
        else if (v==3){
          infiltr3[p] <- outp_season$Infilt
          RO3[p] <- outp_season$Runoff
          ETP3[p] <- sum(outp$ET, na.rm=FALSE)
          BioM3[p] <- outp_season$BioMass
          Yld3[p] <- outp_season$Yield
          Dr3[p] <- outp_season$Drain
          NetIrrig3[p] <- outp_season$Irri
        }
        else if (v==4){
          infiltr4[p] <- outp_season$Infilt
          RO4[p] <- outp_season$Runoff
          ETP4[p] <- sum(outp$ET, na.rm=FALSE)
          BioM4[p] <- outp_season$BioMass
          Yld4[p] <- outp_season$Yield
          Dr4[p] <- outp_season$Drain
          NetIrrig4[p] <- outp_season$Irri
        }
        else if (v==5){
          infiltr5[p] <- outp_season$Infilt
          RO5[p] <- outp_season$Runoff
          ETP5[p] <- sum(outp$ET, na.rm=FALSE)
          BioM5[p] <- outp_season$BioMass
          Yld5[p] <- outp_season$Yield
          Dr5[p] <- outp_season$Drain
          NetIrrig5[p] <- outp_season$Irri
        }
        else {
          infiltr6[p] <- outp_season$Infilt
          RO6[p] <- outp_season$Runoff
          ETP6[p] <- sum(outp$ET, na.rm=FALSE)
          BioM6[p] <- outp_season$BioMass
          Yld6[p] <- outp_season$Yield
          Dr6[p] <- outp_season$Drain
          NetIrrig6[p] <- outp_season$Irri
        }
      }
    }else if (nbhori==4){
      
      soil[3] <- paste ("       ",datasoil$CN4[p],"                  : CN(Curve Number)")
      soil[9] <-  paste("     ",sth1,"      ",datasoil$stp1_vec[p],"    ",datasoil$fcp1_vec[p],"    " ,datasoil$wpp1_vec[p],"      "  ,datasoil$ksat_vec1[p], "          ",pen,"           ",grav,"       ",datasoil$CRa1[p],"    ",datasoil$CRb1[p],"               ",datasoil$soiltext_vec1[p])
      soil[10] <- paste("     ",sth2,"      ",datasoil$stp2_vec[p],"    ",datasoil$fcp2_vec[p],"    " ,datasoil$wpp2_vec[p],"      "  ,datasoil$ksat_vec2[p], "          ",pen,"           ",grav,"       ",datasoil$CRa2[p],"    ",datasoil$CRb2[p],"               ",datasoil$soiltext_vec2[p])
      soil[11] <- paste("     ",sth3,"      ",datasoil$stp3_vec[p],"    ",datasoil$fcp3_vec[p],"    " ,datasoil$wpp3_vec[p],"      "  ,datasoil$ksat_vec3[p], "          ",pen,"           ",grav,"       ",datasoil$CRa3[p],"    ",datasoil$CRb3[p],"               ",datasoil$soiltext_vec3[p])
      soil[12] <- paste("     ",sth4,"      ",datasoil$stp4_vec[p],"    ",datasoil$fcp4_vec[p],"    " ,datasoil$wpp4_vec[p],"      "  ,datasoil$ksat_vec4[p], "          ",pen,"           ",grav,"       ",datasoil$CRa4[p],"    ",datasoil$CRb4[p],"               ",datasoil$soiltext_vec4[p])
      writeLines(soil, "soil.SOL")
      
      climate_fun(p,precic,tempminc,tempmac,etc,prec,temp,et)
      
      system(exe,invisible = FALSE, wait= TRUE)
      
      setwd(outrun)
      
      for (v in 1:nbcrops){
        outp <- read.table(paste("Input",v,"PROday.OUT"),header=FALSE, skip=4, col.names= title_oup, fill=TRUE)
        outp_season <-read.table(paste("Input",v,"PROseason.OUT"),header = FALSE, skip=4, col.names=title_seasonal, fill=TRUE)
        if (v==1) {
          infiltr1[p] <- outp_season$Infilt
          RO1[p] <- outp_season$Runoff
          ETP1[p] <- sum(outp$ET, na.rm=FALSE)
          BioM1[p] <- outp_season$BioMass
          Yld1[p] <- outp_season$Yield
          Dr1[p] <- outp_season$Drain
          NetIrrig1[p] <- outp_season$Irri
        }
        else if (v==2){
          infiltr2[p] <- outp_season$Infilt
          RO2[p] <- outp_season$Runoff
          ETP2[p] <- sum(outp$ET, na.rm=FALSE)
          BioM2[p] <- outp_season$BioMass
          Yld2[p] <- outp_season$Yield
          Dr2[p] <- outp_season$Drain
          NetIrrig2[p] <- outp_season$Irri
        }
        else if (v==3){
          infiltr3[p] <- outp_season$Infilt
          RO3[p] <- outp_season$Runoff
          ETP3[p] <- sum(outp$ET, na.rm=FALSE)
          BioM3[p] <- outp_season$BioMass
          Yld3[p] <- outp_season$Yield
          Dr3[p] <- outp_season$Drain
          NetIrrig3[p] <- outp_season$Irri
        }
        else if (v==4){
          infiltr4[p] <- outp_season$Infilt
          RO4[p] <- outp_season$Runoff
          ETP4[p] <- sum(outp$ET, na.rm=FALSE)
          BioM4[p] <- outp_season$BioMass
          Yld4[p] <- outp_season$Yield
          Dr4[p] <- outp_season$Drain
          NetIrrig4[p] <- outp_season$Irri
        }
        else if (v==5){
          infiltr5[p] <- outp_season$Infilt
          RO5[p] <- outp_season$Runoff
          ETP5[p] <- sum(outp$ET, na.rm=FALSE)
          BioM5[p] <- outp_season$BioMass
          Yld5[p] <- outp_season$Yield
          Dr5[p] <- outp_season$Drain
          NetIrrig5[p] <- outp_season$Irri
        }
        else {
          infiltr6[p] <- outp_season$Infilt
          RO6[p] <- outp_season$Runoff
          ETP6[p] <- sum(outp$ET, na.rm=FALSE)
          BioM6[p] <- outp_season$BioMass
          Yld6[p] <- outp_season$Yield
          Dr6[p] <- outp_season$Drain
          NetIrrig6[p] <- outp_season$Irri
        }
      }
    }else {
      
      soil[3] <- paste ("       ",datasoil$CN5[p],"                  : CN(Curve Number)")
      soil[9] <-  paste("     ",sth1,"      ",datasoil$stp1_vec[p],"    ",datasoil$fcp1_vec[p],"    " ,datasoil$wpp1_vec[p],"      "  ,datasoil$ksat_vec1[p], "          ",pen,"           ",grav,"       ",datasoil$CRa1[p],"    ",datasoil$CRb1[p],"               ",datasoil$soiltext_vec1[p])
      soil[10] <- paste("     ",sth2,"      ",datasoil$stp2_vec[p],"    ",datasoil$fcp2_vec[p],"    " ,datasoil$wpp2_vec[p],"      "  ,datasoil$ksat_vec2[p], "          ",pen,"           ",grav,"       ",datasoil$CRa2[p],"    ",datasoil$CRb2[p],"               ",datasoil$soiltext_vec2[p])
      soil[11] <- paste("     ",sth3,"      ",datasoil$stp3_vec[p],"    ",datasoil$fcp3_vec[p],"    " ,datasoil$wpp3_vec[p],"      "  ,datasoil$ksat_vec3[p], "          ",pen,"           ",grav,"       ",datasoil$CRa3[p],"    ",datasoil$CRb3[p],"               ",datasoil$soiltext_vec3[p])
      soil[12] <- paste("     ",sth4,"      ",datasoil$stp4_vec[p],"    ",datasoil$fcp4_vec[p],"    " ,datasoil$wpp4_vec[p],"      "  ,datasoil$ksat_vec4[p], "          ",pen,"           ",grav,"       ",datasoil$CRa4[p],"    ",datasoil$CRb4[p],"               ",datasoil$soiltext_vec4[p])
      soil[13] <- paste("     ",sth5,"      ",datasoil$stp5_vec[p],"    ",datasoil$fcp5_vec[p],"    " ,datasoil$wpp5_vec[p],"      "  ,datasoil$ksat_vec5[p], "          ",pen,"           ",grav,"       ",datasoil$CRa5[p],"    ",datasoil$CRb5[p],"               ",datasoil$soiltext_vec5[p])
      writeLines(soil, "soil.SOL")

      climate_fun(p,precic,tempminc,tempmac,etc,prec,temp,et)

      system(exe,invisible = FALSE, wait= TRUE)

      setwd(outrun)

      for (v in 1:nbcrops){
        outp <- read.table(paste("Input",v,"PROday.OUT"),header=FALSE, skip=4, col.names= title_oup, fill=TRUE)
        outp_season <-read.table(paste("Input",v,"PROseason.OUT"),header = FALSE, skip=4, col.names=title_seasonal, fill=TRUE)
        if (v==1) {
          infiltr1[p] <- outp_season$Infilt
          RO1[p] <- outp_season$Runoff
          ETP1[p] <- sum(outp$ET, na.rm=FALSE)
          BioM1[p] <- outp_season$BioMass
          Yld1[p] <- outp_season$Yield
          Dr1[p] <- outp_season$Drain
          NetIrrig1[p] <- outp_season$Irri
        }
        else if (v==2){
          infiltr2[p] <- outp_season$Infilt
          RO2[p] <- outp_season$Runoff
          ETP2[p] <- sum(outp$ET, na.rm=FALSE)
          BioM2[p] <- outp_season$BioMass
          Yld2[p] <- outp_season$Yield
          Dr2[p] <- outp_season$Drain
          NetIrrig2[p] <- outp_season$Irri
        }
        else if (v==3){
          infiltr3[p] <- outp_season$Infilt
          RO3[p] <- outp_season$Runoff
          ETP3[p] <- sum(outp$ET, na.rm=FALSE)
          BioM3[p] <- outp_season$BioMass
          Yld3[p] <- outp_season$Yield
          Dr3[p] <- outp_season$Drain
          NetIrrig3[p] <- outp_season$Irri
        }
        else if (v==4){
          infiltr4[p] <- outp_season$Infilt
          RO4[p] <- outp_season$Runoff
          ETP4[p] <- sum(outp$ET, na.rm=FALSE)
          BioM4[p] <- outp_season$BioMass
          Yld4[p] <- outp_season$Yield
          Dr4[p] <- outp_season$Drain
          NetIrrig4[p] <- outp_season$Irri
        }
        else if (v==5){
          infiltr5[p] <- outp_season$Infilt
          RO5[p] <- outp_season$Runoff
          ETP5[p] <- sum(outp$ET, na.rm=FALSE)
          BioM5[p] <- outp_season$BioMass
          Yld5[p] <- outp_season$Yield
          Dr5[p] <- outp_season$Drain
          NetIrrig5[p] <- outp_season$Irri
        }
        else {
          infiltr6[p] <- outp_season$Infilt
          RO6[p] <- outp_season$Runoff
          ETP6[p] <- sum(outp$ET, na.rm=FALSE)
          BioM6[p] <- outp_season$BioMass
          Yld6[p] <- outp_season$Yield
          Dr6[p] <- outp_season$Drain
          NetIrrig6[p] <- outp_season$Irri
        }
      }
    }
  }
Output_AQR(cc_inf,nbcrops, infiltr1,infiltr2,infiltr3,infiltr4,infiltr5,infiltr6,
           RO1,RO2,RO3,RO4,RO5,RO6,
           ETP1,ETP2,ETP3,ETP4,ETP5,ETP6,
           BioM1,BioM2, BioM3,BioM4, BioM5,BioM6,
           Yld1, Yld2,Yld3, Yld4, Yld5, Yld6,
           Dr1, Dr2, Dr3, Dr4, Dr5, Dr6,
           NetIrrig1, NetIrrig2, NetIrrig3, NetIrrig4, NetIrrig5, NetIrrig6)

}






