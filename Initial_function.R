Initial_AQC <- function(AQCroot){

  setwd(AQCroot)
  library(svDialogs)
  irr.input <- dlgInput("Do you have irrigation data (1=yes,0=no)", Sys.info()[""])$res
  if(irr.input=="0"){
    message("The model will run considering rainfed condition")}
  else {
    irr.input2 <- dlgInput("Irrigation data souce (1=Create the text file in the AquaCrop Software (If your irrigation mode is either: determination of net irrigation water requirement or generation of water schedule use this option),0=Fill the table that will be provided", Sys.info()[""])$res
    if(irr.input2=="1"){
      message("Create the irrigation file in the Aquacrop Software and paste it in the root folder of the Aquacrop plugin and name it IRRI.IRR")
    }
    else {
      irow1<-c("Irrigation method (Sprinkler irrigation-1, Surface irrigation: Basin-2, Surface irrigation:Border-3, Surface irrigation: Furrow-4, Drip irrigation-5)",
               "Percentage of soil surface wetted by irrigation")
      irow2 <- c("fill")
      irdata <- data.frame(irow1,irow2)
      write.table(irdata,"Data_Irri1.csv",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")
      irow3<-c("Day ","Depth(mm)","ECw(dS/m)")
      irow4<-c("Fill")
      irdata2<-data.frame(irow3,irow4)
      irdata3<-t(irdata2)
      write.table(irdata3,"Data_Irri2.csv",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")}
    }
 fld_man <- dlgInput("Do you have Field Management Data (Weed,mulch and others),(1=Yes,0=No)", Sys.info()[""])$res
 if(fld_man=="0"){
   message("The model will run without considering field management")
 }
 else {
  fld_man1 <- dlgInput("Field management data source (1= Create the text file in the Aquacrop Software,0=Fill the table that will be provided)", Sys.info()[""])$res
  if(fld_man1=="1"){
    message("Create the Field management file in the Aquacrop Software and paste it in the root folder of the Aquacrop plugin and name it MANE.MAN")
  }
   else {
     frow1 <- c("% of ground surface covered by mulches","% of mulches effet on reduction of soil evaporation (if synthetic plastic-100, organic-50, user specified type-choose from 10 to 100%)",
                "Degree of soil fertility stres(%)", "Height (m) of soil bunds(2digits)",
                "Surface runoff (is not affected-0, is affected or completely prevented-1)",
                "% increase/decrease of soil profile CN value (0 if Not Applicable)",
                "Relative cover of weeds (%)",
                "Increase/decrease of relative cover of weeds in mid season (%)",
                "Shape factor of CC expansion function in weed infested field (2 digits)")
     frow2 <- c("Fill")
     fdata <- data.frame(frow1,frow2)
     write.table(fdata,"Data_Man.csv",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")
   }
 }
 groundw <- dlgInput("Do you have Groundwater table data (1=Yes,0=No)", Sys.info()[""])$res
 if(groundw=="0"){
   message("The model will consider there is no groundwater table")
 }
 else {
   groundw1 <- dlgInput("Groundwater data source (1=Create the text file in the Aquacrop Software (If the groundwater table is variable, choose this option),0=Fill the table that will be provided)", Sys.info()[""])$res
   if(groundw1=="1"){
     message("Create the Groundwater file in the Aquacrop Software and paste it in the root folder of the Aquacrop plugin and name it GWTR.GWT")
   }
   else{
     gr1 <- c("Variation of groundwater table(constant=1)",
              "Day", "Depth (m)", "Salinity of groundwater ECw(dS/m)")
     gr2 <- c("Fill")
     grdata <- data.frame(gr1,gr2)
     write.table(grdata,"Data_GWT.csv",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")
   }
 }
 crop.input <- dlgInput("Crop data Source (1=Create the text file(s) in the Aquacrop Software,0=Fill the table that will be provided) (MAXIMUM 6 DIFFERENT CROPS)", Sys.info()[""])$res
 if(crop.input=="1"){
   message("Create the Crop file(s) in the Aquacrop Software and paste it(them) in the root folder of the Aquacrop plugin and name it(them) CROP1.CRO until CROP5.CRO depending on your number of crops")
 }
 else{
   col1 <- c("Crop Name","Crop type (Fruit/grain = 2, leafy/vegetable crop = 1, root/tuber = 3)","Cropping (Sow = 1, transplant = 0)","Crop cycle (by calendar days = 1, by growing degree days = 0)","Base temperature (°C) below which crop development does not progress","Upper temperature (°C) above which crop development no longer increases with an increase in temperature","Total length of crop cycle in growing degree-days",
             "Soil water depletion factor for canopy expansion (p-exp) - Upper threshold","Soil water depletion factor for canopy expansion (p-exp) - Lower threshold","Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)","Soil water depletion fraction for stomatal control (p - sto) - Upper threshold","Shape factor for water stress coefficient for stomatal control (0.0 = straight line)",
             "Soil water depletion factor for canopy senescence (p - sen) - Upper threshold","Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)","Sum(ETo) during stress period to be exceeded before senescence is triggered","Soil water depletion factor for pollination (p - pol) - Upper threshold","Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)","Considered soil fertility stress for calibration of stress response (%)",
             "Response of canopy expansion is not considered","Response of maximum canopy cover is not considered","Response of crop Water Productivity is not considered","Response of decline of canopy cover is not considered","Minimum air temperature below which pollination starts to fail (cold stress) (°C)","Maximum air temperature above which pollination starts to fail (heat stress) (°C)","Minimum growing degrees required for full crop transpiration (°C - day)","Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)",
             "Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)","Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))","Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))","Crop coefficient when canopy is complete but prior to senescence (KcTr,x)","Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.","Minimum effective rooting depth (m)","Maximum effective rooting depth (m)","Shape factor describing root zone expansion",
             "Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone","Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone","Effect of canopy cover in reducing soil evaporation in late season stage","Soil surface covered by an individual seedling at 90 % emergence (cm2)","Canopy size of individual plant (re-growth) at 1st day (cm2)","Number of plants per hectare","Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)","Maximum decrease of Canopy Growth Coefficient in and between seasons","Number of seasons at which maximum decrease of Canopy Growth Coefficient is reached","Shape factor for decrease Canopy Growth Coefficient","Maximum canopy cover (CCx) in fraction soil cover","Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)","Calendar Days: from sowing to emergence",
             "Calendar Days: from sowing to maximum rooting depth","Calendar Days: from sowing to start senescence","Calendar Days: from sowing to maturity (length of crop cycle)","Calendar Days: from sowing to flowering","Length of the flowering stage (days)","Crop determinancy (liked = 1, 0 = unlinked with flowering)","Excess of potential fruits (%)","Building up of Harvest Index starting at flowering (days)","Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)","Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)","Crop performance under elevated atmospheric CO2 concentration (%)",
             "Reference Harvest Index (HIo) (%)","Possible increase (%) of HI due to water stress before flowering","Coefficient describing positive impact on HI of restricted vegetative growth during yield formation","Coefficient describing negative impact on HI of restricted vegetative growth during yield formation","Coefficient describing negative impact on HI of stomatal closure during yield formation","GDDays: from sowing to emergence","GDDays: from sowing to maximum rooting depth","GDDays: from sowing to start senescence","GDDays: from sowing to maturity (length of crop cycle)","GDDays: from sowing to flowering","Length of the flowering stage (growing degree days)","CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)",
             "CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)","GDDays: building-up of Harvest Index during yield formation")
   col2 <- c("Fill")
   col3 <- c("Fill")
   col4 <- c("Fill")
   col5 <- c("Fill")
   col6 <- c("Fill")
   col7 <- c("Fill")
   cdata <-data.frame(col1,col2,col3,col4,col5,col6,col7)
   write.table(cdata,"Data_Crop.csv",sheetName="Crop(s)",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")
 }
 filecol <- c("First day of simulation","First month of simulation","First year of simulation","Last day of simulation",
             "Last month of simulation","Last year of simulation","First day of cropping","First month of cropping","First year of cropping",
             "Last day of cropping","Last month of cropping","Last year of cropping","Length of simulation","Length of cropping",
              "Number of soil horizons (maximum 5) or Raster path","1st soil horizon depth (meters)","2nd soil horizon depth (meters)(Write NA if not applicable)","3rd soil horizon depth (meters)(Write NA if not applicable)","4th soil horizon depth (meters)(Write NA if not applicable)","5th soil horizon depth (meters)(Write NA if not applicable)",
             "Soil saturation Path for 1st Horizon","Soil saturation Path for 2nd Horizon (If applicable, if not write NA)","Soil saturation Path for 3rd Horizon (If applicable, if not write NA)","Soil saturation Path for 4th Horizon (If applicable, if not write NA)","Soil saturation Path for 5th Horizon (If applicable, if not write NA)",
             "Soil field capacity Path for 1st Horizon","Soil field capacity Path for 2nd Horizon (If applicable, if not write NA)","Soil field capacity Path for 3rd Horizon (If applicable, if not write NA)","Soil field capacity Path for 4th Horizon (If applicable, if not write NA)","Soil field capacity Path for 5th Horizon (If applicable, if not write NA)",
             "Soil wilting point Path for 1st horizon","Soil wilting point Path for 2nd Horizon (If applicable, if not write NA)","Soil wilting point Path for 3rd Horizon (If applicable, if not write NA)","Soil wilting point Path for 4th Horizon (If applicable, if not write NA)","Soil wilting point Path for 5th Horizon (If applicable, if not write NA)",
             "Soil Penetrability(%)","Soil Gravel(%)","Soil Texture(RASTER/VALUE)","Soil Texture Value (If applicable, if not write NA)","Soil Texture Path (If applicable, if not write NA)",
             "Ksat value (If applicable, if not write NA)","Ksat RASTER path (If applicable, if not write NA)","First year of record","Data record frequency(1=daily,2=10-daily,3=monthly)",
             "Precipitation(RASTER/TXT/NETCDF)","Precipitation Path",
             "Temperature(RASTER/TXT/NETDF)","Temperature max path","Temperature min path",
             "Reference Evapotranspiration(RASTER/TXT/NETCDF)","Reference Evapotranspiration Path",
             "Path_of_LIST_folder_Aquacrop","Path_of_OUTP_folder_Aquacrop","Number of crops","Path_of_Aquacrop_folder_ROOT","Aquacrop_Plugin.EXE_path","Aquacrop_root_with_backslash","Will you consider irrigation (put 1 if yes)","Will you consider fiel management (put 1 if yes)","Will you consider the groundwater table (put 1 if yes)")
 filecol2 <- c("Fill")
 filedata <- data.frame(filecol,filecol2)
 write.table(filedata,"Data_Fill.csv",col.names=FALSE,row.names=FALSE,append=TRUE,sep=";")

 message("Please FILL THE FILE which was created in the path provided")
 message("If some of the questions are not applicable to your model just write NA")
  }







