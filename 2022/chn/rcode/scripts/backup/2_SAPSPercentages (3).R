#########################################SAPS###################################
# import SAPS files and SAPS Glossary
SAPSED <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_Small_Area_270923.csv"), header = T)
#SAPSCounty <- read.csv(paste0(InputFilesLoc,"/SAPS2022/SAPS_2022_county_270923.csv"), header = T)
SAPSGlossary<- read.csv(paste0(InputFilesLoc,"/SAPS2022/GlossaryEditForPercentages.csv"), header = T)

SAPSTotalWHR <- merge(SAPSED, SAWHRForJoinWithPX, by.x = "GEOGID", by.y = "SA", all.x=T)
SAPSTotalWHR$Region[SAPSTotalWHR$GEOGID == "26701100"] <- "HSE Dublin and South East"
SAPSTotalWHR$IHA[SAPSTotalWHR$GEOGID == "26701100"] <- "HSE Dublin South and Wicklow"
SAPSTotalWHR$IHA[SAPSTotalWHR$GEOGID == "Ireland"] <- "Ignore"
SAPSTotalWHR$CHN[SAPSTotalWHR$GEOGID == "26701100"] <- "Milltown, Churchtown and Dundrum"
SAPSTotalWHR$CHN[SAPSTotalWHR$GEOGID == "Ireland"] <- "Ignore2"

SAPSTotalRegion <- SAPSTotalWHR%>%select(-c(GEOGID,GUID,GEOGDESC,IHA,CHN))%>%group_by(Region)%>%dplyr::summarise(across(everything(), sum,na.rm=T))
SAPSTotalRegion$RegionType <- "HR"
SAPSTotalIHA <- SAPSTotalWHR%>%select(-c(GEOGID,GUID,GEOGDESC,Region,CHN))%>%group_by(IHA)%>%dplyr::summarise(across(everything(), sum,na.rm=T))%>%dplyr::rename("Region" = "IHA")
SAPSTotalIHA$RegionType <- "IHA"
SAPSTotalCHN <- SAPSTotalWHR%>%select(-c(GEOGID,GUID,GEOGDESC,Region,IHA))%>%group_by(CHN)%>%dplyr::summarise(across(everything(), sum,na.rm=T))%>%dplyr::rename("Region" = "CHN")
SAPSTotalCHN$RegionType <- "CHN"
SAPSTotal <- rbind(SAPSTotalIHA,SAPSTotalRegion,SAPSTotalCHN)


for (j in 1:nrow(SAPSGlossary)) {
  # Get the column name and total column name from the csv
  ColumnName <- SAPSGlossary$ColName[j]
  TotalColumnName <- SAPSGlossary$TotalColForPerc[j]
  
  # Create a new column name with "perc" appended
  NewColumnName <- paste0(ColumnName, "_Perc")
  
  # Calculate the new column 
  SAPSTotal <- SAPSTotal %>%
    dplyr::mutate({{NewColumnName }} := get(ColumnName)*100 / get(TotalColumnName))
}

# new_col will be added to main_df for each iteration
SAPSPercentages <- SAPSTotal%>%dplyr::select(matches("_Perc"))
#SAPSPercentages$GUID <- SAPSTotal$GUID
colnames(SAPSPercentages) <- gsub("_Perc","", colnames(SAPSPercentages))

# round the percentages to one decimal place
SAPSPercentages <- SAPSPercentages%>%mutate_if(is.numeric, function(x) round(x,1))

# Select the appropriate ED,AC and State from the SAPS percentages file
#SAPSPercentages <- SAPSPercentages[!duplicated(SAPSPercentages$GUID),]
#SAPSTotal <- SAPSTotal[!duplicated(SAPSTotal$GUID),]

SAPSPercentages$ED <- SAPSTotal$Region
SAPSPercentages$RegionType <- SAPSTotal$RegionType
saveRDS(SAPSPercentages, file = paste0(OutputFilesLoc,"/SAPSPercentages.Rds"))

