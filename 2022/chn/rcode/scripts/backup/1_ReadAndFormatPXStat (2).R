# ################################Population####################################


#read the pxstat file for EDs
PopSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T1/SAP2022T1T1ASA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
PopSourceTable <- as.data.frame(PopSourceTable.px)
PopSourceTable$CSO.Small.Areas.2022 <- as.character(PopSourceTable$CSO.Small.Areas.2022)
PopSourceTableWSA <- merge(PopSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
PopSourceTableWSA$Region[PopSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
PopSourceTableWSA$IHA[PopSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
PopSourceTableWSA$CHN[PopSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
PopSourceTableWSA$IHA[PopSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
PopSourceTableWSA$CHN[PopSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"
PopSourceTableRegion <- PopSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Sex,Age,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

PopSourceTableIHA <- PopSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Sex,Age,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
PopSourceTableCHN <- PopSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Sex,Age,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
PopSourceTable <- rbind(PopSourceTableRegion,PopSourceTableIHA,PopSourceTableCHN)
#factorise age

#factorise age
PopSourceTable$Age <- factor(PopSourceTable$Age, levels = c("Age 0-4","Age 5-9","Age 10-14","Age 15-19","Age 20-24","Age 25-29","Age 30-34","Age 35-39","Age 40-44","Age 45-49","Age 50-54","Age 55-59","Age 60-64","Age 65-69","Age 70-74","Age 75-79","Age 80-84","Age 85 and over","Total"))


#Create a duplicate table where the age groups will be recategorised for tables
PopSourceTable2 <- PopSourceTable

# recategorise
PopSourceTable2$Age <- gsub("Age 0-4|Age 5-9|Age 10-14", "0-14", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 15-19|Age 20-24", "15-24", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 25-29|Age 30-34|Age 35-39|Age 40-44", "25-44", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 45-49|Age 50-54|Age 55-59|Age 60-64", "45-64", PopSourceTable2$Age )
PopSourceTable2$Age <- gsub("Age 65-69|Age 70-74|Age 75-79|Age 80-84|Age 85 and over", "65 years and over", PopSourceTable2$Age )

#remove word age from category
#PopSourceTable2$Age <- as.character(gsub("Age ","", PopSourceTable2$Age))

#group by new categories for summary

PopSourceTableRegrouped <- PopSourceTable2%>%group_by(Sex,Age,Region)%>%dplyr::summarise(value = sum(value, na.rm=T))
PopSourceTableRegrouped$Age <- as.character(gsub("Age ","", PopSourceTableRegrouped$Age))

#factorise age
PopSourceTableRegrouped$Age <- factor(PopSourceTableRegrouped$Age, levels = c("0-14","15-24","25-44", "45-64","65 years and over","Total"))


#############################################CARERS################################
#REad ED and AC  table from PXSTat
CarersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T12/SAP2022T12T2SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
CarersSourceTable <- as.data.frame(CarersSourceTable.px)
CarersSourceTable$CSO.Small.Areas.2022 <- as.character(CarersSourceTable$CSO.Small.Areas.2022)
CarersSourceTable$CSO.Small.Areas.2022 <- str_pad(CarersSourceTable$CSO.Small.Areas.2022, 9, pad = "0")

CarersSourceTableWSA <- merge(CarersSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
CarersSourceTableWSA$Region[CarersSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "HSE Dublin and South East"
CarersSourceTableWSA$Region[CarersSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "State"
CarersSourceTableWSA$IHA[CarersSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "Three"
CarersSourceTableWSA$IHA[CarersSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore"
CarersSourceTableWSA$CHN[CarersSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "d"
CarersSourceTableWSA$CHN[CarersSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore"
CarersSourceTableRegion <- CarersSourceTableWSA%>%select(-c(IHA,CSO.Small.Areas.2022,CHN))%>%group_by(Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

CarersSourceTableIHA <- CarersSourceTableWSA%>%select(-c(Region,CSO.Small.Areas.2022,CHN))%>%group_by(Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
CarersSourceTableCHN <- CarersSourceTableWSA%>%select(-c(Region,CSO.Small.Areas.2022,IHA))%>%group_by(Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")

CarersSourceTable <- rbind(CarersSourceTableRegion,CarersSourceTableCHN,CarersSourceTableIHA)
#factorise age


############################GENERAL HEALTH###########################################
#import ED and AC tables
GenSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T12/SAP2022T12T3SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
GenSourceTable <- as.data.frame(GenSourceTable.px)
GenSourceTable$CSO.Small.Areas.2022 <- as.character(GenSourceTable$CSO.Small.Areas.2022)

GenSourceTable$CSO.Small.Areas.2022 <- str_pad(GenSourceTable$CSO.Small.Areas.2022, 9, pad = "0")
GenSourceTableWSA <- merge(GenSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
GenSourceTableWSA$Region[GenSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "HSE Dublin and South East"
GenSourceTableWSA$Region[GenSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "State"
GenSourceTableWSA$IHA[GenSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "Three"
GenSourceTableWSA$IHA[GenSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore"
GenSourceTableWSA$CHN[GenSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "d"
GenSourceTableWSA$CHN[GenSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore2"
GenSourceTableRegion <- GenSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(General.Health,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
GenSourceTableIHA <- GenSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(General.Health,Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
GenSourceTableCHN <- GenSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(General.Health,Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
GenSourceTable <- rbind(GenSourceTableRegion,GenSourceTableCHN,GenSourceTableIHA)

#factorise age
#
GenWider <- GenSourceTable %>%
  pivot_wider(
    names_from = General.Health,
    values_from = value)

# Calculate percentages
GenWider$`Very Good as a Percentage` <- GenWider$`Very Good`*100/GenWider$Total
GenWider$`Good as a Percentage` <- GenWider$Good*100/GenWider$Total
GenWider$`Fair as a Percentage` <- GenWider$Fair*100/GenWider$Total
GenWider$`Bad as a Percentage` <- GenWider$Bad*100/GenWider$Total
GenWider$`Very Bad as a Percentage` <- GenWider$`Very Bad`*100/GenWider$Total
GenWider$`Bad or Very Bad` <- GenWider$Bad + GenWider$`Very Bad`
GenWider$`Bad or Very Bad as a Percentage` <- GenWider$`Bad or Very Bad` *100/GenWider$Total  

#################################DISABILITY#####################################
# REad pxstat tables
DisSourceTable2.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T12/SAP2022T12T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
DisSourceTable2 <- as.data.frame(DisSourceTable2.px)
DisSourceTable2$CSO.Small.Areas.2022 <- as.character(DisSourceTable2$CSO.Small.Areas.2022)
DisSourceTable2$CSO.Small.Areas.2022[DisSourceTable2$CSO.Small.Areas.2022!="Ireland"&DisSourceTable2$CSO.Small.Areas.2022!="26701100"] <- str_pad(DisSourceTable2$CSO.Small.Areas.2022[DisSourceTable2$CSO.Small.Areas.2022!="Ireland"&DisSourceTable2$CSO.Small.Areas.2022!="26701100"], 9, pad = "0")

DisSourceTable2WSA <- merge(DisSourceTable2, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
DisSourceTable2WSA$Region[DisSourceTable2WSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

DisSourceTable2WSA$IHA[DisSourceTable2WSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"

DisSourceTable2WSA$IHA[DisSourceTable2WSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

DisSourceTable2WSA$CHN[DisSourceTable2WSA$CSO.Small.Areas.2022 == "26701100"] <- "d"

DisSourceTable2WSA$CHN[DisSourceTable2WSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"


DisSourceTable2IHA <- DisSourceTable2WSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
DisSourceTable2Region <- DisSourceTable2WSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
DisSourceTable2CHN <- DisSourceTable2WSA%>%select(-c(CSO.Small.Areas.2022,IHA,Region))%>%group_by(Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")


DisSourceTable2<-rbind(DisSourceTable2Region,DisSourceTable2CHN,DisSourceTable2IHA)

#factorise a
DisWider<- DisSourceTable2 
# pivot wider for structure that is easier to work with

##############################################SMOKING############################################
# Read pxstat tables
SmokingSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T12/SAP2022T12T4SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
SmokingSourceTable <- as.data.frame(SmokingSourceTable.px)
SmokingSourceTable$CSO.Small.Areas.2022 <- as.character(SmokingSourceTable$CSO.Small.Areas.2022)

SmokingSourceTable$CSO.Small.Areas.2022 <- str_pad(SmokingSourceTable$CSO.Small.Areas.2022, 9, pad = "0")
SmokingSourceTableWSA <- merge(SmokingSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
SmokingSourceTableWSA$Region[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
SmokingSourceTableWSA$Region[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "HSE Dublin and South East"
SmokingSourceTableWSA$Region[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "State"
SmokingSourceTableWSA$IHA[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "Three"
SmokingSourceTableWSA$IHA[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
SmokingSourceTableWSA$IHA[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore"

SmokingSourceTableWSA$CHN[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "026701100"] <- "d"
SmokingSourceTableWSA$CHN[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
SmokingSourceTableWSA$CHN[SmokingSourceTableWSA$CSO.Small.Areas.2022 == "00Ireland"] <- "Ignore2"
SmokingSourceTableRegion <- SmokingSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Smoke.Tobacco.Products,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
SmokingSourceTableIHA <- SmokingSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Smoke.Tobacco.Products,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
SmokingSourceTableCHN <- SmokingSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Smoke.Tobacco.Products,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")

SmokingSourceTable <- rbind(SmokingSourceTableRegion,SmokingSourceTableCHN,SmokingSourceTableIHA)
#factorise age

#pivot wider to make table easier to work with
SmokingWider <- SmokingSourceTable %>%
  pivot_wider(
    names_from = Smoke.Tobacco.Products,
    values_from = value)


#calculate percentages
SmokingWider$PercentageWhoSmokeTobaccoProducts <- SmokingWider$`Persons who smoke tobacco products (Daily and Occasionally)`*100/SmokingWider$`All persons`
SmokingWider$PercentageWhoDontSmokeTobaccoProducts <- sprintf("%.1f", round(SmokingWider$`Persons who donΓÇÖt smoke tobacco products (Never and have given up)`*100/SmokingWider$`All persons`,1))
SmokingWider$PercentageNotStated <- sprintf("%.1f", round(SmokingWider$`Smoking status not stated`*100/SmokingWider$`All persons`,1))
SmokingWider <- SmokingWider%>%dplyr::rename(`Persons who dont smoke tobacco products (Never and have given up)` = `Persons who donΓÇÖt smoke tobacco products (Never and have given up)`)

#################################EDUCATION#####################################
# REad pxstat tables
EduSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T10/SAP2022T10T4SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
EduSourceTable <- as.data.frame(EduSourceTable.px)
EduSourceTable$CSO.Small.Areas.2022 <- as.character(EduSourceTable$CSO.Small.Areas.2022)
EduSourceTableWSA <- merge(EduSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
EduSourceTableWSA$Region[EduSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
EduSourceTableWSA$IHA[EduSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
EduSourceTableWSA$IHA[EduSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
EduSourceTableWSA$CHN[EduSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
EduSourceTableWSA$CHN[EduSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"
EduSourceTableRegion <- EduSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Highest.Level.of.Education.Completed,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
EduSourceTableIHA <- EduSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Highest.Level.of.Education.Completed,Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
EduSourceTableCHN <- EduSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Highest.Level.of.Education.Completed,Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
#bind both tables
EduSourceTable <- rbind(EduSourceTableRegion,EduSourceTableCHN,EduSourceTableIHA)


# filter only both sexes as that is all we are interested in here
EduSourceTable <- EduSourceTable%>%filter(Sex == "Both Sexes")
############################PRINCIPLE ECONOMIC STATUS################################
# REad Pxstat tables

############################PRINCIPLE ECONOMIC STATUS################################
# REad Pxstat tables

PESSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T8/SAP2022T8T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
PESSource <- as.data.frame(PESSource.px)
PESSource$CSO.Small.Areas.2022 <- as.character(PESSource$CSO.Small.Areas.2022)
PESSourceWSA <- merge(PESSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
PESSourceWSA$Region[PESSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
PESSourceWSA$IHA[PESSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
PESSourceWSA$IHA[PESSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
PESSourceWSA$CHN[PESSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
PESSourceWSA$CHN[PESSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

PESSourceRegion <- PESSourceWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Principle.Economic.Status,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
PESSourceIHA <- PESSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Principle.Economic.Status,Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
PESSourceCHN <- PESSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Principle.Economic.Status,Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
PESSource <- rbind(PESSourceRegion,PESSourceIHA,PESSourceCHN)


##############################FAMILIES#####################################
# read pxstat tables
FamSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T4/SAP2022T4T3SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
FamSource <- as.data.frame(FamSource.px)
FamSource$CSO.Small.Areas.2022 <- as.character(FamSource$CSO.Small.Areas.2022)
FamSourceWSA <- merge(FamSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
FamSourceWSA$Region[FamSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
FamSourceWSA$IHA[FamSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
FamSourceWSA$IHA[FamSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
FamSourceWSA$CHN[FamSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
FamSourceWSA$CHN[FamSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"
FamSourceRegion <- FamSourceWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Type.of.Family,Age.of.Child,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
FamSourceIHA <- FamSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Type.of.Family,Age.of.Child,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
FamSourceCHN <- FamSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Type.of.Family,Age.of.Child,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
FamSource <- rbind(FamSourceRegion,FamSourceCHN,FamSourceIHA)



################################BIRTHPLACE########################################
#read pxstat tabls for ac and ED

BirthSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T2/SAP2022T2T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
BirthSource <- as.data.frame(BirthSource.px)%>%filter(Statistic == "Usually resident population by birthplace")

BirthSource$CSO.Small.Areas.2022 <- as.character(BirthSource$CSO.Small.Areas.2022)
BirthSourceWSA <- merge(BirthSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
BirthSourceWSA$Region[BirthSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
BirthSourceWSA$IHA[BirthSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
BirthSourceWSA$IHA[BirthSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
BirthSourceWSA$CHN[BirthSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
BirthSourceWSA$CHN[BirthSourceWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"
BirthSourceRegion <- BirthSourceWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Location,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

BirthSourceIHA <- BirthSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Location,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

BirthSourceCHN <- BirthSourceWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Location,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
BirthSource <- rbind(BirthSourceRegion,BirthSourceCHN,BirthSourceIHA)

###############################VOLUNTEERING####################################
#REad ED and AC  table from PXSTat

VolunteersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T7/SAP2022T7T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
VolunteersSourceTable <- as.data.frame(VolunteersSourceTable.px)

VolunteersSourceTable$CSO.Small.Areas.2022 <- as.character(VolunteersSourceTable$CSO.Small.Areas.2022)
VolunteersSourceTableWSA <- merge(VolunteersSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
VolunteersSourceTableWSA$Region[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
VolunteersSourceTableWSA$IHA[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
VolunteersSourceTableWSA$IHA[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

VolunteersSourceTableWSA$CHN[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
VolunteersSourceTableWSA$IHA[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

VolunteersSourceTableRegion <- VolunteersSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

VolunteersSourceTableIHA <- VolunteersSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

VolunteersSourceTableCHN <- VolunteersSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
VolunteersSourceTable <- rbind(VolunteersSourceTableRegion,VolunteersSourceTableCHN,VolunteersSourceTableIHA)

#########################################SOCIAL CLASS##########################################
#REad ED and AC  table from PXSTat
SocialClassSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T9/SAP2022T9T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
SocialClassSourceTable <- as.data.frame(SocialClassSourceTable.px)%>%filter(Sex == "Both Sexes")

SocialClassSourceTable$CSO.Small.Areas.2022 <- as.character(SocialClassSourceTable$CSO.Small.Areas.2022)
SocialClassSourceTableWSA <- merge(SocialClassSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
SocialClassSourceTableWSA$Region[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
SocialClassSourceTableWSA$IHA[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
SocialClassSourceTableWSA$IHA[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

SocialClassSourceTableWSA$CHN[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
SocialClassSourceTableWSA$CHN[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

SocialClassSourceTableRegion <- SocialClassSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Social.Class,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

SocialClassSourceTableIHA <- SocialClassSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Social.Class,Sex,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

SocialClassSourceTableCHN <- SocialClassSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Social.Class,Sex,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
SocialClassSourceTable <- rbind(SocialClassSourceTableRegion,SocialClassSourceTableCHN,SocialClassSourceTableIHA)

##################################################HOUSEHOLDS##################################################
#REad ED and AC  table from PXSTat
HouseholdsSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T6/SAP2022T6T3SA?query=%7B%22query%22:%5B%7B%22code%22:%22STATISTIC%22,%22selection%22:%7B%22filter%22:%22item%22,%22values%22:%5B%22SAP2022T6T3C01%22%5D%7D%7D%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
HouseholdsSourceTable <- as.data.frame(HouseholdsSourceTable.px)%>%filter(Statistic == "Permanent private households")

HouseholdsSourceTable$CSO.Small.Areas.2022 <- as.character(HouseholdsSourceTable$CSO.Small.Areas.2022)
HouseholdsSourceTableWSA <- merge(HouseholdsSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
HouseholdsSourceTableWSA$Region[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
HouseholdsSourceTableWSA$IHA[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
HouseholdsSourceTableWSA$IHA[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

HouseholdsSourceTableWSA$CHN[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
HouseholdsSourceTableWSA$CHN[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

HouseholdsSourceTableRegion <- HouseholdsSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Type.of.Occupancy,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

HouseholdsSourceTableIHA <- HouseholdsSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Type.of.Occupancy,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

HouseholdsSourceTableCHN <- HouseholdsSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Type.of.Occupancy,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
HouseholdsSourceTable <- rbind(HouseholdsSourceTableRegion,HouseholdsSourceTableCHN,HouseholdsSourceTableIHA)

#################################RENEWABLE ENERGY################################
#REad ED and AC  table from PXSTat
RenewableEnergySourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T6/SAP2022T6T10SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
RenewableEnergySourceTable <- as.data.frame(RenewableEnergySourceTable.px)

RenewableEnergySourceTable$CSO.Small.Areas.2022 <- as.character(RenewableEnergySourceTable$CSO.Small.Areas.2022)
RenewableEnergySourceTableWSA <- merge(RenewableEnergySourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
RenewableEnergySourceTableWSA$Region[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
RenewableEnergySourceTableWSA$IHA[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
RenewableEnergySourceTableWSA$IHA[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

RenewableEnergySourceTableWSA$CHN[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
RenewableEnergySourceTableWSA$CHN[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

RenewableEnergySourceTableRegion <- RenewableEnergySourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Renewable.Energy,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

RenewableEnergySourceTableIHA <- RenewableEnergySourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Renewable.Energy,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

RenewableEnergySourceTableCHN <- RenewableEnergySourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Renewable.Energy,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")


RenewableEnergySourceTable <- rbind(RenewableEnergySourceTableRegion,RenewableEnergySourceTableCHN,RenewableEnergySourceTableIHA)
######################################TRAVEL#########################################
#REad ED and AC  table from PXSTat
TravelSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T11/SAP2022T11T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
TravelSourceTable <- as.data.frame(TravelSourceTable.px)%>%filter(Statistic == "Usually resident by means of travel to work, school, college or childcare (total)")


TravelSourceTable$CSO.Small.Areas.2022 <- as.character(TravelSourceTable$CSO.Small.Areas.2022)
TravelSourceTableWSA <- merge(TravelSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
TravelSourceTableWSA$Region[TravelSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
TravelSourceTableWSA$IHA[TravelSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
TravelSourceTableWSA$IHA[TravelSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"

TravelSourceTableWSA$CHN[TravelSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
TravelSourceTableWSA$CHN[TravelSourceTableWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"

TravelSourceTableRegion <- TravelSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Means.of.Travel,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

TravelSourceTableIHA <- TravelSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Means.of.Travel,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")

TravelSourceTableCHN <- TravelSourceTableWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Means.of.Travel,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")

TravelSourceTable <- rbind(TravelSourceTableIHA,TravelSourceTableCHN,TravelSourceTableRegion)
############################KEY POINTS AND OTHER CALCS#####################################

#key points stats - names used here should be self explanatory
# Area.pxo <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/F1011/PX/2013/en")
# Areao <- as.data.frame(Area.pxo)
# Area.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/F1011/PX/2013/en")
# Area <- as.data.frame(Area.px)

FamiliesInPrivateHouseholds.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T4/SAP2022T4T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")

FamiliesInPrivateHouseholds <- as.data.frame(FamiliesInPrivateHouseholds.px)



FamiliesInPrivateHouseholds$CSO.Small.Areas.2022 <- as.character(FamiliesInPrivateHouseholds$CSO.Small.Areas.2022)
FamiliesInPrivateHouseholdsWSA <- merge(FamiliesInPrivateHouseholds, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
FamiliesInPrivateHouseholdsWSA$Region[FamiliesInPrivateHouseholdsWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"
FamiliesInPrivateHouseholdsWSA$IHA[FamiliesInPrivateHouseholdsWSA$CSO.Small.Areas.2022 == "26701100"] <- "Three"
FamiliesInPrivateHouseholdsWSA$IHA[FamiliesInPrivateHouseholdsWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore"
FamiliesInPrivateHouseholdsWSA$CHN[FamiliesInPrivateHouseholdsWSA$CSO.Small.Areas.2022 == "26701100"] <- "d"
FamiliesInPrivateHouseholdsWSA$CHN[FamiliesInPrivateHouseholdsWSA$CSO.Small.Areas.2022 == "Ireland"] <- "Ignore2"


FamiliesInPrivateHouseholdsRegion <- FamiliesInPrivateHouseholdsWSA%>%select(-c(CSO.Small.Areas.2022,IHA,CHN))%>%group_by(Household.Size,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
FamiliesInPrivateHouseholdsIHA<- FamiliesInPrivateHouseholdsWSA%>%select(-c(CSO.Small.Areas.2022,Region,CHN))%>%group_by(Household.Size,Census.Year,Statistic,IHA)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "IHA")
FamiliesInPrivateHouseholdsCHN<- FamiliesInPrivateHouseholdsWSA%>%select(-c(CSO.Small.Areas.2022,Region,IHA))%>%group_by(Household.Size,Census.Year,Statistic,CHN)%>%dplyr::summarise(value = sum(value))%>%dplyr::rename("Region" = "CHN")
FamiliesInPrivateHouseholds <- rbind(FamiliesInPrivateHouseholdsRegion,FamiliesInPrivateHouseholdsCHN,FamiliesInPrivateHouseholdsIHA)

