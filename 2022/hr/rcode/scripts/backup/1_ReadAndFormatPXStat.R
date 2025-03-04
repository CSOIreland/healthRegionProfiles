# ################################Population####################################


#read the pxstat file for EDs
PopSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T1/SAP2022T1T1ASA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
PopSourceTable <- as.data.frame(PopSourceTable.px)
PopSourceTable$CSO.Small.Areas.2022 <- as.character(PopSourceTable$CSO.Small.Areas.2022)
PopSourceTableWSA <- merge(PopSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
PopSourceTableWSA$Region[PopSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

PopSourceTable <- PopSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Sex,Age,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
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

CarersSourceTable <- CarersSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
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

GenSourceTable <- GenSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(General.Health,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
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

DisSourceTable2 <- DisSourceTable2WSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
#factorise a
DisWider<- DisSourceTable2 
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

SmokingSourceTable <- SmokingSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Smoke.Tobacco.Products,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))
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

EduSourceTable <- EduSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Highest.Level.of.Education.Completed,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

#bind both tables
EduSourceTable <- rbind(EduSourceTable)


# filter only both sexes as that is all we are interested in here
EduSourceTable <- EduSourceTable%>%filter(Sex == "Both Sexes")

############################PRINCIPLE ECONOMIC STATUS################################
# REad Pxstat tables

PESSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T8/SAP2022T8T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
PESSource <- as.data.frame(PESSource.px)
PESSource$CSO.Small.Areas.2022 <- as.character(PESSource$CSO.Small.Areas.2022)
PESSourceWSA <- merge(PESSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
PESSourceWSA$Region[PESSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

PESSource <- PESSourceWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Principle.Economic.Status,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))


PESSource <- PESSource%>%filter(Sex == "Both Sexes")

##############################FAMILIES#####################################
# read pxstat tables
FamSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T4/SAP2022T4T3SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
FamSource <- as.data.frame(FamSource.px)
FamSource$CSO.Small.Areas.2022 <- as.character(FamSource$CSO.Small.Areas.2022)
FamSourceWSA <- merge(FamSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
FamSourceWSA$Region[FamSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

FamSource <- FamSourceWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Type.of.Family,Age.of.Child,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))




################################BIRTHPLACE########################################
#read pxstat tabls for ac and ED

BirthSource.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T2/SAP2022T2T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
BirthSource <- as.data.frame(BirthSource.px)%>%filter(Statistic == "Usually resident population by birthplace")

BirthSource$CSO.Small.Areas.2022 <- as.character(BirthSource$CSO.Small.Areas.2022)
BirthSourceWSA <- merge(BirthSource, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
BirthSourceWSA$Region[BirthSourceWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

BirthSource <- BirthSourceWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Location,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))



###############################VOLUNTEERING####################################
#REad ED and AC  table from PXSTat

VolunteersSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T7/SAP2022T7T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
VolunteersSourceTable <- as.data.frame(VolunteersSourceTable.px)

VolunteersSourceTable$CSO.Small.Areas.2022 <- as.character(VolunteersSourceTable$CSO.Small.Areas.2022)
VolunteersSourceTableWSA <- merge(VolunteersSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
VolunteersSourceTableWSA$Region[VolunteersSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

VolunteersSourceTable <- VolunteersSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))



#########################################SOCIAL CLASS##########################################
#REad ED and AC  table from PXSTat
SocialClassSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T9/SAP2022T9T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
SocialClassSourceTable <- as.data.frame(SocialClassSourceTable.px)%>%filter(Sex == "Both Sexes")

SocialClassSourceTable$CSO.Small.Areas.2022 <- as.character(SocialClassSourceTable$CSO.Small.Areas.2022)
SocialClassSourceTableWSA <- merge(SocialClassSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
SocialClassSourceTableWSA$Region[SocialClassSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

SocialClassSourceTable <- SocialClassSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Social.Class,Sex,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))




##################################################HOUSEHOLDS##################################################
#REad ED and AC  table from PXSTat
HouseholdsSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T6/SAP2022T6T3SA?query=%7B%22query%22:%5B%7B%22code%22:%22STATISTIC%22,%22selection%22:%7B%22filter%22:%22item%22,%22values%22:%5B%22SAP2022T6T3C01%22%5D%7D%7D%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
HouseholdsSourceTable <- as.data.frame(HouseholdsSourceTable.px)%>%filter(Statistic == "Permanent private households")

HouseholdsSourceTable$CSO.Small.Areas.2022 <- as.character(HouseholdsSourceTable$CSO.Small.Areas.2022)
HouseholdsSourceTableWSA <- merge(HouseholdsSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
HouseholdsSourceTableWSA$Region[HouseholdsSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

HouseholdsSourceTable <- HouseholdsSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Type.of.Occupancy,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))




#################################RENEWABLE ENERGY################################
#REad ED and AC  table from PXSTat
RenewableEnergySourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T6/SAP2022T6T10SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
RenewableEnergySourceTable <- as.data.frame(RenewableEnergySourceTable.px)

RenewableEnergySourceTable$CSO.Small.Areas.2022 <- as.character(RenewableEnergySourceTable$CSO.Small.Areas.2022)
RenewableEnergySourceTableWSA <- merge(RenewableEnergySourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
RenewableEnergySourceTableWSA$Region[RenewableEnergySourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

RenewableEnergySourceTable <- RenewableEnergySourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Renewable.Energy,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))


######################################TRAVEL#########################################
#REad ED and AC  table from PXSTat
TravelSourceTable.px <- read.px("https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.PxAPIv1/en/88/SM2022T11/SAP2022T11T1SA?query=%7B%22query%22:%5B%5D,%22response%22:%7B%22format%22:%22px%22,%22pivot%22:null,%22codes%22:false%7D%7D")
TravelSourceTable <- as.data.frame(TravelSourceTable.px)%>%filter(Statistic == "Usually resident by means of travel to work, school, college or childcare (total)")


TravelSourceTable$CSO.Small.Areas.2022 <- as.character(TravelSourceTable$CSO.Small.Areas.2022)
TravelSourceTableWSA <- merge(TravelSourceTable, SAWHRForJoinWithPX, by.x = "CSO.Small.Areas.2022", by.y = "SA", all.x=T)
TravelSourceTableWSA$Region[TravelSourceTableWSA$CSO.Small.Areas.2022 == "26701100"] <- "HSE Dublin and South East"

TravelSourceTable <- TravelSourceTableWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Means.of.Travel,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))


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

FamiliesInPrivateHouseholds <- FamiliesInPrivateHouseholdsWSA%>%select(-CSO.Small.Areas.2022)%>%group_by(Household.Size,Census.Year,Statistic,Region)%>%dplyr::summarise(value = sum(value))

