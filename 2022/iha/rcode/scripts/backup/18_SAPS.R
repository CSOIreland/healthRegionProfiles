SAPSPercentagesEDACState <- SAPSPercentages[SAPSPercentages$ED == ED | 	SAPSPercentages$ED == AC|SAPSPercentages$ED == "State",]
#SAPSPercentagesEDACState <- SAPSPercentagesEDACState[!duplicated(SAPSPercentagesEDACState$GUID),]
# Add ED var
SAPSPercentagesEDACState$ED[SAPSPercentagesEDACState$ED == "State"] <- "State"
SAPSPercentagesEDACState$ED[SAPSPercentagesEDACState$ED == ED] <- "ED"
SAPSPercentagesEDACState$ED[SAPSPercentagesEDACState$ED == AC] <- "AC"

# Transpose the dataframe so tables can be more easily created
SAPSTransposed <- as.data.frame(t(SAPSPercentagesEDACState))
colnames(SAPSTransposed) <- SAPSTransposed[rownames(SAPSTransposed) == "ED",]
SAPSTransposed <- SAPSTransposed[rownames(SAPSTransposed)!="ED" & rownames(SAPSTransposed)!="ED", ]
SAPSTransposed$Lookup <- rownames(SAPSTransposed)
# SAPSTransposed$AbsDiff <- round(as.numeric(SAPSTransposed$ED) - as.numeric(SAPSTransposed$State),1)
# SAPSTransposed$RelDiff <- round(as.numeric(SAPSTransposed$ED)*100/as.numeric(SAPSTransposed$AC) - 100,1)

# merge with glossary file so appropriate tables can be created using columns from glossary
SAPSWGlossary <- merge(SAPSTransposed, SAPSGlossary, by.x = "Lookup", by.y = "ColName", all.x=T)
SAPSWGlossary[is.na(SAPSWGlossary)] <- "Missing"


# Columns to select for each dataframe
ColsToSelect <- c("Label", "ED", "AC","State", "LatexTableTitle","Rank")
SAPSWGlossary$LatexTableTitle <- factor(SAPSWGlossary$LatexTableTitle,levels = unique(SAPSWGlossary$LatexTableTitle))

# Split the dataframes based on latex table title
SplitDataframes <- SAPSWGlossary %>%
  dplyr::select(all_of(ColsToSelect)) %>%
  group_split(LatexTableTitle, .keep = F)


# Naming the list elements with make.names of the values in  latextabletitle column
names(SplitDataframes) <- make.names(unique(SAPSWGlossary$LatexTableTitle))

# Creating individual dataframes from the list
list2env(SplitDataframes, envir = .GlobalEnv)


#Ordering the tables by the "rank column"
Education..percentage.of.those.aged.15.. <- as.data.frame(Education..percentage.of.those.aged.15..)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Employment..percentage.of.those.aged.15.. <- as.data.frame(Employment..percentage.of.those.aged.15..)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Employment..percentage.of.those.at.work. <- as.data.frame(Employment..percentage.of.those.at.work.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Families..percentage.of.family.units. <- as.data.frame(Families..percentage.of.family.units.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Health..percentage.of.population. <- as.data.frame(Health..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Household.Composition..percentage.of.private.households. <- as.data.frame(Household.Composition..percentage.of.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.households. <- as.data.frame(Housing..percentage.of.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.permanent.dwellings. <- as.data.frame(Housing..percentage.of.permanent.dwellings.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.permanent.private.households. <- as.data.frame(Housing..percentage.of.permanent.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Housing..percentage.of.private.households. <- as.data.frame(Housing..percentage.of.private.households.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Marital.Status..percentage.of.population. <- as.data.frame(Marital.Status..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Migration.and.Ethnicity..percentage.of.population. <- as.data.frame(Migration.and.Ethnicity..percentage.of.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Migration.and.Ethnicity..percentage.of.usually.resident.population. <- as.data.frame(Migration.and.Ethnicity..percentage.of.usually.resident.population.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Occupation..percentage.of.those.at.work.or.unemployed. <- as.data.frame(Occupation..percentage.of.those.at.work.or.unemployed.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Social.Class..percentage.of.population... <- as.data.frame(Social.Class..percentage.of.population...)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Socio.economic.group.of.reference.person..percentage.of.private.households... <- as.data.frame(Socio.economic.group.of.reference.person..percentage.of.private.households...)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Transport..percentage.of.population.aged.5.years.and.over. <- as.data.frame(Transport..percentage.of.population.aged.5.years.and.over.)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Other <- as.data.frame(Other)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)
Age1 <- as.data.frame(Age1)%>%mutate_at("Rank", as.numeric)%>%arrange(Rank)


# refactoring the age column of the age table
Age1$AgeNew <- ifelse(Age1$Label == "Age 0 - Total"|Age1$Label=="Age 1 - Total"|Age1$Label=="Age 2 - Total"|Age1$Label=="Age 3 - Total"|Age1$Label=="Age 4 - Total"|Age1$Label == "Age 5 - Total"|Age1$Label=="Age 6 - Total"|Age1$Label=="Age 7 - Total"|Age1$Label=="Age 8 - Total"|Age1$Label=="Age 9 - Total"|Age1$Label == "Age 10 - Total"|Age1$Label=="Age 11 - Total"|Age1$Label=="Age 12 - Total"|Age1$Label=="Age 13 - Total"|Age1$Label=="Age 14 - Total", "0-14",
              ifelse(Age1$Label=="Age 15 - Total"|Age1$Label=="Age 16 - Total"|Age1$Label=="Age 17 - Total"|Age1$Label=="Age 18 - Total"|Age1$Label=="Age 19 - Total"|Age1$Label == "Age 20 - 24 - Total"|Age1$Label=="Age 25 - 29 - Total"|Age1$Label=="Age 30 - 34 - Total"|Age1$Label=="Age 35 - 39 - Total"|Age1$Label=="Age 40 - 44 - Total"|Age1$Label == "Age 45 - 49 - Total"|Age1$Label=="Age 50 - 54 - Total"|Age1$Label=="Age 55 - 59 - Total"|Age1$Label=="Age 60 - 64 - Total", "15-64",
                                   ifelse(Age1$Label == "Age 65 - 69 - Total"|Age1$Label=="Age 70 - 74 - Total"|Age1$Label == "Age 75 - 79 - Total"|Age1$Label=="Age 80 - 84 - Total"|Age1$Label == "Age 85 and over - Total", "65+", "Missing")))
#changing format of Age columns so rank works successfully
Age1$ED <- as.numeric(Age1$ED)
Age1$AC <- as.numeric(Age1$AC)
Age1$State <- as.numeric(Age1$State)
Age1$Rank <- as.character(Age1$Rank)
Age2 <- Age1%>%group_by(AgeNew)%>%dplyr::summarise(across(where(is.numeric), sum), across(where(is.character), ~first(.)))
Age2$Rank <- as.numeric(Age2$Rank)
Age2 <- Age2[order(Age2$Rank),]

# changing the labels of the transport tables
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare on foot", "On Foot", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Bicycle", "By Bicycle", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Bus, minibus or coach", "By Bus, minibus or coach", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Train, DART or LUAS", "By Train, DART or LUAS", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Motorcycle or scooter", "By Motorcycle or scooter", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Car driver", "Car driver", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Car passenger", "Car passenger", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Van", "By Van", Transport..percentage.of.population.aged.5.years.and.over.$Label)
Transport..percentage.of.population.aged.5.years.and.over.$Label <- gsub("Travel to work, school, college or childcare by Other (incl. lorry)", "By Other (incl. lorry)", Transport..percentage.of.population.aged.5.years.and.over.$Label, fixed=T)


SAPSPercentagesRegion <- SAPSPercentages%>%filter(RegionType == "HR")
SAPSPercentagesIHA <- SAPSPercentages%>%filter(RegionType == "IHA")
CHNsInIHA2 <- SAPSHRLookup$IHA[SAPSHRLookup$Region == ED]
SAPSPercentagesIHAHR <- SAPSPercentages%>%filter(RegionType == "IHA" & ED %in% CHNsInIHA2)

SAPSTotalRegionRanked <- SAPSPercentagesRegion%>%filter(ED!="State" & ED!="ignore"& ED!="ignore2"& ED!="Ignore"& ED!="Ignore2"& ED!="ignore3"& ED!="Ignore3")%>%mutate(PrimaryOrLower =T10_4_NFT +T10_4_PT,
                                                                                DegreeOrHigher = T10_4_ODNDT + T10_4_HDPQT + T10_4_PDT + T10_4_DT,
                                                                                ZeroTo14 = T1_1AGE1T+T1_1AGE2T+T1_1AGE3T+T1_1AGE4T+T1_1AGE5T+T1_1AGE6T+T1_1AGE7T
                                                                                +T1_1AGE8T+T1_1AGE9T+T1_1AGE10T+T1_1AGE11T+T1_1AGE12T+T1_1AGE13T+T1_1AGE14T,
                                                                                SixtyFiveP = T1_1AGE65_69T+T1_1AGE70_74T+T1_1AGE75_79T+T1_1AGE80_84T+T1_1AGEGE_85T,
                                                                                BadVBadHealth = T12_3_BT + T12_3_VBT,
                                                                                Unemployed = T8_1_STUT+T8_1_LTUT,
                                                                                LoneParentsFamUnits =T4_3FOPMCT+T4_3FOPFCT,
                                                                                OutsideIreland = T2_1UKBP+T2_1PLBP+T2_1INBP+T2_1EUBP+T2_1RWBP,
                                                                                OnFootBicycle = T11_1_FT+T11_1_BIT)%>%mutate(
                                                                                  PrimaryOrLowerRanked =  dense_rank(desc(PrimaryOrLower)),
                                                                                  DegreeOrHigherRanked = dense_rank(desc(DegreeOrHigher)),
                                                                                  BadVBadHealthRanked = dense_rank(desc(BadVBadHealth)),
                                                                                  UnemployedRanked = dense_rank(desc(Unemployed)),
                                                                                  LoneParFamUnitRanked = dense_rank(desc(LoneParentsFamUnits)),
                                                                                  SmokingRanked = dense_rank(desc(T12_4_YES)),
                                                                                  LARanked = dense_rank(desc(T6_3_RLAH)),
                                                                                  OwnedOutrightRanked = dense_rank(desc(T6_3_OOH)),
                                                                                  OutsideIrelandRanked = dense_rank(desc(OutsideIreland)),
                                                                                  UnskilledRanked = dense_rank(desc(T9_1_UST)),
                                                                                  ProfWorkersRanked = dense_rank(desc(T9_1_PWT)),
                                                                                  MalesDisabilityRanked = dense_rank(desc(T12_1_M)),
                                                                                  FemalesDisabilityRanked = dense_rank(desc(T12_1_F)),
                                                                                  MalesCarersRanked = dense_rank(desc(T12_2_M)),
                                                                                  FemalesCarersRanked = dense_rank(desc(T12_2_F)),
                                                                                  VolunteersRanked = dense_rank(desc(T7_1_VOL)),
                                                                                  RenewableEnergyRanked = dense_rank(desc(T6_10_NORE)),
                                                                                  OnFootBicycleRanked = dense_rank(desc(OnFootBicycle)),
                                                                                  CarDriverRanked = dense_rank(desc(T11_1_CDT)),
                                                                                  ZeroTo14Ranked = dense_rank(desc(ZeroTo14)),
                                                                                  SixtyFivePRanked = dense_rank(desc(ZeroTo14))
                                                                                )

SAPSTotalIHARanked <- SAPSPercentagesIHA%>%filter(ED!="State" & ED!="ignore"& ED!="ignore2"& ED!="Ignore"& ED!="Ignore2"& ED!="ignore3"& ED!="Ignore3")%>%mutate(PrimaryOrLower =T10_4_NFT +T10_4_PT,
                                                                          DegreeOrHigher = T10_4_ODNDT + T10_4_HDPQT + T10_4_PDT + T10_4_DT,
                                                                          ZeroTo14 = T1_1AGE1T+T1_1AGE2T+T1_1AGE3T+T1_1AGE4T+T1_1AGE5T+T1_1AGE6T+T1_1AGE7T
                                                                          +T1_1AGE8T+T1_1AGE9T+T1_1AGE10T+T1_1AGE11T+T1_1AGE12T+T1_1AGE13T+T1_1AGE14T,
                                                                          SixtyFiveP = T1_1AGE65_69T+T1_1AGE70_74T+T1_1AGE75_79T+T1_1AGE80_84T+T1_1AGEGE_85T,
                                                                          BadVBadHealth = T12_3_BT + T12_3_VBT,
                                                                          Unemployed = T8_1_STUT+T8_1_LTUT,
                                                                          LoneParentsFamUnits =T4_3FOPMCT+T4_3FOPFCT,
                                                                          OutsideIreland = T2_1UKBP+T2_1PLBP+T2_1INBP+T2_1EUBP+T2_1RWBP,
                                                                          OnFootBicycle = T11_1_FT+T11_1_BIT)%>%mutate(
                                                                            PrimaryOrLowerRanked =  dense_rank(desc(PrimaryOrLower)),
                                                                            DegreeOrHigherRanked = dense_rank(desc(DegreeOrHigher)),
                                                                            BadVBadHealthRanked = dense_rank(desc(BadVBadHealth)),
                                                                            UnemployedRanked = dense_rank(desc(Unemployed)),
                                                                            LoneParFamUnitRanked = dense_rank(desc(LoneParentsFamUnits)),
                                                                            SmokingRanked = dense_rank(desc(T12_4_YES)),
                                                                            LARanked = dense_rank(desc(T6_3_RLAH)),
                                                                            OwnedOutrightRanked = dense_rank(desc(T6_3_OOH)),
                                                                            OutsideIrelandRanked = dense_rank(desc(OutsideIreland)),
                                                                            UnskilledRanked = dense_rank(desc(T9_1_UST)),
                                                                            ProfWorkersRanked = dense_rank(desc(T9_1_PWT)),
                                                                            MalesDisabilityRanked = dense_rank(desc(T12_1_M)),
                                                                            FemalesDisabilityRanked = dense_rank(desc(T12_1_F)),
                                                                            MalesCarersRanked = dense_rank(desc(T12_2_M)),
                                                                            FemalesCarersRanked = dense_rank(desc(T12_2_F)),
                                                                            VolunteersRanked = dense_rank(desc(T7_1_VOL)),
                                                                            RenewableEnergyRanked = dense_rank(desc(T6_10_NORE)),
                                                                            OnFootBicycleRanked = dense_rank(desc(OnFootBicycle)),
                                                                            CarDriverRanked = dense_rank(desc(T11_1_CDT)),
                                                                            ZeroTo14Ranked = dense_rank(desc(ZeroTo14)),
                                                                            SixtyFivePRanked = dense_rank(desc(ZeroTo14))
                                                                          )

SAPSTotalIHAHRRanked <- SAPSPercentagesIHAHR%>%filter(ED!="State" & ED!="ignore"& ED!="ignore2"& ED!="Ignore"& ED!="Ignore2"& ED!="ignore3"& ED!="Ignore3")%>%mutate(PrimaryOrLower =T10_4_NFT +T10_4_PT,
                                                                          DegreeOrHigher = T10_4_ODNDT + T10_4_HDPQT + T10_4_PDT + T10_4_DT,
                                                                          ZeroTo14 = T1_1AGE1T+T1_1AGE2T+T1_1AGE3T+T1_1AGE4T+T1_1AGE5T+T1_1AGE6T+T1_1AGE7T
                                                                          +T1_1AGE8T+T1_1AGE9T+T1_1AGE10T+T1_1AGE11T+T1_1AGE12T+T1_1AGE13T+T1_1AGE14T,
                                                                          SixtyFiveP = T1_1AGE65_69T+T1_1AGE70_74T+T1_1AGE75_79T+T1_1AGE80_84T+T1_1AGEGE_85T,
                                                                          BadVBadHealth = T12_3_BT + T12_3_VBT,
                                                                          Unemployed = T8_1_STUT+T8_1_LTUT,
                                                                          LoneParentsFamUnits =T4_3FOPMCT+T4_3FOPFCT,
                                                                          OutsideIreland = T2_1UKBP+T2_1PLBP+T2_1INBP+T2_1EUBP+T2_1RWBP,
                                                                          OnFootBicycle = T11_1_FT+T11_1_BIT)%>%mutate(
                                                                            PrimaryOrLowerRanked =  dense_rank(desc(PrimaryOrLower)),
                                                                            DegreeOrHigherRanked = dense_rank(desc(DegreeOrHigher)),
                                                                            BadVBadHealthRanked = dense_rank(desc(BadVBadHealth)),
                                                                            UnemployedRanked = dense_rank(desc(Unemployed)),
                                                                            LoneParFamUnitRanked = dense_rank(desc(LoneParentsFamUnits)),
                                                                            SmokingRanked = dense_rank(desc(T12_4_YES)),
                                                                            LARanked = dense_rank(desc(T6_3_RLAH)),
                                                                            OwnedOutrightRanked = dense_rank(desc(T6_3_OOH)),
                                                                            OutsideIrelandRanked = dense_rank(desc(OutsideIreland)),
                                                                            UnskilledRanked = dense_rank(desc(T9_1_UST)),
                                                                            ProfWorkersRanked = dense_rank(desc(T9_1_PWT)),
                                                                            MalesDisabilityRanked = dense_rank(desc(T12_1_M)),
                                                                            FemalesDisabilityRanked = dense_rank(desc(T12_1_F)),
                                                                            MalesCarersRanked = dense_rank(desc(T12_2_M)),
                                                                            FemalesCarersRanked = dense_rank(desc(T12_2_F)),
                                                                            VolunteersRanked = dense_rank(desc(T7_1_VOL)),
                                                                            RenewableEnergyRanked = dense_rank(desc(T6_10_NORE)),
                                                                            OnFootBicycleRanked = dense_rank(desc(OnFootBicycle)),
                                                                            CarDriverRanked = dense_rank(desc(T11_1_CDT)),
                                                                            ZeroTo14Ranked = dense_rank(desc(ZeroTo14)),
                                                                            SixtyFivePRanked = dense_rank(desc(ZeroTo14))
                                                                          )
SAPSTotalIHAHRRanked2 <- SAPSTotalIHAHRRanked[SAPSTotalIHAHRRanked$ED == ED,]

PrimaryOrLowerRankedIHAHR <- SAPSTotalIHAHRRanked2$PrimaryOrLowerRanked
DegreeOrHigherRankedIHAHR <- SAPSTotalIHAHRRanked2$DegreeOrHigherRanked
BadVBadHealthRankedIHAHR <- SAPSTotalIHAHRRanked2$BadVBadHealthRanked
UnemployedRankedIHAHR  <- SAPSTotalIHAHRRanked2$UnemployedRanked
LoneParFamUnitRankedIHAHR <- SAPSTotalIHAHRRanked2$LoneParFamUnitRanked
SmokingRankedIHAHR <- SAPSTotalIHAHRRanked2$SmokingRanked 
LARankedIHAHR <- SAPSTotalIHAHRRanked2$LARanked 
OwnedOutrightRankedIHAHR <- SAPSTotalIHAHRRanked2$OwnedOutrightRanked
OutsideIrelandRankedIHAHR  <- SAPSTotalIHAHRRanked2$OutsideIrelandRanked
UnskilledRankedIHAHR <-  SAPSTotalIHAHRRanked2$UnskilledRanked 
ProfWorkersRankedIHAHR <- SAPSTotalIHAHRRanked2$ProfWorkersRanked
MalesDisabilityRankedIHAHR <- SAPSTotalIHAHRRanked2$MalesDisabilityRanked
FemalesDisabilityRankedIHAHR <- SAPSTotalIHAHRRanked2$FemalesDisabilityRanked
MalesCarersRankedIHAHR <- SAPSTotalIHAHRRanked2$MalesCarersRanked
FemalesCarersRankedIHAHR <- SAPSTotalIHAHRRanked2$FemalesCarersRanked
VolunteersRankedIHAHR  <- SAPSTotalIHAHRRanked2$VolunteersRanked
RenewableEnergyRankedIHAHR <- SAPSTotalIHAHRRanked2$RenewableEnergyRanked 
OnFootBicycleRankedIHAHR <- SAPSTotalIHAHRRanked2$OnFootBicycleRanked 
CarDriverRankedIHAHR <- SAPSTotalIHAHRRanked2$CarDriverRanked 
ZeroTo14RankedIHAHR <- SAPSTotalIHAHRRanked2$ZeroTo14Ranked 
SixtyFivePRankedIHAHR <- SAPSTotalIHAHRRanked2$SixtyFivePRanked

PrimaryOrLowerRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$PrimaryOrLowerRanked == PrimaryOrLowerRankedIHAHR)>1,"Joint","Not Joint")
DegreeOrHigherRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$DegreeOrHigherRanked == DegreeOrHigherRankedIHAHR)>1,"Joint","Not Joint")
BadVBadHealthRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$BadVBadHealthRanked == BadVBadHealthRankedIHAHR)>1,"Joint","Not Joint")
UnemployedRankedIHAHRJoint  <- ifelse(sum(SAPSTotalIHAHRRanked2$UnemployedRanked == UnemployedRankedIHAHR)>1,"Joint","Not Joint")
LoneParFamUnitRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$LoneParFamUnitRanked == LoneParFamUnitRankedIHAHR)>1,"Joint","Not Joint")
SmokingRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$SmokingRanked  == SmokingRankedIHAHR)>1,"Joint","Not Joint")
LARankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$LARanked  == LARankedIHAHR)>1,"Joint","Not Joint")
OwnedOutrightRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$OwnedOutrightRanked == OwnedOutrightRankedIHAHR)>1,"Joint","Not Joint")
OutsideIrelandRankedIHAHRJoint  <- ifelse(sum(SAPSTotalIHAHRRanked2$OutsideIrelandRanked == OutsideIrelandRankedIHAHR)>1,"Joint","Not Joint")
UnskilledRankedIHAHRJoint <-  ifelse(sum(SAPSTotalIHAHRRanked2$UnskilledRanked  == UnskilledRankedIHAHR)>1,"Joint","Not Joint")
ProfWorkersRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$ProfWorkersRanked == ProfWorkersRankedIHAHR)>1,"Joint","Not Joint")
MalesDisabilityRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$MalesDisabilityRanked == MalesDisabilityRankedIHAHR)>1,"Joint","Not Joint")
FemalesDisabilityRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$FemalesDisabilityRanked == FemalesDisabilityRankedIHAHR)>1,"Joint","Not Joint")
MalesCarersRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$MalesCarersRanked == MalesCarersRankedIHAHR)>1,"Joint","Not Joint")
FemalesCarersRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$FemalesCarersRanked == FemalesCarersRankedIHAHR)>1,"Joint","Not Joint")
VolunteersRankedIHAHRJoint  <- ifelse(sum(SAPSTotalIHAHRRanked2$VolunteersRanked == VolunteersRankedIHAHR)>1,"Joint","Not Joint")
RenewableEnergyRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$RenewableEnergyRanked  == RenewableEnergyRankedIHAHR)>1,"Joint","Not Joint")
OnFootBicycleRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$OnFootBicycleRanked  == OnFootBicycleRankedIHAHR)>1,"Joint","Not Joint")
CarDriverRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$CarDriverRanked  == CarDriverRankedIHAHR)>1,"Joint","Not Joint")
ZeroTo14RankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$ZeroTo14Ranked  == ZeroTo14RankedIHAHR)>1,"Joint","Not Joint")
SixtyFivePRankedIHAHRJoint <- ifelse(sum(SAPSTotalIHAHRRanked2$SixtyFivePRanked == SixtyFivePRankedIHAHR)>1,"Joint","Not Joint")

################################
SAPSTotalIHARanked2 <- SAPSTotalIHARanked[SAPSTotalIHARanked$ED == ED,]

PrimaryOrLowerRankedIHA <- SAPSTotalIHARanked2$PrimaryOrLowerRanked
DegreeOrHigherRankedIHA <- SAPSTotalIHARanked2$DegreeOrHigherRanked
BadVBadHealthRankedIHA <- SAPSTotalIHARanked2$BadVBadHealthRanked
UnemployedRankedIHA  <- SAPSTotalIHARanked2$UnemployedRanked
LoneParFamUnitRankedIHA <- SAPSTotalIHARanked2$LoneParFamUnitRanked
SmokingRankedIHA <- SAPSTotalIHARanked2$SmokingRanked 
LARankedIHA <- SAPSTotalIHARanked2$LARanked 
OwnedOutrightRankedIHA <- SAPSTotalIHARanked2$OwnedOutrightRanked
OutsideIrelandRankedIHA  <- SAPSTotalIHARanked2$OutsideIrelandRanked
UnskilledRankedIHA <-  SAPSTotalIHARanked2$UnskilledRanked 
ProfWorkersRankedIHA <- SAPSTotalIHARanked2$ProfWorkersRanked
MalesDisabilityRankedIHA <- SAPSTotalIHARanked2$MalesDisabilityRanked
FemalesDisabilityRankedIHA <- SAPSTotalIHARanked2$FemalesDisabilityRanked
MalesCarersRankedIHA <- SAPSTotalIHARanked2$MalesCarersRanked
FemalesCarersRankedIHA <- SAPSTotalIHARanked2$FemalesCarersRanked
VolunteersRankedIHA  <- SAPSTotalIHARanked2$VolunteersRanked
RenewableEnergyRankedIHA <- SAPSTotalIHARanked2$RenewableEnergyRanked 
OnFootBicycleRankedIHA <- SAPSTotalIHARanked2$OnFootBicycleRanked 
CarDriverRankedIHA <- SAPSTotalIHARanked2$CarDriverRanked 
ZeroTo14RankedIHA <- SAPSTotalIHARanked2$ZeroTo14Ranked 
SixtyFivePRankedIHA <- SAPSTotalIHARanked2$SixtyFivePRanked


PrimaryOrLowerRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$PrimaryOrLowerRanked == PrimaryOrLowerRankedIHA)>1,"Joint","Not Joint")
DegreeOrHigherRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$DegreeOrHigherRanked == DegreeOrHigherRankedIHA)>1,"Joint","Not Joint")
BadVBadHealthRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$BadVBadHealthRanked == BadVBadHealthRankedIHA)>1,"Joint","Not Joint")
UnemployedRankedIHAJoint  <- ifelse(sum(SAPSTotalIHARanked2$UnemployedRanked == UnemployedRankedIHA)>1,"Joint","Not Joint")
LoneParFamUnitRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$LoneParFamUnitRanked == LoneParFamUnitRankedIHA)>1,"Joint","Not Joint")
SmokingRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$SmokingRanked  == SmokingRankedIHA)>1,"Joint","Not Joint")
LARankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$LARanked  == LARankedIHA)>1,"Joint","Not Joint")
OwnedOutrightRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$OwnedOutrightRanked == OwnedOutrightRankedIHA)>1,"Joint","Not Joint")
OutsideIrelandRankedIHAJoint  <- ifelse(sum(SAPSTotalIHARanked2$OutsideIrelandRanked == OutsideIrelandRankedIHA)>1,"Joint","Not Joint")
UnskilledRankedIHAJoint <-  ifelse(sum(SAPSTotalIHARanked2$UnskilledRanked  == UnskilledRankedIHA)>1,"Joint","Not Joint")
ProfWorkersRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$ProfWorkersRanked == ProfWorkersRankedIHA)>1,"Joint","Not Joint")
MalesDisabilityRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$MalesDisabilityRanked == MalesDisabilityRankedIHA)>1,"Joint","Not Joint")
FemalesDisabilityRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$FemalesDisabilityRanked == FemalesDisabilityRankedIHA)>1,"Joint","Not Joint")
MalesCarersRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$MalesCarersRanked == MalesCarersRankedIHA)>1,"Joint","Not Joint")
FemalesCarersRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$FemalesCarersRanked == FemalesCarersRankedIHA)>1,"Joint","Not Joint")
VolunteersRankedIHAJoint  <- ifelse(sum(SAPSTotalIHARanked2$VolunteersRanked == VolunteersRankedIHA)>1,"Joint","Not Joint")
RenewableEnergyRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$RenewableEnergyRanked  == RenewableEnergyRankedIHA)>1,"Joint","Not Joint")
OnFootBicycleRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$OnFootBicycleRanked  == OnFootBicycleRankedIHA)>1,"Joint","Not Joint")
CarDriverRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$CarDriverRanked  == CarDriverRankedIHA)>1,"Joint","Not Joint")
ZeroTo14RankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$ZeroTo14Ranked  == ZeroTo14RankedIHA)>1,"Joint","Not Joint")
SixtyFivePRankedIHAJoint <- ifelse(sum(SAPSTotalIHARanked2$SixtyFivePRanked == SixtyFivePRankedIHA)>1,"Joint","Not Joint")



NumIHAs <- length(unique(SAPSHRLookup$IHA))
NumIHAHR <- length(unique(SAPSHRLookup$IHA[SAPSHRLookup$Region == AC]))

