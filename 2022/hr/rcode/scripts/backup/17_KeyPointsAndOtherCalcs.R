#Area Stats


ED<- as.character(ED)
# AreaED <- Area%>%filter(Electoral.Divisions == ED)
# EDPopDens <- AreaED$value[AreaED$Statistic == "Population density (persons per sq km)"]
# EDArea <- AreaED$value[AreaED$Statistic == "Area (sq km)"]
# AreaState <- Area%>%filter(Electoral.Divisions == "Ireland")
# StatePopDens <- AreaState$value[AreaState$Statistic == "Population density (persons per sq km)"]
# StateArea <- AreaState$value[AreaState$Statistic == "Area (sq km)"]

#families

EDFamiliesInPrivateHouseholds <- FamiliesInPrivateHouseholds$value[FamiliesInPrivateHouseholds$Household.Size == "Total" & FamiliesInPrivateHouseholds$Region == ED & FamiliesInPrivateHouseholds$Statistic == "Number of families in private households"]
StateFamiliesInPrivateHouseholds <- FamiliesInPrivateHouseholds$value[FamiliesInPrivateHouseholds$Household.Size == "Total" & FamiliesInPrivateHouseholds$Region == "State" & FamiliesInPrivateHouseholds$Statistic == "Number of families in private households"]

#age and dependency ratio
EDAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependency <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)
EDYouthDependency <- round((EDAged0to14)*100/EDAged1564,1)
EDOldAgeDependency <- round((EDAged65P)*100/EDAged1564,1)

EDAged0to14Fem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65PFem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564Fem <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Females" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotalFem <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Females" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependencyFem <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)

EDAged0to14Mal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "0-14"]
EDAged65PMal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "65 years and over"]
EDAged1564Mal <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Males" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
EDAgeTotalMal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == ED & PopSourceTableRegrouped$Sex == "Males" & PopSourceTableRegrouped$Age == "Total"]
EDAgeDependencyMal <- round((EDAged0to14+EDAged65P)*100/EDAged1564,1)

EDAged0to14Perc <- sprintf("%.1f", round(EDAged0to14*100/TotalPopEDBothSexes,1))
EDAged15to64Perc <- sprintf("%.1f", round(EDAged1564*100/TotalPopEDBothSexes,1))
EDAged65PPerc <- sprintf("%.1f", round(EDAged65P*100/TotalPopEDBothSexes,1))

EDAged0to14PercMal <- sprintf("%.1f", round(EDAged0to14Mal*100/TotalPopEDMales,1))
EDAged15to64PercMal <- sprintf("%.1f", round(EDAged1564Mal*100/TotalPopEDMales,1))
EDAged65PPercMal <- sprintf("%.1f", round(EDAged65PMal*100/TotalPopEDMales,1))

EDAged0to14PercFem <- sprintf("%.1f", round(EDAged0to14Fem*100/TotalPopEDFemales,1))
EDAged15to64PercFem <- sprintf("%.1f", round(EDAged1564Fem*100/TotalPopEDFemales,1))
EDAged65PPercFem <- sprintf("%.1f", round(EDAged65PFem*100/TotalPopEDFemales,1))



ACAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == AC & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
ACAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
ACAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
ACAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == AC  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
ACAgeDependency <- round((ACAged0to14+ACAged65P)*100/ACAged1564,1)
ACYouthDependency <- round((ACAged0to14)*100/ACAged1564,1)
ACOldAgeDependency <- round((ACAged65P)*100/ACAged1564,1)

ACAged0to14Perc <- sprintf("%.1f", round(ACAged0to14*100/TotalPopACBothSexes,1))
ACAged15to64Perc <- sprintf("%.1f", round(ACAged1564*100/TotalPopACBothSexes,1))
ACAged65PPerc <- sprintf("%.1f", round(ACAged65P*100/TotalPopACBothSexes,1))


ACAged0to14Perc <- sprintf("%.1f", round(ACAged0to14*100/TotalPopACBothSexes,1))
ACAged15to64Perc <- sprintf("%.1f", round(ACAged1564*100/TotalPopACBothSexes,1))
ACAged65PPerc <- sprintf("%.1f", round(ACAged65P*100/TotalPopACBothSexes,1))

#disabilit
StateAged0to14 <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == "AC" & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "0-14"]
StateAged65P <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == "AC"  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "65 years and over"]
StateAged1564 <- sum(PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == "AC"  & PopSourceTableRegrouped$Sex == "Both Sexes" & (PopSourceTableRegrouped$Age == "15-24" |PopSourceTableRegrouped$Age == "25-44" |PopSourceTableRegrouped$Age == "45-64" )])
StateAgeTotal <- PopSourceTableRegrouped$value[PopSourceTableRegrouped$Region == "AC"  & PopSourceTableRegrouped$Sex == "Both Sexes" & PopSourceTableRegrouped$Age == "Total"]
StateAgeDependency <- round((ACAged0to14+ACAged65P)*100/ACAged1564,1)
StateYouthDependency <- round((ACAged0to14)*100/ACAged1564,1)
StateOldAgeDependency <- round((ACAged65P)*100/ACAged1564,1)

PopDisabledED <-  EDDisWiderAllSexes$value 
PopDisabledPercED <- EDDisWiderAllSexes$PercentageWithADisability
PopDisabledState <- ACDisWiderAllSexes$value
PopDisabledPercState <-ACDisWiderAllSexes$PercentageWithADisability

TotalCarersED <- CarersED$value[CarersED$Sex == "Both Sexes"]
PercCarersED <- round(as.numeric(CarersED$PercentageOfPopulation)[CarersED$Sex == "Both Sexes"],1)

TotalCarersState <- CarersAC$value[CarersAC$Sex == "Both Sexes"]
PercCarersState <- round(as.numeric(CarersAC$PercentageOfPopulation)[CarersAC$Sex == "Both Sexes"],1)

#health
BadVBadED <- sum(GenLongEDBothSexes$value[GenLongEDBothSexes$General.Health == "Bad" |GenLongEDBothSexes$General.Health == "Very Bad" ])
BadVBadEDPerc <- round(sum(as.numeric(GenLongEDBothSexes$Percentage.Of.Population)[GenLongEDBothSexes$General.Health == "Bad" |GenLongEDBothSexes$General.Health == "Very Bad" ]),1)


BadVBadState <- sum(GenLongACBothSexes$value[GenLongACBothSexes$General.Health == "Bad" |GenLongACBothSexes$General.Health == "Very Bad" ])
BadVBadStatePerc <- round(sum(as.numeric(GenLongACBothSexes$Percentage.Of.Population)[GenLongACBothSexes$General.Health == "Bad" |GenLongACBothSexes$General.Health == "Very Bad" ]),1)

#Education
LowerEduEDPerc <- as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "No formal education"]) +as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "Primary education"]) +as.numeric(EduEDTable$PercentageOfPopulation[EduEDTable$Highest.Level.of.Education.Completed == "Lower secondary"])
LowerEduStatePerc <- as.numeric(EduACTable$PercentageOfPopulation[EduACTable$Highest.Level.of.Education.Completed == "No formal education"]) +as.numeric(EduACTable$PercentageOfPopulation[EduACTable$Highest.Level.of.Education.Completed == "Primary education"]) +as.numeric(EduACTable$PercentageOfPopulation[EduACTable$Highest.Level.of.Education.Completed == "Lower secondary"])

#Birthplace
OIEDPerc <- sum(as.numeric(BirthED$PercentageOfPopulation[BirthED$Location!="Ireland" & BirthED$Location!= "Not stated"& BirthED$Location!= "Total"]))
OIStatePerc <- sum(as.numeric(BirthAC$PercentageOfPopulation[BirthAC$Location!="Ireland" & BirthAC$Location!= "Not ACd"& BirthAC$Location!= "Total"]))
                
                
#Occupancy

