DisWider <- as.data.frame(DisWider)
# ED subtables
EDDisWiderAllSexes <- DisWider%>%filter(Region == ED & Sex == "Both Sexes")
EDDisWiderAllSexes$PercentageWithADisability <- EDDisWiderAllSexes$value*100/TotalPopEDBothSexes
EDDisWiderMale <- DisWider%>%filter(Region == ED & Sex == "Males")
EDDisWiderMale$PercentageWithADisability <- EDDisWiderMale$value*100/TotalPopEDMales
EDDisWiderFemale <- DisWider%>%filter(Region == ED & Sex == "Females")

EDDisWiderFemale$PercentageWithADisability <- EDDisWiderFemale$value*100/TotalPopEDFemales

#State subtables
# StateDisWiderAllSexes <- DisWider%>%filter(Region == "State" & Sex == "Both Sexes")
# StateDisWiderMale <- DisWider%>%filter(Region == "State" & Sex == "Male")
# StateDisWiderFemale <- DisWider%>%filter(Region == "State" & Sex == "Female")

# AC subtabels
ACDisWiderAllSexes <- DisWider%>%filter(Region == AC & Sex == "Both Sexes")
ACDisWiderAllSexes$PercentageWithADisability <- ACDisWiderAllSexes$value*100/TotalPopACBothSexes
ACDisWiderMale <- DisWider%>%filter(Region == AC & Sex == "Males")
ACDisWiderMale$PercentageWithADisability <- ACDisWiderMale$value*100/TotalPopACMales
ACDisWiderFemale <- DisWider%>%filter(Region == AC & Sex == "Females")

ACDisWiderFemale$PercentageWithADisability <- ACDisWiderFemale$value*100/TotalPopACFemales

StateDisWiderAllSexes <- DisWider%>%filter(Region == "State" & Sex == "Both Sexes")
StateDisWiderAllSexes$PercentageWithADisability <- StateDisWiderAllSexes$value*100/TotalPopStateBothSexes
StateDisWiderMale <- DisWider%>%filter(Region == "State" & Sex == "Males")
StateDisWiderMale$PercentageWithADisability <- StateDisWiderMale$value*100/TotalPopStateMales
StateDisWiderFemale <- DisWider%>%filter(Region == "State" & Sex == "Females")

StateDisWiderFemale$PercentageWithADisability <- StateDisWiderFemale$value*100/TotalPopStateFemales

#Recategorise AC age groups because they are different
# ACDisWiderAllSexesNewAges <- ACDisWiderAllSexes
# ACDisWiderAllSexesNewAges$Age.Group <- ifelse(ACDisWiderAllSexesNewAges$Age.Group == "0 - 4 years"|ACDisWiderAllSexesNewAges$Age.Group == "5 - 9 years"|ACDisWiderAllSexesNewAges$Age.Group =="10 - 14 years", "0 - 14 years" ,
#                                               ifelse(ACDisWiderAllSexesNewAges$Age.Group == "15 - 19 years"|ACDisWiderAllSexesNewAges$Age.Group == "20 - 24 years", "15 -24 years" ,
#                                                      ifelse(ACDisWiderAllSexesNewAges$Age.Group == "25 - 29 years"|ACDisWiderAllSexesNewAges$Age.Group == "30 - 34 years"|ACDisWiderAllSexesNewAges$Age.Group == "35 - 39 years"|ACDisWiderAllSexesNewAges$Age.Group == "40 - 44 years", "25 - 44 years" ,
#                                                             ifelse(ACDisWiderAllSexesNewAges$Age.Group == "45 - 49 years"|ACDisWiderAllSexesNewAges$Age.Group == "50 - 54 years"|ACDisWiderAllSexesNewAges$Age.Group == "55 - 59 years"|ACDisWiderAllSexesNewAges$Age.Group == "60 - 64 years", "45 - 64 years" ,
#                                                                    ifelse(ACDisWiderAllSexesNewAges$Age.Group == "65 - 69 years"|ACDisWiderAllSexesNewAges$Age.Group == "70 - 74 years"|ACDisWiderAllSexesNewAges$Age.Group == "75 - 79 years"|ACDisWiderAllSexesNewAges$Age.Group == "80 - 84 years"|ACDisWiderAllSexesNewAges$Age.Group == "85 years and over", "65 years and over" ,
#                                                                           ifelse(ACDisWiderAllSexesNewAges$Age.Group == "All ages", "All ages","Missing"))))))
# 
# 
# #regroup by new categories and summarise
# ACDisWiderAllSexesNewAges <- ACDisWiderAllSexesNewAges%>%group_by(Region, Age.Group, Sex, CensusYear)%>%dplyr::summarise( `Population with any disability` = sum(`Population with any disability`, na.rm=T),  `Population with a disability to a great extent` = sum(`Population with a disability to a great extent`, na.rm=T), `Population with a disability to some extent` = sum(`Population with a disability to some extent`, na.rm=T), `Population with a disability as a percentage` = sum(`Population with a disability as a percentage`, na.rm=T), `Population with a disability to a great extent as a percentage` = sum(`Population with a disability to a great extent as a percentage`, na.rm=T), `Population with a disability to some extent as a percentage` = sum(`Population with a disability to some extent as a percentage`, na.rm=T))
# 
# #add total pop so percentages can ve calculated
# ACDisWiderAllSexesNewAges$TotalPop <- PopACBothSexes$value
# 
# #calculate percentage
# ACDisWiderAllSexesNewAges$`Population with a disability as a percentage` <- ACDisWiderAllSexesNewAges$`Population with any disability`*100/ACDisWiderAllSexesNewAges$TotalPop

# bind ED,  AC and State for plot
DisBinded <- bind_rows(EDDisWiderAllSexes,StateDisWiderAllSexes,ACDisWiderAllSexes)
DisBinded$Region <- factor(DisBinded$Region, levels = c(ED,AC,"State"))
#plot disability
DisPlot<- ggplot(DisBinded , aes(fill=Region, y=PercentageWithADisability , x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72'))+
  scale_x_discrete(name = "Age Group")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
DisBinded$PercentageWithADisability <- round(DisBinded$PercentageWithADisability,1)
DisBinded$colouract <- 1
DisBinded$colouract[DisBinded$Region == ED] <- '#405381'
DisBinded$colouract[DisBinded$Region == AC] <- '#FCBE72'
DisBinded$colouract[DisBinded$Region == "State"] <- '#13C1A5'

DisPlot2 <- highchart()|>       
  hc_add_series(DisBinded, "column", hcaes(x = Region, y = PercentageWithADisability, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "Age Group"))


#export plot for latex
pdf(paste0(getwd(),"/figures/DisED.pdf"))
print(DisPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/DisED.svg"))
# print(DisPlot)
# dev.off()


# reformat percentages so they are correct for table

#ACDisWiderAllSexesNewAges$`Population with a disability as a percentage` <- sprintf("%.1f", round(ACDisWiderAllSexesNewAges$`Population with a disability as a percentage`,1))


EDDisWiderAllSexes$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderAllSexes$PercentageWithADisability,1)) 
# EDDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
# EDDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

EDDisWiderMale$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderMale$PercentageWithADisability,1)) 
# EDDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
# EDDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


EDDisWiderFemale$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderFemale$PercentageWithADisability,1)) 
# EDDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(EDDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
# EDDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(EDDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 

ACDisWiderAllSexes$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderAllSexes$PercentageWithADisability,1)) 
# ACDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
# ACDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

ACDisWiderMale$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderMale$PercentageWithADisability,1)) 
# ACDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
# ACDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


ACDisWiderFemale$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderFemale$PercentageWithADisability,1)) 
# ACDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(ACDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
# ACDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(ACDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 

StateDisWiderAllSexes$PercentageWithADisability <- sprintf("%.1f", round(StateDisWiderAllSexes$PercentageWithADisability,1)) 
# StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 

StateDisWiderMale$PercentageWithADisability <- sprintf("%.1f", round(StateDisWiderMale$PercentageWithADisability,1)) 
# StateDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 


StateDisWiderFemale$PercentageWithADisability <- sprintf("%.1f", round(StateDisWiderFemale$PercentageWithADisability,1)) 
# StateDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 
