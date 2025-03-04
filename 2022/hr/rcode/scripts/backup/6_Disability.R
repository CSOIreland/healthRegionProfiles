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

#Recategorise AC age groups because they are different

# bind ED,  AC and State for plot
DisBinded <- bind_rows(EDDisWiderAllSexes,ACDisWiderAllSexes)
DisBinded$Region <- factor(DisBinded$Region, levels = c(ED,AC))
#plot disability
DisPlot<- ggplot(DisBinded , aes(fill = Region, y=PercentageWithADisability , x=Region)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)))  +
  scale_fill_manual(values=c('#405381', '#FCBE72'))+
  scale_x_discrete(name = "Age Group")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
DisBinded$PercentageWithADisability <- round(DisBinded$PercentageWithADisability,1)
DisBinded$colouract <- 1
DisBinded$colouract[DisBinded$Region == ED] <- '#405381'
DisBinded$colouract[DisBinded$Region == AC] <- '#FCBE72'
#DisBinded$colouract[DisBinded$Region == "State"] <- '#13C1A5'

DisPlot2 <- highchart()|>       
  hc_add_series(DisBinded, "column", hcaes(x = Region, y = PercentageWithADisability, color = colouract,  group = Region), color = c('#405381','#FCBE72'), showInLegend = T)|>
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

EDDisWiderAllSexes$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderAllSexes$PercentageWithADisability,1)) 
EDDisWiderMale$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderMale$PercentageWithADisability,1)) 

EDDisWiderFemale$PercentageWithADisability <- sprintf("%.1f", round(EDDisWiderFemale$PercentageWithADisability,1)) 
ACDisWiderAllSexes$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderAllSexes$PercentageWithADisability,1)) 

ACDisWiderMale$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderMale$PercentageWithADisability,1)) 


ACDisWiderFemale$PercentageWithADisability <- sprintf("%.1f", round(ACDisWiderFemale$PercentageWithADisability,1)) 

# StateDisWiderAllSexes$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability as a percentage`,1)) 
# StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderAllSexes$`Population with a disability to some extent as a percentage`,1)) 
# 
# StateDisWiderMale$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability as a percentage`,1)) 
# StateDisWiderMale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderMale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderMale$`Population with a disability to some extent as a percentage`,1)) 
# 
# 
# StateDisWiderFemale$`Population with a disability as a percentage` <- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability as a percentage`,1)) 
# StateDisWiderFemale$`Population with a disability to a great extent as a percentage` <- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to a great extent as a percentage`,1)) 
# StateDisWiderFemale$`Population with a disability to some extent as a percentage`<- sprintf("%.1f", round(StateDisWiderFemale$`Population with a disability to some extent as a percentage`,1)) 

