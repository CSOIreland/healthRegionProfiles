TravelSourceTable <- as.data.frame(TravelSourceTable)
# ED Table
TravelED <- TravelSourceTable%>%filter(Region == ED)

# Add population variable to Travel table so percentage of population can be calculated
TravelED$TotalPop <- TravelED$value[TravelED$Means.of.Travel == "Total"]

# Calculate percentage of population
TravelED$PercentageOfPopulation <- TravelED$value*100/TravelED$TotalPop

#State table
# TravelState <- TravelSourceTable%>%filter(Region == "State")
# 
# # Total pop added so percentage of pop can be calculated
# TravelState$TotalPop <- TravelState$value[TravelState$Means.of.Travel == "Total"]
# 
# # Calculate percentage of pop
# TravelState$PercentageOfPopulation <- TravelState$value*100/TravelState$TotalPop

# AC table
TravelSourceTable$Region[TravelSourceTable$Region == "Dun Laoghaire Rathdown County Council Rathdown County Council"] <- "Dun Laoghaire Rathdown County Council"

TravelAC <- TravelSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

TravelAC$TotalPop <- TravelAC$value[TravelAC$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelAC$PercentageOfPopulation <- TravelAC$value*100/TravelAC$TotalPop

# Bind all tables for plot
TravelBinded <- bind_rows(TravelED,TravelAC)%>%filter(Means.of.Travel!="Total")
TravelBinded$Region <- factor(TravelBinded$Region, levels = c(ED,AC))

#wrap labels and factorise
TravelBinded$Means.of.Travel2 <- stringr::str_wrap(TravelBinded$Means.of.Travel, 25)
TravelBinded$Means.of.Travel2 <- factor(TravelBinded$Means.of.Travel2, levels = c("On Foot", "Bicycle","Bus, minibus or coach","Train, DART or LUAS", "Motorcycle or scooter", "Car Driver","Car passenger","Van","Other (incl. lorry)","Work mainly at or from\nhome","Not stated"))


# Plot
TravelPlot <- ggplot(TravelBinded, aes(fill=Region, y=PercentageOfPopulation, x = Means.of.Travel2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_x_discrete(name = "Means of Travel")+
  scale_fill_manual(values=c('#405381', '#FCBE72'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Usually Resident Population")


# Highcharts plot for RMD
TravelBinded$PercentageOfPopulation <- round(TravelBinded$PercentageOfPopulation,1)
TravelBinded$colouract <- 1
TravelBinded$colouract[TravelBinded$Region == ED] <- '#405381'
TravelBinded$colouract[TravelBinded$Region == AC] <- '#FCBE72'
#TravelBinded$colouract[TravelBinded$Region == "State"] <- '#13C1A5'

TravelPlot2 <- highchart()|>       
  hc_add_series(TravelBinded, "column", hcaes(x =Means.of.Travel2, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Usually Resident Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "Means of Travel"))


#export plot for latex
pdf(paste0(getwd(),"/figures/TravelED.pdf"))
print(TravelPlot)
dev.off() 

# Export plot for RMD
# svg(paste0(getwd(),"/figures/TravelED.svg"))
# print(TravelPlot)
# dev.off()



# Reformat percentages so they are correct for tables
#TravelState$PercentageOfPopulation <- sprintf("%.1f", round(TravelState$PercentageOfPopulation,1))
TravelED$PercentageOfPopulation <- sprintf("%.1f", round(TravelED$PercentageOfPopulation,1))
TravelAC$PercentageOfPopulation <- sprintf("%.1f", round(TravelAC$PercentageOfPopulation,1))

