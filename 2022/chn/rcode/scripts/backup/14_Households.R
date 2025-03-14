
# ED Table
HouseholdsED <- HouseholdsSourceTable%>%filter(Region == ED)

# Add population variable to Households table so percentage of population can be calculated
HouseholdsED$TotalPop <- HouseholdsED$value[HouseholdsED$Type.of.Occupancy == "Total"]

# Calculate percentage of population
HouseholdsED$PercentageOfPopulation <- HouseholdsED$value*100/HouseholdsED$TotalPop

#State table
HouseholdsState <- HouseholdsSourceTable%>%filter(Region == "State")

# Total pop added so percentage of pop can be calculated
HouseholdsState$TotalPop <- HouseholdsState$value[HouseholdsState$Type.of.Occupancy == "Total"]

# Calculate percentage of pop
HouseholdsState$PercentageOfPopulation <- HouseholdsState$value*100/HouseholdsState$TotalPop

# AC table
HouseholdsAC <- HouseholdsSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

HouseholdsAC$TotalPop <- HouseholdsAC$value[HouseholdsAC$Type.of.Occupancy == "Total"]

# Calculate percentage of pop
HouseholdsAC$PercentageOfPopulation <- HouseholdsAC$value*100/HouseholdsAC$TotalPop
# IHA table
HouseholdsIHA <- HouseholdsSourceTable%>%filter(Region == IHA)

#Total pop added so perentage of population can be calculated

HouseholdsIHA$TotalPop <- HouseholdsIHA$value[HouseholdsIHA$Type.of.Occupancy == "Total"]

# Calculate percentage of pop
HouseholdsIHA$PercentageOfPopulation <- HouseholdsIHA$value*100/HouseholdsIHA$TotalPop

# Bind all tables for plot
HouseholdsBinded <- bind_rows(HouseholdsED,HouseholdsIHA, HouseholdsAC, HouseholdsState)%>%filter(Type.of.Occupancy!="Total")
HouseholdsBinded$Region <- factor(HouseholdsBinded$Region, levels = c(ED,IHA,AC,"State"))


#wrap labels and factorise
HouseholdsBinded$Type.of.Occupancy2 <- stringr::str_wrap(HouseholdsBinded$Type.of.Occupancy, 25)
HouseholdsBinded$Type.of.Occupancy2 <- factor(HouseholdsBinded$Type.of.Occupancy2, levels = c("Owned with mortgage or\nloan", "Owned outright","Rented from private\nlandlord","Rented from Local\nAuthority", "Rented from\nvoluntary/co-operative\nhousing body", "Occupied free of rent","Not stated"))


# Plot
HouseholdsPlot <- ggplot(HouseholdsBinded, aes(fill=Region, y=PercentageOfPopulation, x = Type.of.Occupancy2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-5, 0, -5, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  scale_x_discrete(name = "Type of Occupancy")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Households")


# Highcharts plot for RMD
HouseholdsBinded$PercentageOfPopulation <- round(HouseholdsBinded$PercentageOfPopulation,1)
HouseholdsBinded$colouract <- 1
HouseholdsBinded$colouract[HouseholdsBinded$Region == ED] <- '#405381'
HouseholdsBinded$colouract[HouseholdsBinded$Region == AC] <- '#FCBE72'
HouseholdsBinded$colouract[HouseholdsBinded$Region == "State"] <- '#13C1A5'
HouseholdsBinded$colouract[HouseholdsBinded$Region == IHA] <- '#90989f'
HouseholdsPlot2 <- highchart()|>       
  hc_add_series(HouseholdsBinded, "column", hcaes(x = Type.of.Occupancy2, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Households"))|>
  hc_xAxis(type = "category",
           title = list(text = "Type of Occupancy"))


#export plot for latex
pdf(paste0(getwd(),"/figures/HouseholdsED.pdf"))
print(HouseholdsPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/HouseholdsED.svg"))
# print(HouseholdsPlot)
# dev.off()

HouseholdsED <- as.data.frame(HouseholdsED)
HouseholdsAC <- as.data.frame(HouseholdsAC)
HouseholdsState <- as.data.frame(HouseholdsState)
HouseholdsIHA <- as.data.frame(HouseholdsIHA)
# Reformat percentages so they are correct for tables
HouseholdsState$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(HouseholdsState$PercentageOfPopulation),1))
HouseholdsED$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(HouseholdsED$PercentageOfPopulation),1))
HouseholdsAC$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(HouseholdsAC$PercentageOfPopulation),1))
HouseholdsIHA$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(HouseholdsIHA$PercentageOfPopulation),1))
