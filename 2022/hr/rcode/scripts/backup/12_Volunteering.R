VolunteersSourceTable <- as.data.frame(VolunteersSourceTable)
# ED Table
VolunteersED <- VolunteersSourceTable%>%filter(Region == ED)

# Add population variable to Volunteers table so percentage of population can be calculated
VolunteersED$TotalPop <- TotalPopEDBothSexes

# Calculate percentage of population
VolunteersED$PercentageOfPopulation <- VolunteersED$value*100/VolunteersED$TotalPop

#State table
# VolunteersState <- VolunteersSourceTable%>%filter(Region == "State")
# 
# # Total pop added so percentage of pop can be calculated
# VolunteersState$TotalPop <- TotalPopStateBothSexes
# 
# # Calculate percentage of pop
# VolunteersState$PercentageOfPopulation <- VolunteersState$value*100/VolunteersState$TotalPop

# AC table
VolunteersAC <- VolunteersSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

VolunteersAC$TotalPop <- TotalPopACBothSexes

# Calculate percentage of pop
VolunteersAC$PercentageOfPopulation <- VolunteersAC$value*100/VolunteersAC$TotalPop

# Bind all tables for plot
VolunteersBinded <- bind_rows(VolunteersED,VolunteersAC)
VolunteersBinded$Region <- factor(VolunteersBinded$Region, levels = c(ED,AC))
# Plot
VolunteerPlot <- ggplot(VolunteersBinded, aes(fill=Region, y=PercentageOfPopulation, x = Region)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values=c('#405381', '#FCBE72'))+
  scale_x_discrete(name = "Region")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
VolunteersBinded$PercentageOfPopulation <- round(VolunteersBinded$PercentageOfPopulation,1)
VolunteersBinded$colouract <- 1
VolunteersBinded$colouract[VolunteersBinded$Region == ED] <- '#405381'
VolunteersBinded$colouract[VolunteersBinded$Region == AC] <- '#FCBE72'
#VolunteersBinded$colouract[VolunteersBinded$Region == "State"] <- '#13C1A5'

VolunteerPlot2 <- highchart()|>       
  hc_add_series(VolunteersBinded, "column", hcaes(x = Region, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "Region"))


#export plot for latex
pdf(paste0(getwd(),"/figures/VolunteerED.pdf"))
print(VolunteerPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/VolunteerED.svg"))
# print(VolunteerPlot)
# dev.off()


# Reformat percentages so they are correct for tables
#VolunteersState$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersState$PercentageOfPopulation,1))
VolunteersED$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersED$PercentageOfPopulation,1))
VolunteersAC$PercentageOfPopulation <- sprintf("%.1f", round(VolunteersAC$PercentageOfPopulation,1))
