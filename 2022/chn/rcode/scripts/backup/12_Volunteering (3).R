# ED Table
VolunteersED <- VolunteersSourceTable%>%filter(Region == ED)

# Add population variable to Volunteers table so percentage of population can be calculated
VolunteersED$TotalPop <- TotalPopEDBothSexes

# Calculate percentage of population
VolunteersED$PercentageOfPopulation <- VolunteersED$value*100/VolunteersED$TotalPop

#State table
VolunteersState <- VolunteersSourceTable%>%filter(Region == "State")

# Total pop added so percentage of pop can be calculated
VolunteersState$TotalPop <- TotalPopStateBothSexes

# Calculate percentage of pop
VolunteersState$PercentageOfPopulation <- VolunteersState$value*100/VolunteersState$TotalPop

# AC table
VolunteersAC <- VolunteersSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

VolunteersAC$TotalPop <- TotalPopACBothSexes

# Calculate percentage of pop
VolunteersAC$PercentageOfPopulation <- VolunteersAC$value*100/VolunteersAC$TotalPop
# AC table
VolunteersIHA <- VolunteersSourceTable%>%filter(Region == IHA)

#Total pop added so perentage of population can be calculated

VolunteersIHA$TotalPop <- TotalPopIHABothSexes

# Calculate percentage of pop
VolunteersIHA$PercentageOfPopulation <- VolunteersIHA$value*100/VolunteersIHA$TotalPop

# Bind all tables for plot
VolunteersBinded <- bind_rows(VolunteersED,VolunteersIHA,VolunteersAC, VolunteersState)
VolunteersBinded$Region <- factor(VolunteersBinded$Region, levels = c(ED,IHA,AC,"State"))
# Plot
VolunteerPlot <- ggplot(VolunteersBinded, aes(fill=Region, y=PercentageOfPopulation, x = Region)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  scale_x_discrete(name = "Region")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
VolunteersBinded$PercentageOfPopulation <- round(VolunteersBinded$PercentageOfPopulation,1)
VolunteersBinded$colouract <- 1
VolunteersBinded$colouract[VolunteersBinded$Region == ED] <- '#405381'
VolunteersBinded$colouract[VolunteersBinded$Region == AC] <- '#FCBE72'
VolunteersBinded$colouract[VolunteersBinded$Region == "State"] <- '#13C1A5'
VolunteersBinded$colouract[VolunteersBinded$Region == IHA] <-'#90989f'
VolunteerPlot2 <- highchart()|>       
  hc_add_series(VolunteersBinded, "column", hcaes(x = Region, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
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

VolunteersED <- as.data.frame(VolunteersED)

VolunteersAC <- as.data.frame(VolunteersAC)

VolunteersState <- as.data.frame(VolunteersState)%>%filter(!duplicated(Region))
VolunteersIHA <- as.data.frame(VolunteersIHA)
# Reformat percentages so they are correct for tables
VolunteersState$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(VolunteersState$PercentageOfPopulation),1))
VolunteersED$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(VolunteersED$PercentageOfPopulation),1))
VolunteersAC$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(VolunteersAC$PercentageOfPopulation),1))
VolunteersIHA$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(VolunteersIHA$PercentageOfPopulation),1))
