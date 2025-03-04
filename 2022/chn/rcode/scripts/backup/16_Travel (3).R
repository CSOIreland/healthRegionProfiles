
# ED Table
TravelED <- TravelSourceTable%>%filter(Region == ED)

# Add population variable to Travel table so percentage of population can be calculated
TravelED$TotalPop <- TravelED$value[TravelED$Means.of.Travel == "Total"]

# Calculate percentage of population
TravelED$PercentageOfPopulation <- TravelED$value*100/TravelED$TotalPop

#State table
TravelState <- TravelSourceTable%>%filter(Region == "State")

# Total pop added so percentage of pop can be calculated
TravelState$TotalPop <- TravelState$value[TravelState$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelState$PercentageOfPopulation <- TravelState$value*100/TravelState$TotalPop

# AC table
TravelAC <- TravelSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

TravelAC$TotalPop <- TravelAC$value[TravelAC$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelAC$PercentageOfPopulation <- TravelAC$value*100/TravelAC$TotalPop


TravelIHA <- TravelSourceTable%>%filter(Region == IHA)

#Total pop added so perentage of population can be calculated

TravelIHA$TotalPop <- TravelIHA$value[TravelIHA$Means.of.Travel == "Total"]

# Calculate percentage of pop
TravelIHA$PercentageOfPopulation <- TravelIHA$value*100/TravelIHA$TotalPop

# Bind all tables for plot
TravelBinded <- bind_rows(TravelED,TravelIHA, TravelAC,TravelState)%>%filter(Means.of.Travel!="Total")
TravelBinded$Region <- factor(TravelBinded$Region, levels = c(ED,AC,IHA,"State"))

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
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Usually Resident Population")


# Highcharts plot for RMD
TravelBinded$PercentageOfPopulation <- round(TravelBinded$PercentageOfPopulation,1)
TravelBinded$colouract <- 1
TravelBinded$colouract[TravelBinded$Region == ED] <- '#405381'
TravelBinded$colouract[TravelBinded$Region == AC] <- '#FCBE72'
TravelBinded$colouract[TravelBinded$Region == "State"] <- '#13C1A5'
TravelBinded$colouract[TravelBinded$Region == IHA] <- '#90989f'

TravelPlot2 <- highchart()|>       
  hc_add_series(TravelBinded, "column", hcaes(x =Means.of.Travel2, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
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

TravelED <- as.data.frame(TravelED)
TravelAC<- as.data.frame(TravelAC)
TravelState <- as.data.frame(TravelState)

TravelIHA <- as.data.frame(TravelIHA)

# Reformat percentages so they are correct for tables
TravelState$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(TravelState$PercentageOfPopulation),1))
TravelED$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(TravelED$PercentageOfPopulation),1))
TravelAC$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(TravelAC$PercentageOfPopulation),1))
TravelIHA$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(TravelIHA$PercentageOfPopulation),1))

