# ED Table
SocialClassED <- SocialClassSourceTable%>%filter(Region == ED)

# Add population variable to SocialClass table so percentage of population can be calculated
SocialClassED$TotalPop <- SocialClassED$value[SocialClassED$Social.Class == "Total"]

# Calculate percentage of population
SocialClassED$PercentageOfPopulation <- SocialClassED$value*100/SocialClassED$TotalPop

#State table
SocialClassState <- SocialClassSourceTable%>%filter(Region == "State")

# Total pop added so percentage of pop can be calculated
SocialClassState$TotalPop <- SocialClassState$value[SocialClassState$Social.Class == "Total"]

# Calculate percentage of pop
SocialClassState$PercentageOfPopulation <- SocialClassState$value*100/SocialClassState$TotalPop

# AC table
SocialClassAC <- SocialClassSourceTable%>%filter(Region == AC)

#Total pop added so perentage of population can be calculated

SocialClassAC$TotalPop <- SocialClassAC$value[SocialClassAC$Social.Class == "Total"]

# Calculate percentage of pop
SocialClassAC$PercentageOfPopulation <- SocialClassAC$value*100/SocialClassAC$TotalPop
# IHA table
SocialClassIHA <- SocialClassSourceTable%>%filter(Region == IHA)

#Total pop added so perentage of population can be calculated

SocialClassIHA$TotalPop <- SocialClassIHA$value[SocialClassIHA$Social.Class == "Total"]

# Calculate percentage of pop
SocialClassIHA$PercentageOfPopulation <- SocialClassIHA$value*100/SocialClassIHA$TotalPop

# Bind all tables for plot
SocialClassBinded <- bind_rows(SocialClassED,SocialClassIHA,SocialClassAC, SocialClassState)%>%filter(Social.Class!="Total")
SocialClassBinded$Region <- factor(SocialClassBinded$Region, levels = c(ED,IHA,AC,"State"))
#wrap labels and factorise
SocialClassBinded$Social.Class2 <- stringr::str_wrap(SocialClassBinded$Social.Class, 25)
SocialClassBinded$Social.Class2 <- factor(SocialClassBinded$Social.Class2, levels = c("Professional workers", "Managerial and technical","Non-manual","Skilled manual", "Semi-skilled", "Unskilled","All others gainfully\noccupied and unknown"))


# Plot
SocialClassPlot <- ggplot(SocialClassBinded, aes(fill=Region, y=PercentageOfPopulation, x = Social.Class2)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(-0.1, 0, -0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  scale_x_discrete(name = "Social Class")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
SocialClassBinded$PercentageOfPopulation <- round(SocialClassBinded$PercentageOfPopulation,1)
SocialClassBinded$colouract <- 1
SocialClassBinded$colouract[SocialClassBinded$Region == ED] <- '#405381'
SocialClassBinded$colouract[SocialClassBinded$Region == AC] <- '#FCBE72'
SocialClassBinded$colouract[SocialClassBinded$Region == "State"] <- '#13C1A5'
SocialClassBinded$colouract[SocialClassBinded$Region == IHA] <-'#90989f'
SocialClassPlot2 <- highchart()|>       
  hc_add_series(SocialClassBinded, "column", hcaes(x = Social.Class2, y = PercentageOfPopulation, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "Social Class"))



#export plot for latex
pdf(paste0(getwd(),"/figures/SocialClassED.pdf"))
print(SocialClassPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/SocialClassED.svg"))
# print(SocialClassPlot)
# dev.off()
SocialClassED <- as.data.frame(SocialClassED)
SocialClassAC <- as.data.frame(SocialClassAC)
SocialClassState <- as.data.frame(SocialClassState)
SocialClassIHA <- as.data.frame(SocialClassIHA)

# Reformat percentages so they are correct for tables
SocialClassState$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(SocialClassState$PercentageOfPopulation),1))
SocialClassED$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(SocialClassED$PercentageOfPopulation),1))
SocialClassAC$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(SocialClassAC$PercentageOfPopulation),1))
SocialClassIHA$PercentageOfPopulation <- sprintf("%.1f", round(as.numeric(SocialClassIHA$PercentageOfPopulation),1))
