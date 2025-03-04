
#ED subtables
GenEDMales <- GenWider%>%filter(Region == ED & Sex == "Males")
GenEDFemales <- GenWider%>%filter(Region == ED & Sex == "Females")
GenEDBothSexes<- GenWider%>%filter(Region == ED & Sex == "Both Sexes")

#State subtables
GenStateMales <- GenWider%>%filter(Region == "State" & Sex == "Males")
GenStateFemales <- GenWider%>%filter(Region == "State" & Sex == "Females")
GenStateBothSexes<- GenWider%>%filter(Region == "State" & Sex == "Both Sexes")

#IHA subtables
GenACMales <- GenWider%>%filter(Region == AC & Sex == "Males")
GenACFemales <- GenWider%>%filter(Region == AC & Sex == "Females")
GenACBothSexes<- GenWider%>%filter(Region == AC & Sex == "Both Sexes")
#IHA subtables
GenIHAMales <- GenWider%>%filter(Region == IHA & Sex == "Males")
GenIHAFemales <- GenWider%>%filter(Region == IHA & Sex == "Females")
GenIHABothSexes<- GenWider%>%filter(Region == IHA & Sex == "Both Sexes")

#long table ed males, including table without total for plot
GenLongEDMales <- GenSourceTable%>%filter(Region == ED & Sex == "Males")
GenLongEDMales$Percentage.Of.Population <- GenLongEDMales$value*100/GenLongEDMales$value[GenLongEDMales$General.Health == "Total"]
GenLongEDMales$General.Health <- factor(GenLongEDMales$General.Health, levels = unique(GenLongEDMales$General.Health))
GenLongEDMalesGraph <- GenLongEDMales%>%filter(General.Health!="Total")


#long table ed females, including table without total for plot
GenLongEDFemales <- GenSourceTable%>%filter(Region == ED & Sex == "Females")
GenLongEDFemales$Percentage.Of.Population <- GenLongEDFemales$value*100/GenLongEDFemales$value[GenLongEDFemales$General.Health == "Total"]
GenLongEDFemales$General.Health <- factor(GenLongEDFemales$General.Health, levels = unique(GenLongEDFemales$General.Health))
GenLongEDFemalesGraph <- GenLongEDFemales%>%filter(General.Health!="Total")


#long table ed both sexes, including table without total for plot
GenLongEDBothSexes<- GenSourceTable%>%filter(Region == ED & Sex == "Both Sexes")
GenLongEDBothSexes$Percentage.Of.Population <- GenLongEDBothSexes$value*100/GenLongEDBothSexes$value[GenLongEDBothSexes$General.Health == "Total"]
GenLongEDBothSexes$General.Health <- factor(GenLongEDBothSexes$General.Health, levels = unique(GenLongEDBothSexes$General.Health))
GenLongEDBothSexesGraph <- GenLongEDBothSexes%>%filter(General.Health!="Total")

#long table ed males, including table without total for plot
GenLongACMales <- GenSourceTable%>%filter(Region == AC & Sex == "Males")
GenLongACMales$Percentage.Of.Population <- GenLongACMales$value*100/GenLongACMales$value[GenLongACMales$General.Health == "Total"]
GenLongACMales$General.Health <- factor(GenLongACMales$General.Health, levels = unique(GenLongACMales$General.Health))
GenLongACMalesGraph <- GenLongACMales%>%filter(General.Health!="Total")


#long table AC females, including table without total for plot
GenLongACFemales <- GenSourceTable%>%filter(Region == AC & Sex == "Females")
GenLongACFemales$Percentage.Of.Population <- GenLongACFemales$value*100/GenLongACFemales$value[GenLongACFemales$General.Health == "Total"]
GenLongACFemales$General.Health <- factor(GenLongACFemales$General.Health, levels = unique(GenLongACFemales$General.Health))
GenLongACFemalesGraph <- GenLongACFemales%>%filter(General.Health!="Total")


#long table AC both sexes, including table without total for plot
GenLongACBothSexes<- GenSourceTable%>%filter(Region == AC & Sex == "Both Sexes")
GenLongACBothSexes$Percentage.Of.Population <- GenLongACBothSexes$value*100/GenLongACBothSexes$value[GenLongACBothSexes$General.Health == "Total"]
GenLongACBothSexes$General.Health <- factor(GenLongACBothSexes$General.Health, levels = unique(GenLongACBothSexes$General.Health))
GenLongACBothSexesGraph <- GenLongACBothSexes%>%filter(General.Health!="Total")

#long table state males, including table without total for plot
GenLongStateMales <- GenSourceTable%>%filter(Region == "State" & Sex == "Males")
GenLongStateMales$Percentage.Of.Population <- GenLongStateMales$value*100/GenLongStateMales$value[GenLongStateMales$General.Health == "Total"]
GenLongStateMales$General.Health <- factor(GenLongStateMales$General.Health, levels = unique(GenLongStateMales$General.Health))
GenLongStateMalesGraph <- GenLongStateMales%>%filter(General.Health!="Total")


#long table state females, including table without total for plot
GenLongStateFemales <- GenSourceTable%>%filter(Region == "State" & Sex == "Females")
GenLongStateFemales$Percentage.Of.Population <- GenLongStateFemales$value*100/GenLongStateFemales$value[GenLongStateFemales$General.Health == "Total"]
GenLongStateFemales$General.Health <- factor(GenLongStateFemales$General.Health, levels = unique(GenLongStateFemales$General.Health))
GenLongStateFemalesGraph <- GenLongStateFemales%>%filter(General.Health!="Total")


#long table state both sexes, including table without total for plot
GenLongStateBothSexes<- GenSourceTable%>%filter(Region == "State" & Sex == "Both Sexes")
GenLongStateBothSexes$Percentage.Of.Population <- GenLongStateBothSexes$value*100/GenLongStateBothSexes$value[GenLongStateBothSexes$General.Health == "Total"]
GenLongStateBothSexes$General.Health <- factor(GenLongStateBothSexes$General.Health, levels = unique(GenLongStateBothSexes$General.Health))
GenLongStateBothSexesGraph <- GenLongStateBothSexes%>%filter(General.Health!="Total")


#long table IHA males, including table without total for plot
GenLongIHAMales <- GenSourceTable%>%filter(Region == IHA & Sex == "Males")
GenLongIHAMales$Percentage.Of.Population <- GenLongIHAMales$value*100/GenLongIHAMales$value[GenLongIHAMales$General.Health == "Total"]
GenLongIHAMales$General.Health <- factor(GenLongIHAMales$General.Health, levels = unique(GenLongIHAMales$General.Health))
GenLongIHAMalesGraph <- GenLongIHAMales%>%filter(General.Health!="Total")


#long table IHA females, including table without total for plot
GenLongIHAFemales <- GenSourceTable%>%filter(Region == IHA & Sex == "Females")
GenLongIHAFemales$Percentage.Of.Population <- GenLongIHAFemales$value*100/GenLongIHAFemales$value[GenLongIHAFemales$General.Health == "Total"]
GenLongIHAFemales$General.Health <- factor(GenLongIHAFemales$General.Health, levels = unique(GenLongIHAFemales$General.Health))
GenLongIHAFemalesGraph <- GenLongIHAFemales%>%filter(General.Health!="Total")

#long table IHA both sexes, including table without total for plot
GenLongIHABothSexes<- GenSourceTable%>%filter(Region == IHA & Sex == "Both Sexes")
GenLongIHABothSexes$Percentage.Of.Population <- GenLongIHABothSexes$value*100/GenLongIHABothSexes$value[GenLongIHABothSexes$General.Health == "Total"]
GenLongIHABothSexes$General.Health <- factor(GenLongIHABothSexes$General.Health, levels = unique(GenLongIHABothSexes$General.Health))
GenLongIHABothSexesGraph <- GenLongIHABothSexes%>%filter(General.Health!="Total")

#bind long tables for plot
GenLongBinded <- bind_rows(GenLongEDBothSexesGraph,GenLongIHABothSexesGraph,GenLongStateBothSexesGraph,GenLongACBothSexesGraph)
GenLongBinded$Region <- factor(GenLongBinded$Region, levels = c(ED,IHA,AC,"State"))
#plot general health
GenPlot<- ggplot(GenLongBinded , aes(fill=Region, y=Percentage.Of.Population, x=General.Health)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.25, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_x_discrete(name = "General Health")+
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Population")


# Highcharts plot for RMD
GenLongBinded$Percentage.Of.Population <- round(GenLongBinded$Percentage.Of.Population,1)
GenLongBinded$colouract <- 1
GenLongBinded$colouract[GenLongBinded$Region == ED] <- '#405381'
GenLongBinded$colouract[GenLongBinded$Region == AC] <- '#FCBE72'
GenLongBinded$colouract[GenLongBinded$Region == "State"] <- '#13C1A5'
GenLongBinded$colouract[GenLongBinded$Region == IHA] <- '#90989f'
GenPlot2 <- highchart()|>       
  hc_add_series(GenLongBinded, "column", hcaes(x = General.Health, y = Percentage.Of.Population, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Population"))|>
  hc_xAxis(type = "category",
           title = list(text = "General Health"))


#export plot for latex
pdf(paste0(getwd(),"/figures/GenED.pdf"))
print(GenPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/GenED.svg"))
# print(GenPlot)
# dev.off()


# Reformat percentages to be correct for table
GenLongEDMales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDMales$Percentage.Of.Population,1))
GenLongEDFemales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDFemales$Percentage.Of.Population,1))
GenLongEDBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(GenLongEDBothSexes$Percentage.Of.Population,1))
GenLongStateMales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateMales$Percentage.Of.Population,1))
GenLongStateFemales$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateFemales$Percentage.Of.Population,1))
GenLongStateBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(GenLongStateBothSexes$Percentage.Of.Population,1))

GenLongACMales$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongACMales$Percentage.Of.Population),1))
GenLongACFemales$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongACFemales$Percentage.Of.Population),1))
GenLongACBothSexes$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongACBothSexes$Percentage.Of.Population),1))
GenLongIHAMales$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongIHAMales$Percentage.Of.Population),1))
GenLongIHAFemales$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongIHAFemales$Percentage.Of.Population),1))
GenLongIHABothSexes$Percentage.Of.Population <- sprintf("%.1f", round(as.numeric(GenLongIHABothSexes$Percentage.Of.Population),1))

