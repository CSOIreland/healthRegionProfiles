
#ED table
FamED <- FamSource%>%filter(Region == ED & Statistic == "Number of Families with Children" & Age.of.Child == "Total")

#Total families so percentage can be calculated
TotalFamiliesED <- sum(FamED$value)

#Calculate percentages
FamED$PercentageOfFamilies <- FamED$value*100/TotalFamiliesED

#Total families in ED
FamiliesED <- sum(FamED$value)

# Loner parents in ED and Lone parents as a percentage of all families
LoneParentsED <- sum(FamED$value[FamED$Type.of.Family == "Mothers with children"|  FamED$Type.of.Family == "Fathers with children"])
LoneParentsEDPerc <- sprintf("%.1f", round(sum(FamED$value[FamED$Type.of.Family == "Mothers with children"|  FamED$Type.of.Family == "Fathers with children"])*100/FamiliesED,1))

#families state table, total families and percentage of lone parents in ED as before
FamState <- FamSource%>%filter(Region == "State" & Statistic == "Number of Families with Children" & Age.of.Child == "Total")
TotalFamiliesState <- sum(FamState$value)
FamState$PercentageOfFamilies <- FamState$value*100/TotalFamiliesState
FamiliesState <- sum(FamState$value)
LoneParentsState <- sum(FamState$value[FamState$Type.of.Family == "Mothers with children"|  FamState$Type.of.Family == "Fathers with children"])
LoneParentsStatePerc <- sprintf("%.1f", round((sum(FamState$value[FamState$Type.of.Family == "Mothers with children"|  FamState$Type.of.Family == "Fathers with children"])*100/FamiliesState),1))

#families ac table, total families and percentage of lone parents in ED as before
FamSource$Region[FamSource$Region == "Dun Laoghaire  Rathdown County Council"] <- "Dun Laoghaire Rathdown County Council"
FamAC <- FamSource%>%filter(Region == AC & Statistic == "Number of Families with Children" & Age.of.Child == "Total")
TotalFamiliesAC <- sum(FamAC$value)
FamAC$PercentageOfFamilies <- FamAC$value*100/TotalFamiliesAC
FamiliesAC <- sum(FamAC$value)
LoneParentsAC <- sum(FamAC$value[FamAC$Type.of.Family == "Mothers with children"|  FamAC$Type.of.Family == "Fathers with children"])
LoneParentsACPerc <- sprintf("%.1f", round((sum(FamAC$value[FamAC$Type.of.Family == "Mothers with children"|  FamAC$Type.of.Family == "Fathers with children"])*100/FamiliesAC),1))

#families ac table, total families and percentage of lone parents in ED as before
FamSource$Region[FamSource$Region == "Dun Laoghaire  Rathdown County Council"] <- "Dun Laoghaire Rathdown County Council"
FamIHA <- FamSource%>%filter(Region == IHA & Statistic == "Number of Families with Children" & Age.of.Child == "Total")
TotalFamiliesIHA <- sum(FamIHA$value)
FamIHA$PercentageOfFamilies <- FamIHA$value*100/TotalFamiliesIHA
FamiliesIHA <- sum(FamIHA$value)
LoneParentsIHA <- sum(FamIHA$value[FamIHA$Type.of.Family == "Mothers with children"|  FamIHA$Type.of.Family == "Fathers with children"])
LoneParentsIHAPerc <- sprintf("%.1f", round((sum(FamIHA$value[FamIHA$Type.of.Family == "Mothers with children"|  FamIHA$Type.of.Family == "Fathers with children"])*100/FamiliesIHA),1))

# bind tables and percentage as numeric for plot
FamAll <- bind_rows(FamED,FamState,FamAC,FamIHA)
FamAll$Region <- factor(FamAll$Region, levels = c(ED,IHA,AC,"State"))
FamAll$PercentageOfFamilies <- as.numeric(FamAll$PercentageOfFamilies)

#plot
FamPlot  <- ggplot(FamAll , aes(fill=Region, y=PercentageOfFamilies, x=Type.of.Family)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 0.5, unit = "in")),
        axis.title.x = element_text(margin = margin(0.1, 0, 0.1, 0)),
        legend.position = c(0.85, 0.85))  +
  scale_fill_manual(values=c('#405381', '#13C1A5', '#FCBE72','#90989f'))+
  scale_x_discrete(name = "Type of Family")+
  labs(fill = "Legend") +
  scale_y_continuous(name = "% of Families")


# Highcharts plot for RMD
FamAll$PercentageOfFamilies <- round(FamAll$PercentageOfFamilies,1)
FamAll$colouract <- 1
FamAll$colouract[FamAll$Region == ED] <- '#405381'
FamAll$colouract[FamAll$Region == AC] <- '#FCBE72'
FamAll$colouract[FamAll$Region == "State"] <- '#13C1A5'
FamAll$colouract[FamAll$Region == IHA] <-'#90989f'

FamPlot2 <- highchart()|>       
  hc_add_series(FamAll, "column", hcaes(x = Type.of.Family, y = PercentageOfFamilies, color = colouract,  group = Region), color = c('#405381','#FCBE72','#13C1A5','#90989f'), showInLegend = T)|>
  hc_yAxis(
    title = list(text = "% of Families"))|>
  hc_xAxis(type = "category",
           title = list(text = "Type of Family"))


#export plot for latex
pdf(paste0(getwd(),"/figures/FamED.pdf"))
print(FamPlot)
dev.off()

# Export plot for RMD
# svg(paste0(getwd(),"/figures/FamED.svg"))
# print(FamPlot)
# dev.off()


#reformat percentages to be correct for tables
FamED$PercentageOfFamilies <- sprintf("%.1f", round(FamED$PercentageOfFamilies,1))
FamState$PercentageOfFamilies <- sprintf("%.1f", round(FamState$PercentageOfFamilies,1))
FamAC$PercentageOfFamilies <- sprintf("%.1f", round(FamAC$PercentageOfFamilies,1))
FamIHA$PercentageOfFamilies <- sprintf("%.1f", round(FamIHA$PercentageOfFamilies,1))

