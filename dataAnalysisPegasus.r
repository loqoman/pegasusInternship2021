# agtronRawData Analysis on Pegasus Roastary EVN and Agtron converted values for the months of april through June.
# By Darwin Clark for Pegasus Roastary

# TODO: Indexing function for the xml (List the number of roasts, the date of the roast and the type of bean)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(lubridate)

library(argparse)

library(xml2)
library(purrr)

library(plyr) # Used to fix the lists out of XML world
library(dplyr) # Used to access group_by and get agtronRawDatabase statistics 

# TODO: Rename this variable to somethnig meaningful
agtronRawData = read.csv("~/Programming/pegasusInternship2021/data/roastaryRoastData0819.csv")
agtronRawData$Date = as.Date(agtronRawData$Date, format="%y.%m.%d") # Fix the dates in the d
agtronRawData$Time = as.numeric(hm(agtronRawData$Time)) # Converts the character time's to numerical time's

xmlagtronRawData = read_xml("~/Programming/pegasusInternship2021/data/For_darwin.xml")
# Setting up colors
# ====================== agtronRawData FORMATTING =========================== #
countByBlend = agtronRawData %>% group_by(Blend) %>% summarize(count=n()) # Simple table with a count of each blend

# ----- OPTIONAL, culling blends with less than a certain number of samples -----
for(i in seq_len(nrow(countByBlend))){
  # print(paste("This year is", df1$x[i], df1$y[i], "color", df1$z[i]))
  if (countByBlend$count[i] < 15) {
    agtronRawData = subset(agtronRawData, Blend != countByBlend$Blend[i])
  }
} 


# ====================== START OF GRAPHING ========================== #
# Boxplots

# aes(fill = ifelse(countByBlend[grep(agtronRawData$Blend, countByBlend$Blend),][[2]] > 15, "darkcyan", "red"))
# TODO: Add coloring based off of sample size
ggplot(agtronRawData, aes(x = agtronRawData$Blend, y = agtronRawData$Agtron.Converted)) +
  geom_boxplot(aes(color=agtronRawData$Blend)) +
  xlab("Blend Variety") + ylab("Agtron Converted Value") + labs(title = "Box Plots Comparing Agtron Converted Values of Different Roast Blends") +
  theme(legend.position = "non")

summary(agtronRawData$Agtron.Converted)

tempMaxDates = agtronRawData %>% group_by(Blend) %>% filter(Date == max(Date))

# Labeled line plot 
ggplot(agtronRawData, aes(x = agtronRawData$Date, y = agtronRawData$Agtron.Converted)) +
  geom_line(aes(group=agtronRawData$Blend, color=agtronRawData$Blend), size = 1.1) +
  #geom_point(aes(color=agtronRawData$Blend)) +
  geom_label_repel(aes(label=ifelse(Date>as.Date("2021-05-20"), Blend, ''), fill=agtronRawData$Blend), nudge_x=1, na.rm = TRUE,
                   fontface = 'bold', color = 'white',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = 'grey50') +
  xlab("Date") + ylab("Agtron Converted Value") + labs(title = "Line Plot Comparing Date of Roast to Agtron Converted Value") 

  
# Unlabeled line plot, colored by blend
ggplot(agtronRawData) +
  geom_line(aes(x = agtronRawData$Time, y = agtronRawData$Agtron.Converted, group=agtronRawData$Blend, color=agtronRawData$Blend), size = 1.5) +
  #geom_point(aes(color=agtronRawData$Blend)) +
  xlab("Time") + ylab("Agtron Converted Value") + labs(title = "Line Plot Comparing Time of Roast to Agtron Converted Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))

# ============================= XML 2 ============================= #
masterXMLagtronRawDataFrame = data.frame(matrix(vector(), 28, 3)) #agtronRawData.frame(matrix(vector(), 28, 3, dimnames=list(c(), c("Date", "File", "User"))), stringsAsFactors=F)

xml_attrs(xmlagtronRawData, "RoastmasterEntities")
    
xml_children(xmlagtronRawData)[2]["Roast"]

xml_find_all(xmlagtronRawData, "RoastmasterXML/RoastmasterEntities/Roast")

# What I'll likely do is find all the roasts FIRST, then pull from the proper thermocouple in each roast to create a agtronRawData.frame() object
rawNodes = xml_find_all(xmlagtronRawData, "//Node") # Will select every "Node" element in the Document
rawRoasts = xml_find_all(xmlagtronRawData, "//Roast") # Will select every "Roast" element in the Document

rawagtronRawDataCurves = xml_find_all(xmlagtronRawData, "//Curve") # Will select every "Curve" element in the Document


# ===== Testing how to get the first (of 4) "curve" agtronRawDatasets for each roast ===== # 

firstTestagtronRawDataCurve = xml_find_all(xmlagtronRawData, "//agtronRawDataCurves")[1]
secondTestagtronRawDataCurve = xml_find_all(xmlagtronRawData, "//agtronRawDataCurves")[2]

# Now we have the 4 'curve' objects fr each of the 2 roasts
# now want to match for each of the thermocouples
# N.B The matching is based off of a RegEX system called XPath

xml_children(rawagtronRawDataCurves)
xml_attrs(rawagtronRawDataCurves)
xml_text(rawagtronRawDataCurves)

# These are each individual curve objects
roast1AMBT = rawagtronRawDataCurves[1] # Pick the first agtronRawDataCurve (arbitraryily)
roast1AMBT = xml_find_all(roast1AMBT, ".//Node")  # Get all the "node" children within this roast 

roast2AMBT = rawagtronRawDataCurves[9] # Pick the NEXT AMBT agtronRawDataCurve (arbitraryily)
roast2AMBT = xml_find_all(roast2AMBT, ".//Node")

roast1BT = rawagtronRawDataCurves[2]
roast1BT = xml_find_all(roast1BT, ".//Node")  # Get all the "node" children within this roast 
  
roast2BT = rawagtronRawDataCurves[10]
roast2BT = xml_find_all(roast2BT, ".//Node")  # Get all the "node" children within this roast 

masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time = laply(laply(as_list(xml_find_all(roast1AMBT, ".//Time")),identity), identity) # REMEMBER SUBSET FUNCTION IS []
masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1agtronRawData = as.numeric(laply(laply(as_list(xml_find_all(roast1AMBT, ".//Level")), identity), identity)) # REMEMBER SUBSET FUNCTION IS []

masterXMLagtronRawDataFrame$roas2agtronRawDataCurve1agtronRawData = as.numeric(laply(laply(as_list(xml_find_all(roast2AMBT, ".//Level")), identity), identity)[-c(29, 30)]) # REMEMBER SUBSET FUNCTION IS []

masterXMLagtronRawDataFrame$roast1BTagtronRawData = as.numeric((laply(laply(as_list(xml_find_all(roast1BT, ".//Level")), identity), identity))) # REMEMBER SUBSET FUNCTION IS []
masterXMLagtronRawDataFrame$roast2BTagtronRawData = as.numeric(laply(laply(as_list(xml_find_all(roast2BT, ".//Level")), identity), identity)[-c(29, 30)]) # REMEMBER SUBSET FUNCTION IS []

# ----- Changing Time to mins in XML data frame -----
masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time = as.numeric(masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time) / 60

# RED = Curve 1 = r1 ambt
# BLUE = Curve 2 = r1 Bt
ggplot(masterXMLagtronRawDataFrame, aes(x = masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time, group=1)) +
  geom_line(size=1.2, color="blue", aes(y= masterXMLagtronRawDataFrame$roas2agtronRawDataCurve1agtronRawData)) + 
  geom_line(size=1.2, color="red", aes(y = masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1agtronRawData)) +
  xlab("Time (s) Since Start of Roast") + ylab("Ambient Temperature (Note flipped scales)") + labs(title = "Line Plot Comparing AMBT Thermocouple of Roast 1 (04.12 Peru)(Red) and Roast 2 (06.02 Peru)(Blue)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  # + scale_y_discrete() + scale_y_reverse()
  
ggplot(masterXMLagtronRawDataFrame, aes(x = masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time, group=1)) +
  geom_line(size=1.2, color="blue", aes(y= masterXMLagtronRawDataFrame$roast1BTagtronRawData)) + 
  geom_line(size=1.2, color="red", aes(y = masterXMLagtronRawDataFrame$roast2BTagtronRawData)) +
  
  xlab("Time (s) Since Start of Roast") + ylab("BT Temperature") + labs(title = "Line Plot Comparing BT Thermocouple of Roast 1 (04.12 Peru)(Red) and Roast 2 (06.02 Peru)(Blue)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  + scale_y_discrete()

# ------ Derivitie -----
for(i in seq_len(nrow(masterXMLagtronRawDataFrame) - 1)){
  masterXMLagtronRawDataFrame$roast1BtDeriv[i] = (masterXMLagtronRawDataFrame$roast1BTagtronRawData[i+1] - masterXMLagtronRawDataFrame$roast1BTagtronRawData[i]) / 2
  masterXMLagtronRawDataFrame$roast2BtDeriv[i] = (masterXMLagtronRawDataFrame$roast2BTagtronRawData[i+1] - masterXMLagtronRawDataFrame$roast2BTagtronRawData[i]) / 2
} 

ggplot(masterXMLagtronRawDataFrame, aes(x = masterXMLagtronRawDataFrame$roast1agtronRawDataCurve1Time, group=1)) +
  #geom_line(size=1.2, color="blue", aes(y= masterXMLagtronRawDataFrame$roast1BTagtronRawData)) + 
  #geom_line(size=1.2, color="red", aes(y = masterXMLagtronRawDataFrame$roast2BTagtronRawData)) +
  geom_line(size=1.2, color="cyan", aes(y = masterXMLagtronRawDataFrame$roast1BtDeriv)) +
  geom_line(size=1.2, color="orange", aes(y = masterXMLagtronRawDataFrame$roast2BtDeriv)) +
  
  xlab("Time (s) Since Start of Roast") + ylab("BT Temperature") + labs(title = "Line Plot Comparing BT Thermocouple of Roast 1 (04.12 Peru)(Red) and Roast 2 (06.02 Peru)(Blue)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
+ scale_y_discrete()

# ----- Faceting -----
ggplot(agtronRawData, aes(x = agtronRawData$Agtron.Converted, group=1)) +
  geom_histogram() + 
  facet_wrap(. ~ agtronRawData$Blend) +
  xlab("Agtron Converted Value") + ylab("Count")
  labs(title = "Faceted Histogram of Agtron Values by Blend") 
  

# roast1OXT = rawagtronRawDataCurves[2]
roast1BT  
roast1AMBT

roast2EXT
roast2OXT
roast2BT
roast2AMBT

