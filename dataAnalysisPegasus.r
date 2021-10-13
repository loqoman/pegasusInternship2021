# Data Analysis on Pegasus Roastary EVN and Agtron converted values for the months of april through June.
# By Darwin Clark for Pegasus Roastary

# TODO: Indexing function for the xml (List the number of roasts, the date of the roast and the type of bean)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(lubridate)

library(xml2)
library(purrr)

library(plyr) # Used to fix the lists out of XML world
library(dplyr) # Used to fix 

# TODO: Rename this variable to somethnig meaningful
data = read.csv("~/Programming/pegasusInternship2021/data/roastaryRoastData0603.csv")
data$Date = as.Date(data$Date, format="%y.%m.%d") # Fix the dates in the d
 
xmlData = read_xml("~/Programming/pegasusInternship2021/data/For_darwin.xml")
# Setting up colors
myColors = brewer.pal("div")(5)
show_col(brewer_pal()(10))

# ====================== DATA FORMATTING =========================== #

names(myColors) = levels(data$Blend)
custom_colors = scale_color_manual(name = "Blends", values = myColors)

tempCountByBlend = data %>% group_by(Blend) %>% summarize(count=n())



# ====================== START OF GRAPHING ========================== #
# Boxplots

# aes(fill = ifelse(tempCountByBlend[grep(data$Blend, tempCountByBlend$Blend),][[2]] > 15, "darkcyan", "red"))
# TODO: Add coloring based off of sample size
ggplot(data, aes(x = data$Blend, y = data$Agtron.Converted)) +
  geom_boxplot(aes(color=data$Blend)) +
  xlab("Blend Variety") + ylab("Agtron Converted Value") + labs(title = "Box Plots Comparing Agtron Converted Values of Different Roast Blends") +
  theme(legend.position = "non")

summary(data$Agtron.Converted)

tempMaxDates = data %>% group_by(Blend) %>% filter(Date == max(Date))

# Scatter Plots
ggplot(data, aes(x = data$Date, y = data$Agtron.Converted)) +
  geom_line(aes(group=data$Blend, color=data$Blend), size = 1.1) +
  #geom_point(aes(color=data$Blend)) +
  geom_label_repel(aes(label=ifelse(Date>as.Date("2021-05-20"), Blend, ''), fill=data$Blend), nudge_x=1, na.rm = TRUE,
                   fontface = 'bold', color = 'white',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = 'grey50') +
  xlab("Date") + ylab("Agtron Converted Value") + labs(title = "Line Plot Comparing Date of Roast to Agtron Converted Value") 



ggplot(data) +
  geom_line(aes(x = data$Time, y = data$Agtron.Converted, group=data$Blend, color=data$Blend)) +
  #geom_point(aes(color=data$Blend)) +
  xlab("Time") + ylab("Agtron Converted Value") + labs(title = "Line Plot Comparing Time of Roast to Agtron Converted Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))

# ============================= XML 2 ============================= #
masterXMLDataFrame = data.frame(matrix(vector(), 28, 3, dimnames=list(c(), c("Date", "File", "User"))), stringsAsFactors=F)

xml_attrs(xmlData, "RoastmasterEntities")
  
xml_children(xmlData)[2]["Roast"]

xml_find_all(xmlData, "RoastmasterXML/RoastmasterEntities/Roast")

# What I'll likely do is find all the roasts FIRST, then pull from the proper thermocouple in each roast to create a data.frame() object
rawNodes = xml_find_all(xmlData, "//Node") # Will select every "Node" element in the Document
rawRoasts = xml_find_all(xmlData, "//Roast") # Will select every "Roast" element in the Document

rawDataCurves = xml_find_all(xmlData, "//Curve") # Will select every "Curve" element in the Document


# ===== Testing how to get the first (of 4) "curve" datasets for each roast ===== # 

firstTestDataCurve = xml_find_all(xmlData, "//DataCurves")[1]
secondTestDataCurve = xml_find_all(xmlData, "//DataCurves")[2]

# Now we have the 4 'curve' objects fr each of the 2 roasts
# now want to match for each of the thermocouples
# N.B The matching is based off of a RegEX system called XPath

xml_children(rawDataCurves)
xml_attrs(rawDataCurves)
xml_text(rawDataCurves)

# These are each individual curve objects
roast1AMBT = rawDataCurves[1] # Pick the first dataCurve (arbitraryily)
roast1AMBT = xml_find_all(roast1AMBT, ".//Node")  # Get all the "node" children within this roast 

roast2AMBT = rawDataCurves[9] # Pick the NEXT AMBT dataCurve (arbitraryily)
roast2AMBT = xml_find_all(roast2AMBT, ".//Node")

roast1BT = rawDataCurves[2]
roast1BT = xml_find_all(roast1BT, ".//Node")  # Get all the "node" children within this roast 
  
roast2BT = rawDataCurves[10]
roast2BT = xml_find_all(roast2BT, ".//Node")  # Get all the "node" children within this roast 

masterXMLDataFrame$roast1dataCurve1Time = laply(laply(as_list(xml_find_all(roast1AMBT, ".//Time")),identity), identity) # REMEMBER SUBSET FUNCTION IS []
masterXMLDataFrame$roast1dataCurve1Data = laply(laply(as_list(xml_find_all(roast1AMBT, ".//Level")), identity), identity) # REMEMBER SUBSET FUNCTION IS []

masterXMLDataFrame$roas2dataCurve1Data = laply(laply(as_list(xml_find_all(roast2AMBT, ".//Level")), identity), identity)[-c(29, 30)] # REMEMBER SUBSET FUNCTION IS []

masterXMLDataFrame$roast1BTData = laply(laply(as_list(xml_find_all(roast1BT, ".//Level")), identity), identity) # REMEMBER SUBSET FUNCTION IS []
masterXMLDataFrame$roast2BTData = laply(laply(as_list(xml_find_all(roast2BT, ".//Level")), identity), identity)[-c(29, 30)] # REMEMBER SUBSET FUNCTION IS []


# RED = Curve 1 = r1 ambt
# BLUE = Curve 2 = r1 Bt
ggplot(masterXMLDataFrame, aes(x = masterXMLDataFrame$roast1dataCurve1Time, group=1)) +
  geom_line(size=1.2, color="blue", aes(y= masterXMLDataFrame$roas2dataCurve1Data)) + 
  geom_line(size=1.2, color="red", aes(y = masterXMLDataFrame$roast1dataCurve1Data)) +
  xlab("Time (s) Since Start of Roast") + ylab("Ambient Temperature (Note flipped scales)") + labs(title = "Line Plot Comparing AMBT Thermocouple of Roast 1 (04.12 Peru)(Red) and Roast 2 (06.02 Peru)(Blue))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggplot(masterXMLDataFrame, aes(x = masterXMLDataFrame$roast1dataCurve1Time, group=1)) +
  geom_line(size=1.2, color="blue", aes(y= masterXMLDataFrame$roast1BTData)) + 
  geom_line(size=1.2, color="red", aes(y = masterXMLDataFrame$roast2BTData)) +
  xlab("Time (s) Since Start of Roast") + ylab("BT Temperature") + labs(title = "Line Plot Comparing BT Thermocouple of Roast 1 (04.12 Peru)(Red) and Roast 2 (06.02 Peru)(Blue))") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# roast1OXT = rawDataCurves[2]
roast1BT  
roast1AMBT

roast2EXT
roast2OXT
roast2BT
roast2AMBT

