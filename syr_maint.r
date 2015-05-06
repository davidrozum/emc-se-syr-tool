# File-Name:        syr_maint.r
# Date:             2015-05-04
# Author:           David Rozum
# Purpose:          Using EMC SYR data, produce a graphical "Entitled/Non-entitled" analysis, and maintenance end data by quarter
# Real Purpose:     Learn some basic concepts of R
# Data Used:        Configuration file: SYR_maint_cfg.csv - defines input/output file names
#                   Customer SYR output file as a .csv
# Packages Used:    None
# Output Files:     .csv with quarterly maint. expirations, 
#                   .png with entitled/non-entitled by site id or city state.
#
# Note:             The graphical output can be done by either site ID, or by City & State. This is specified in the 
#                   SYR_maint_cfg.csv file.  If the "Y Axis" field is set to "CityState", then the City & State version
#                   will output. Any other entry yields the site ID output.
#


#read the configuration file - assumes it's in the current directory. Force it to be a matrix instead of a data frame
#a matrix lets us use a config[row-label,column-label] convention to access elements
#---------
config <- as.matrix(read.csv("SYR_maint_cfg.csv", header=TRUE, row.names=1))
cat("Using the following configuration:\n") #show 'em what we got
print(config)

#read the customer SYR file
#----------
cat("\nReading customer data\n")
cust.syr <- read.csv(config["Customer File","value"], header=TRUE, as.is=TRUE)  # don't want data as factors

#play with the data
#----------
cust.syr <- cust.syr[which(cust.syr$Category != "ESRS"),]                                #don't care about ESRS
cust.syr[which(cust.syr$Entitlement.Status==""), ]$Entitlement.Status <- "Not Entitled" #empty entitlements are 'not entitled'
cust.syr[which(cust.syr$Contract.Line.End==""), ]$Contract.Line.End <- "12/31/2005"     #empty date gets ficticious date
cust.syr$Contract.End <- as.Date(cust.syr$Contract.Line.End,"%m/%d/%Y")              #convert date string to date, then to qtrs.
cust.syr$Contract.Line.End <- paste( strftime(cust.syr$Contract.End,format="%Y-"),quarters(cust.syr$Contract.End),sep="")

#create a table of category vs. contract end by quarter, then write it.
#----------
date.tab <- table(cust.syr$Category, cust.syr$Contract.Line.End, dnn=c("Category","Quarter"))
write.csv(date.tab,file=config["Output CSV","value"])

#set up for the graphics
#----------
colors <- c("blue","red")  #colors for the bar chart
chart.width <- 1400  #pixels

if( config["Y Axis","value"]=="CityState" ){                                #summarize by city and state
    cust.syr$CityState <- paste(cust.syr$City, cust.syr$State)                #create a pseudo field to make it easier to count
    cust.tab <- table(cust.syr$Entitlement.Status, cust.syr$CityState)      #make a 2x2 table out of it
    chart.width <- 1800                                                     #make the png wider

} else {   #else by site id
    cust.tab <- table(cust.syr$Entitlement.Status, cust.syr$Site.No)      #make a 2x2 table out of it
}

#display it first - just for fun
#-----------
barplot(cust.tab, beside=TRUE, 
    main=config["Title","value"], sub=config["Sub","value"],
    cex.axis=0.8, col=colors)
legend("topright", c("Entitled","Not Entitled"),bty="o", cex=1.0, fill=colors)


#create a png device (file) for output. Could do a jpg, tiff, whatever. png is lossless, though
#-----------
cat("Generating plot\n")
png(config["Output Image","value"], width=chart.width, height=1200, res=100)

barplot(cust.tab, beside=TRUE,
    main=config["Title","value"], sub=config["Sub","value"],
    cex.axis=0.8, col=colors)
legend("topright",c("Entitled","Not Entitled"),bty="o", cex=1.3, fill=colors)

dev.off()  #close the file
cat("Done! Output file is ", config["Output Image","value"], "\n")
