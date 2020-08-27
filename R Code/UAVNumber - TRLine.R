install.packages("dplyr")
install.packages("readxl")
install.packages("ggpubr")
library(ggpubr)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(magrittr)
library(Hmisc)
library(e1071)
Egypy_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/Egypy-Yearly UAV.csv")
Egypy_Yearly_UAV[is.na(Egypy_Yearly_UAV)] <- 0

UK_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/UK-Yearly UAV.csv")
UK_Yearly_UAV

UAE_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/UAE-Yearly UAV.csv")
UAE_Yearly_UAV

Israel_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/Israel-Yearly UAV.csv")
Israel_Yearly_UAV

Israel_Yearly_UAV[is.na(Israel_Yearly_UAV)] <- 0

Iran_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/Iran-Yearly UAV.csv")
Iran_Yearly_UAV

Greece_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/Greece-Yearly UAV.csv")
Greece_Yearly_UAV


TR_Yearly_UAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/TR-Yearly UAV.csv",na = "NA")
TR_Yearly_UAV

TR_Yearly_UAV_Future <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/TR-Yearly UAV - Future.csv")
TR_Yearly_UAV_Future

TR_MonthlyUAV <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/TR-MonthUAV.csv")
TR_MonthlyUAV

TR_MonthlyUAVTEST <- read.csv("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/TR-MonthUAV - Copy.csv")
TR_MonthlyUAVTEST

TR_Yearly_UAV[is.na(TR_Yearly_UAV)] <- 0
TR_Yearly_UAV_Future[is.na(TR_Yearly_UAV_Future)] <- 0
TR_MonthlyUAV[is.na(TR_MonthlyUAV)] <- 0

# Barplot
#ggplot(data = TR_Yearly_UAV, aes(x = TR_Yearly_UAV$Year, y = TR_Yearly_UAV$Number)) + geom_bar(position="dodge", stat="identity", colour="black", width=.8)

#barplot(TR_Yearly_UAV$Number,names.arg = TR_Yearly_UAV$Year)


xrange <- range(c(2007:2020))
yrange <- range(TR_Yearly_UAV$Number)

# set up the plot***************************************************************************************************************
plot(xrange, yrange, type="n", xlab="Years",
     ylab="Numbers" )

lines(x = TR_Yearly_UAV$Year,y =TR_Yearly_UAV$Number,type="b", col="red", lwd=2, pch=19 )
title("Turkey's Yearly UAV Production Graph")


TR_Yearly_UAVREV<- TR_Yearly_UAV[seq(dim(TR_Yearly_UAV)[1],1),]
TR_Yearly_UAV_FutureREV<- TR_Yearly_UAV_Future[seq(dim(TR_Yearly_UAV_Future)[1],1),]


X <-barplot(TR_Yearly_UAV_FutureREV$Number,
        main = " Future of Turkey's Yearly UAV Production",
        xlab = "Years",
        ylim = c(0,70),
        ylab = "Numbers",
        names.arg = TR_Yearly_UAV_FutureREV$Year,
        col = "darkred",
        las=2,
        density=c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,20) , angle=c(0,0,0,0,0,0,0,0,0,0,0,0,0,50)
        )



# *********************************************************************************************************************************************** USE THIS
plot_ly(TR_Yearly_UAV_FutureREV, x = ~Year, y = ~Number, type = 'bar', 
             text = TR_Yearly_UAV_FutureREV$Number, textposition = 'auto',
             marker = list(color = c('rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',
                                     'rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(255,0,0)'),
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Future of Turkey's Yearly UAV Production",
               xaxis = list(title = "Years"),
               yaxis = list(title = "Number of UAV Production"))

# *********************************************************************************************************************************************** 
barplot(TR_Yearly_UAVREV$Number,
        main = "Turkey's Yearly UAV Production",
        xlab = "Years",
        ylim = c(0,70),
        ylab = "Numbers",
        names.arg = TR_Yearly_UAVREV$Year,
        col = "darkred",
        las=2,
)

install.packages("ggpubr")
library("ggpubr")

df <- data.frame(dose=TR_Yearly_UAV_FutureREV$Year,
                 len=TR_Yearly_UAV_FutureREV$Number)

ggbarplot(df, "dose", "len",
          fill = "steelblue", color = "steelblue",
          label = TRUE, lab.pos = "in", lab.col = "white")


library(ggplot2)
ggplot(data=df, aes(x=dose, y=len)) +
        geom_bar(stat="identity", fill="steelblue")+
        coord_cartesian(ylim = c(0, 70,10))+
        geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)+
        #theme_bw()
        theme_pubclean()



# *********************************************************************************************************************************************** USE THIS
plot_ly(TR_Yearly_UAVREV, x = ~Year, y = ~Number, type = 'bar', 
        text = TR_Yearly_UAVREV$Number, textposition = 'auto',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Turkey's Yearly UAV Production",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Produced UAV"))


library(ggplot2)
Top$Counrty
Top$Number
df2 <- data.frame(Year=factor(TR_Yearly_UAVREV$Year,levels=unique(TR_Yearly_UAVREV$Year)), Number2 = TR_Yearly_UAVREV$Number)

ggplot(df2, aes(x=Year, y=Number2)) +
        geom_bar(stat="identity", fill= c("#9ecae1"))+
        geom_text(aes(label=Number2), vjust=1.6, color="white", size=3.5)+
        theme_pubclean()+
        xlab("Year") + ylab("Number of Produced UAV") +
        ggtitle(label = "Turkey's Yearly UAV Production")+
        theme(plot.title = element_text(hjust = 0.5))


# *********************************************************************************************************************************************** 
sumMonth<- rowSums(TR_MonthlyUAV[,-1])

TRMonth <- cbind(TR_MonthlyUAV[1],sumMonth)

barplot(TRMonth$sumMonth,
        main = "Turkey's Monthly UAV Production (2014-2019)",
        xlab = "Years",
        ylim = c(0,40),
        ylab = "Numbers",
        names.arg = TRMonth$Month,
        col = "darkred",
        las=2,
)


library(plotly)
xform <- list(categoryorder = "array",
              categoryarray = TRMonth[1],title="Months")

plot_ly(
        x = c("giraffes", "orangutans", "monkeys"),
        y = c(20, 14, 23),
        name = "SF Zoo",
        type = "bar") %>% 
        layout(xaxis = xform)

#FIX 
# *********************************************************************************************************************************************** USE THIS
plot_ly(TRMonth, x = TRMonth$Month, y = ~sumMonth, type = 'bar', 
        text = TRMonth$sumMonth, textposition = 'auto',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Turkey's Monthly UAV Production (2014-2019)",
               xaxis = xform,
               yaxis = list(title = "Number of Produced UAV"))




library(ggplot2)
Top$Counrty
Top$Number
df2 <- data.frame(Year=factor(TRMonth$Month,levels=unique(TRMonth$Month)), Number2 = TRMonth$sumMonth)

ggplot(df2, aes(x=Year, y=Number2)) +
        geom_bar(stat="identity", fill= c("#9ecae1"))+
        geom_text(aes(label=Number2), vjust=1.6, color="white", size=3.5)+
        theme_pubclean()+
        xlab("Months") + ylab("Number of Produced UAV") +
        ggtitle(label = "Turkey's Monthly UAV Production (2014-2019)")+
        theme(plot.title = element_text(hjust = 0.5))

# *********************************************************************************************************************************************** 
#NO
plot_ly(TRMonth, x = TRMonth$Month, y = ~sumMonth, type = 'bar', 
        text = TRMonth$sumMonth, textposition = 'auto',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Turkey's Monthly UAV Production (2014-2019)",
               xaxis = list(title = ""),
               yaxis = list(title = ""))



TR_Yearly_UAVCOMP <- TR_Yearly_UAV[1:5,]

TR_Yearly_UAVCOMPREV<- TR_Yearly_UAVCOMP[seq(dim(TR_Yearly_UAVCOMP)[1],1),]
UK_Yearly_UAVCOMPREV <- UK_Yearly_UAV[seq(dim(UK_Yearly_UAV)[1],1),]

TRLIST<- cumsum(TR_Yearly_UAVCOMPREV[2])
UKLIST<- cumsum(UK_Yearly_UAVCOMPREV[2])

UKLine <- UK_Yearly_UAV
TRLine <- TR_Yearly_UAVCOMP

UKLine$Number <- UKLIST[seq(dim(UKLIST)[1],1),]
TRLine$Number <- TRLIST[seq(dim(TRLIST)[1],1),]


TRLineREV <- TRLine[seq(dim(TRLine)[1],1),]
TRLineREV_F <- TRLine
TRLineREV_F[2] <- TRLineREV[2]

TR_Yearly_UAVCOMPTOTAL <- TR_Yearly_UAV[0:13,]
TR_Yearly_UAVCOMPREVTOTAL<- TR_Yearly_UAVCOMPTOTAL[seq(dim(TR_Yearly_UAVCOMPTOTAL)[1],1),]
TRLISTTOTAL<- cumsum(TR_Yearly_UAVCOMPREVTOTAL[2])
TRLineTOTAL <- TR_Yearly_UAVCOMPTOTAL
TRLineTOTAL$Number <- TRLISTTOTAL[seq(dim(TRLISTTOTAL)[1],1),]

TRLineTOTAL <- TRLineTOTAL[seq(dim(TRLineTOTAL)[1],1),]
barplot(TRLineTOTAL$Number,
        main = "Turkey's Total Number of UAVs (2007-2019)",
        xlab = "Years",
        ylim = c(0,200),
        ylab = "Numbers",
        names.arg = TRLineTOTAL$Year,
        col = "darkred",
        las=2,
        horiz = F
        
)#
# *********************************************************************************************************************************************** USE THIS
plot_ly(TRLineTOTAL, x = ~Year, y = ~Number, type = 'bar', 
        text = TRLineTOTAL$Number, textposition = 'auto',
        marker = list(color = 'rgb(158,202,225)',
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Turkey's Total Number of UAVs (2007-2019)",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of UAV"))



library(ggplot2)
Top$Counrty
Top$Number
df2 <- data.frame(Year=factor(TRLineTOTAL$Year,levels=unique(TRLineTOTAL$Year)), Number2 = TRLineTOTAL$Number)

ggplot(df2, aes(x=Year, y=Number2)) +
        geom_bar(stat="identity", fill= c("#9ecae1"))+
        geom_text(aes(label=Number2), vjust=1.6, color="white", size=3.5)+
        theme_pubclean()+
        xlab("Year") + ylab("Number of UAV`s") +
        ggtitle(label = "Turkey's Total Number of UAVs (2007-2019)")+
        theme(plot.title = element_text(hjust = 0.5))



# *********************************************************************************************************************************************** 

Egypy_Yearly_UAVCOMPREV <- Egypy_Yearly_UAV[seq(dim(Egypy_Yearly_UAV)[1],1),]
Israel_Yearly_UAVCOMPEV <- Israel_Yearly_UAV[seq(dim(Israel_Yearly_UAV)[1],1),]

EGLIST<- cumsum(Egypy_Yearly_UAVCOMPREV[2])
ISLIST<- cumsum(Israel_Yearly_UAVCOMPEV[2])

EGLine <- Egypy_Yearly_UAV
ISLine <- Israel_Yearly_UAV
ISLine$Number <- ISLIST[seq(dim(ISLIST)[1],1),]
EGLine$Number <- EGLIST[seq(dim(EGLIST)[1],1),]


Greece_Yearly_UAV[is.na(Greece_Yearly_UAV)] <- 0
Iran_Yearly_UAV[is.na(Iran_Yearly_UAV)] <- 0
UAE_Yearly_UAV[is.na(UAE_Yearly_UAV)] <- 0

Greece_Yearly_UAVCOMPREV<- Greece_Yearly_UAV[seq(dim(Greece_Yearly_UAV)[1],1),]
Iran_Yearly_UAVCOMPREV <- Iran_Yearly_UAV[seq(dim(Iran_Yearly_UAV)[1],1),]
UAE_Yearly_UAVCOMPREV <- UAE_Yearly_UAV[seq(dim(UAE_Yearly_UAV)[1],1),]

GRLIST<- cumsum(Greece_Yearly_UAVCOMPREV[2])
IRLIST<- cumsum(Iran_Yearly_UAVCOMPREV[2])
UAELIST<- cumsum(UAE_Yearly_UAVCOMPREV[2])

GRLine <- Greece_Yearly_UAV
IRLine <- Iran_Yearly_UAV
UARLine <- UAE_Yearly_UAV

GRLine$Number <- GRLIST[seq(dim(GRLIST)[1],1),]
IRLine$Number <- IRLIST[seq(dim(IRLIST)[1],1),]
UARLine$Number <- UAELIST[seq(dim(UAELIST)[1],1),]
# *********************************************************************************************************************************************** USE THIS
plot(UKLine,type = "b",col = "red",lwd=2, xlab = "Year", ylab = "Number of UAV's", main = "Total Number of UAV by Country",ylim=c(0,800)  )
lines(TRLine, type = "b", col = "blue",lwd=2)
lines(ISLine, type = "b", col = "black",lwd=2)
lines(EGLine, type = "b", col = "green",lwd=2)
lines(GRLine, type = "o", col = "orange",lwd=2)
lines(IRLine, type = "b", col = "pink",lwd=2)
lines(UARLine, type = "o", col = "grey",lwd=2)
mtext("MENA Countries",side = 3)
grid()

legend(2015,850, legend=c("United Kingdom", "Turkey","Isreal","Egypt","Greece","Iran","UAE"),
       col=c("red", "blue","black","green","orange","pink","grey"),
       pch=(1),
       lty=1, xpd=F, horiz=F, bty="n",cex = 0.9,xjust = .2,yjust = 1.2)
# *********************************************************************************************************************************************** 
#text(2019,138,"TR",adj = -.3,cex=.9)
#text(2019,285,"IS",adj = -.3,cex=.9)
#text(2019,726,"IR",adj = -.3,cex=.9)

# *********************************************************************************************************************************************** USE THIS
plot(UKLine,type = "o",col = "red",lwd=2, xlab = "Year", ylab = "Number of UAV's", main = "Total Number of UAV by Country",ylim=c(0,400)  )
lines(TRLine, type = "o", col = "blue",lwd=2)
lines(ISLine, type = "o", col = "black",lwd=2)
lines(EGLine, type = "o", col = "green",lwd=2)
lines(GRLine, type = "o", col = "orange",lwd=2)
lines(UARLine, type = "o", col = "grey",lwd=2)
mtext("MENA Countries Excluding Iran",side = 3)
grid()
#text(2019,138,"TR",adj = -.3,cex=.9)
#text(2019,285,"IS",adj = -.3,cex=.9)
#text(2019,87,"IS",adj = -.3,cex=.9)

legend(2015,400, legend=c("United Kingdom", "Turkey","Isreal","Egypt","Greece","UAE"),
       col=c("red", "blue","black","green","orange","grey"), 
       pch=(1),
       lty=1, xpd=F, horiz=F, bty="n",cex = 0.9,xjust = .2,yjust = 1)
# *********************************************************************************************************************************************** 

slices <- c(TRLine[1,2],UKLine[1,2],GRLine[1,2],ISLine[1,2],EGLine[1,2],UARLine[1,2],IRLine[1,2])
lbls <- c("Turkey", "United Kingdom", "Greece", "Isreal","Egypt","UAE","Iran")
pie(slices, labels = lbls, main="Pie Chart Total Numer of UAV's by Country 2019")




# Create Data
data <- data.frame(
        Country=lbls,
        value=slices
)

# Basic piechart
ggplot(data, aes(x="", y=value, fill=Country)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() # remove background, grid, numeric labels


p <- plot_ly(data, labels = ~Country, values = ~value, type = 'pie',marker = list(colors = colors,
                                                                                 line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = 'Pie Chart Total Numer of UAV`s by Country (2019)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p


library(plotly)

#*********************************************************************************************************************************************** USE THIS
# Create test data.
data <- data.frame(
        category=lbls,
        count=slices
)

p <- data %>%
        group_by(category,
                textinfo = 'label+percent') %>%
        plot_ly(labels = ~category, values = ~count,textinfo = 'label+percent') %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Total Numer of UAV`s by Country (2019)",  showlegend = T,
               xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
               yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE))

p 

#*********************************************************************************************************************************************** 




# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=4.3, aes(y=labelPosition, label=label), size=4) +
        scale_fill_brewer(palette=3) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "None")




SpendingUSD2 <- read_excel("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/2005-20018 Milisary spending USD-V3.xlsx",sheet = "This")
SpendingUSD2 <- as.data.frame(SpendingUSD2)
SpendingUSD2[is.na(SpendingUSD2)] <- 0

SpendingUSD <- read_excel("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/2005-20018 Milisary spending USD.xlsx",sheet = "Data", na = "NA")
SpendingUSD <- as.data.frame(SpendingUSD)
df2019 <- as.data.frame(SpendingUSD)
SpendingUSD[is.na(SpendingUSD)] <- 0
df2019[is.na(df2019)] <- 0

Years<- SpendingUSD2[1]
USDBil<- round(SpendingUSD2[,2]/1000000000,2)
SpendingTR <- cbind.data.frame(Years,Turkey=USDBil)

USDBil<- round(SpendingUSD2[,3]/1000000000,2)
SpendingUK<- cbind.data.frame(Years,UK=USDBil)

USDBil<- round(SpendingUSD2[,4]/1000000000,2)
SpendingIS<- cbind.data.frame(Years,IS=USDBil)

USDBil<- round(SpendingUSD2[,5]/1000000000,2)
SpendingEG<- cbind.data.frame(Years,EG=USDBil)

USDBil<- round(SpendingUSD2[,6]/1000000000,2)
SpendingGR<- cbind.data.frame(Years,GR=USDBil)

USDBil<- round(SpendingUSD2[,7]/1000000000,2)
SpendingIR<- cbind.data.frame(Years,IR=USDBil)

USDBil <- round(SpendingUSD2[,8]/1000000000,2)
SpendingUAE<- cbind.data.frame(Years,UAE=USDBil)

#*********************************************************************************************************************************************** USE THIS
plot(tail(SpendingUK),type = "o",lwd=2,col = "red", xlab = "Year", ylab = "Billion USD", main = "Defence Budget 2014-2019",ylim=c(0,70), )
lines(tail(SpendingTR), type = "o", col = "blue",lwd=2)
lines(tail(SpendingIS), type = "o", col = "black",lwd=2)
lines(tail(SpendingEG), type = "o", col = "green",lwd=2)
lines(tail(SpendingGR), type = "o", col = "orange",lwd=2)
lines(tail(SpendingIR), type = "o", col = "pink",lwd=2)
lines(tail(SpendingUAE), type = "o", col = "grey",lwd=2)
grid()
mtext("MENA Countries",side = 3)
legend("top",pch=1,x.intersp = -0.3,
       c("United Kingdom", "Turkey","Isreal","Egypt","Greece","UAE","Iran"),
       col=c("red", "blue","black","green","orange","grey","pink"), 
       lty=1, xpd=TRUE, horiz=TRUE, bty="n",cex = 0.9,xjust = .2,yjust = 1,inset = c(0,0))
#*********************************************************************************************************************************************** 




#text(2019,21.97,"TR",adj = -.3,cex=.9)
#text(2019,65.52,"UK",adj = -.3,cex=.9)
#text(2019,18.95,"IS",adj = -.3,cex=.9)


allSpending <- data.frame(SpendingTR[2])




round(df2019[-1]/1000000000,2)

test2019<- rbind(test2019[1:5,],test2019[7:8,])


test2019<- cbind.data.frame(df2019[1],df2019[16])
test2019$NUmber <- c(TRLine[1,2],
                     UKLine[1,2],
                     ISLine[1,2],
                     EGLine[1,2],
                     GRLine[1,2],
                     IRLine[1,2],
                     UARLine[1,2])

test2019


SpendingUK
SpendingTR
SpendingIS
SpendingEG
SpendingSA
SpendingIR
SpendingGR


plot_ly(test2019) %>%
        add_trace(x = test2019$`Country Name`, y = test2019$`2019`, type = 'bar', name = 'Wind',
                  marker = list(color = '#C9EFF9'),
                  hoverinfo = "text",
                  text = ~paste(test2019$`2019`, ' mph')) %>%
        add_trace(x = test2019$`Country Name`, y =test2019$NUmber , type = 'scatter', mode = 'lines', name = 'Number', yaxis = 'y2',
                  line = list(color = '#45171D'),
                  hoverinfo = "text",
                  text = ~paste(y =test2019$NUmber, '°F')) %>%
        layout(title = 'New York Wind and Temperature Measurements for September 1973',
               xaxis = list(title = ""),
               yaxis = list(side = 'left', title = 'Wind in mph', showgrid = FALSE, zeroline = FALSE),
               yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees F', showgrid = FALSE, zeroline = FALSE))

plot_ly(test2019) %>%
        add_trace(x = test2019$`Country Name`, y =test2019$NUmber , type = 'bar', name = 'Wind',
                  marker = list(color = '#C9EFF9'),
                  hoverinfo = "text",
                  text = ~paste(test2019$NUmber, ' mph')) %>%
        add_trace(x = test2019$`Country Name`, y =test2019$`2019`, type = 'scatter', mode = 'lines', name = 'Number', yaxis = 'y2',
                  line = list(color = '#45171D'),
                  hoverinfo = "text",
                  text = ~paste(y =test2019$`2019`, '°F')) %>%
        layout(title = 'New York Wind and Temperature Measurements for September 1973',
               xaxis = list(title = ""),
               yaxis = list(side = 'left', title = 'Wind in mph', showgrid = FALSE, zeroline = FALSE),
               yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees F', showgrid = FALSE, zeroline = FALSE))



airquality_sept <- airquality[which(airquality$Month == 9),]
airquality_sept$Date <- as.Date(paste(airquality_sept$Month, airquality_sept$Day, 1973, sep = "."), format = "%m.%d.%Y")

plot_ly(airquality_sept) %>%
        add_trace(x = ~Date, y = ~Wind, type = 'bar', name = 'Wind',
                  marker = list(color = '#C9EFF9'),
                  hoverinfo = "text",
                  text = ~paste(Wind, ' mph')) %>%
        add_trace(x = ~Date, y = ~Temp, type = 'scatter', mode = 'lines', name = 'Temperature', yaxis = 'y2',
                  line = list(color = '#45171D'),
                  hoverinfo = "text",
                  text = ~paste(Temp, '°F')) %>%
        layout(title = 'New York Wind and Temperature Measurements for September 1973',
               xaxis = list(title = ""),
               yaxis = list(side = 'left', title = 'Wind in mph', showgrid = FALSE, zeroline = FALSE),
               yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees F', showgrid = FALSE, zeroline = FALSE))




AllYAVNumbers <- read_excel("C:/Users/leven/Desktop/RecentHomework/365/365Project/Excel File/Country CSV/AllUAVNumers.xlsx")
AllYAVNumbers <- as.data.frame(AllYAVNumbers)

summary(AllYAVNumbers)
UavNumber_desc <- describe(AllYAVNumbers)
UavNumber_desc
Spending_desc<- describe(round(SpendingUSD[-1]/1000000000,2))
Spending_desc

describe(SpendingUSD2)
describe()


AllYAVNumbers[2]
quantile(AllYAVNumbers[,2], c(.95, .75, .25,0.05)) 
SpendingUSD2[2]

sd(SpendingUSD2[,2])

skewness((SpendingUSD2[,2]),1)








df2 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

df <- read_xlsx('C:/Users/leven/OneDrive/RecentHomework/365/365Project/Excel File/Country CSV/Code.xlsx')

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
)

p <- plot_geo(df) %>%
        add_trace(
                z = df$Number, color = df$Number, colors = 'Blues',
                text = df$Country, locations = df$CODE, marker = list(line = l)
        ) %>%
        colorbar(title = 'Total Number of UAVs (2019)', tickprefix = '') %>%
        layout(
                title = '2019 Total Number of UAVs',
                geo = g
        )

p




install.packages("ggpubr")
library(ggpubr)

library(readxl)
Top <- read_excel("C:/Users/leven/OneDrive/RecentHomework/365/365Project/Excel File/Country CSV/Top.xlsx")
Top$Counrty
Top$Number
test<- Top
library(ggplot2)

xform <- list(categoryorder = "array",
              categoryarray = Top[1],title="Counrty")


library(plotly)
#*********************************************************************************************************************************************** USE THIS

plot_ly(Top, x = ~Counrty, y = ~Number, type = 'bar', 
        text = Top$Number, textposition = 'auto',
        marker = list(color = c('rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',
                                'rgb(255,0,0)'),
                      line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
        layout(title = "Top 5 UAV Users and Turkey *",
               xaxis = xform,
               yaxis = list(title = "Number of UAV`s"))


#***********************************************************************************************************************************************
library(ggplot2)
Top$Counrty
Top$Number
df2 <- data.frame(Counrty2=factor(Top$Counrty,levels=unique(Top$Counrty)), Number2 = Top$Number)

ggplot(df2, aes(x=Counrty2, y=Number2),ylim = c(0,1000)) +
        geom_bar(stat="identity", fill= c("#9ecae1", "#9ecae1", "#9ecae1","#9ecae1", "#9ecae1", "#ff0000"))+
        geom_text(aes(label=Number2), vjust=1.6, color="white", size=3.5)+
        #theme_bw()
        theme_pubclean()+
        xlab("Counrty") + ylab("Number of UAV`s") +
        ggtitle(label = "Top 5 UAV Users and Turkey *")+
        theme(plot.title = element_text(hjust = 0.5))
        #labs(title="Top 5 UAV Users and Turkey *", x ="Counrty", y = "Number of UAV`s")       




