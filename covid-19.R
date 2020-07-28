#Importing Libraries-----------------
library(ggplot2)
library(forcats)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)

# table one -----------------------------------------------------------

df<-read.csv("project(data science)\\covid_19_india.csv", header = TRUE)
head(df)

tail(df)
plot(df)
summary(df)
plot(df$ï..Sno,df$Deaths)


# Need a table with frequencies for each category
states_india <- table(df$State.UnionTerritory)  # Create table
barplot(states_india)              # Bar chart
plot(states_india)                 # Default X-Y plot (lines)



barplot(states_india,
        main = "COVID-19 State wise Distribution in India",
        xlab = "Cases",
        las=1,
        col = "darkred",
        horiz = TRUE) 
summary(df$State.UnionTerritory)
summary(df$ConfirmedIndianNational)




plot(df$Date,df$Deaths)


hist(df$Cured[df$Date==19/04/20])

dev.off() 
cat("\014")






#Table Two------------------------------------------------------------------------

df2<-read.csv("AgeGroupDetails.csv", header = TRUE)

#Head-----

head(df2)

#Tail-----

tail(df2)

#Summary---

summary(df2)


#Total number of cases grouped age wise -----------------------------------------------------------

barplot(df2$TotalCases,
        names.arg = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69","70-79",">=80","Unknown"),
        col=c("indianred1","indianred1","indianred4","indianred4","indianred3","indianred2","indianred3","indianred1","indianred1","indianred1"),
        ylim = c(0,200),
        xlab="AGE",
        ylab = "Total Number of cases",
        main = "TOTAL NUMBER OF CASES GROUPED AGE WISE")






# Table Three======================================================================-

df3<-read.csv("HospitalBedsIndia.csv", header = TRUE)
# Head---------------------------------------------------------------------------------------------
head(df3)
#ploting number of public beds according to states
barplot(df3$NumPublicBeds_HMIS,
        ylim = c(500,300000))

#Removing the last row containing all india stats
df3<- df3[-c(37),]
tail(df3)


df3%>%
mutate(name = fct_reorder(State.UT,desc(NumPublicBeds_HMIS)))%>%
ggplot(aes(x=State.UT,y=NumPublicBeds_HMIS))+
                geom_bar(stat = "identity",alpha=0.7,colour="red")+
                coord_flip()+
                xlab(" ")+
                theme_ipsum(axis_text_size = 7)+
                ylab("Number of Public Beds")+
                ggtitle("Public Beds State Wise")





#=================================================================================

df5<-read.csv("ICMRTestingDetails.csv", header = TRUE)

head(df5)
tail(df5)
plot(df5$ï..SNo,df5$TotalPositiveCases,
     xlab="DATE FROM :13/03/20 TILL 22/04/20",
     ylab="Total positive cases",
     
     col="purple",
     pch=19,
     cex=1.5,
     main="Total Positive cases")

p<-df5 %>%
        
        mutate(text = paste("Date/Time: ", DateTime, "\nTotalSamplesTested: ", TotalSamplesTested, sep="")) %>%
        ggplot(aes(x=ï..SNo,y=TotalPositiveCases,size=TotalSamplesTested,text=text))+
        geom_point(alpha=0.4,color="purple")+
        scale_size(range=c(.5,15))+
        scale_fill_viridis(discrete=TRUE, guide=FALSE) +
        theme_ipsum() +
        theme(legend.position="bottom") +
        ylab("Total Positive Cases") +
        xlab("DATE FROM :13/03/20 TILL 22/04/20")+
        ggtitle("No. of Total Positive Cases \n From March to April 2020")
p
pp<-ggplotly(p,tooltip="text")
pp






plot(df5$TotalSamplesTested,
     xlab="DATE FROM :13/03/20 TILL 22/04/20",
     ylab="Total Samples Tested",
     col="purple",
     pch=15,
     cex=1.5,
     type = "p",)
     points(df5$TotalIndividualsTested,col="red",pch=19,cex=1.5)
     legend("topleft",
            c("samples","individuals"),
            fill = c("purple","red"))
     
     






#----------Table 6--------------------------------------------
df6<-read.csv("ICMRTestingLabs.csv", header = TRUE)
head(df6)

?barplot
states_testcenters<-table(df6$state)

par(mar=c(4,9,4,4))
barplot(states_testcenters[order(states_testcenters,decreasing = FALSE)],
        horiz = TRUE,
        las=1,
        cex.names = 0.5,
        cex.axis = 1.0,
        main = "ICMR Testing Labs ",
        xlim = c(0,40),
        xlab = "Total Number of Labs",
        col=c("gray49","gray48","gray47","gray46","gray45","gray44","gray43","gray42","gray41",
              "gray40","gray39","gray38","gray37","gray36","gray35","gray34","gray33","gray32",
              "gray31","gray30","gray29","gray28","gray27","gray26","gray25","gray24","gray23",
              "gray22","gray21","gray20","gray19","gray18","gray17","gray16")
       
        
        )


# Center Type--------------------------------------
centre_type<-table(df6$type)
barplot(centre_type[order(centre_type,decreasing = FALSE)],
        main = "TYPE OF TEST CENTRES",
        col = c("lightskyblue2","lightskyblue3","lightskyblue4"),
        ylim = c(0,200),
        ylab = " Number of Centres ",
        )

#ggplot version of the above graph-------------------------
ggplot(df6,aes(x=type,fill=state))+
        geom_bar()+
        theme_ipsum()+
        scale_fill_viridis_d(option="plasma")
        

dev.off() 
cat("\014")

#=======================Table 7=========================================
df7<-read.csv("StatewiseTestingDetails.csv", header = TRUE)

#---------------------removing null values------------


df7<-df7[!(df7$State=="Meghalaya" | df7$Positive==552),]
#------head-----------2020-02-16	Meghalaya	552	299	7
head(df7)
#--------Tail---------

tail(df7)

#----------changing date column to type date----------
str(df7$Date)
df7$Date<-as.Date(df7$Date)

par(mfrow = c(3, 1))

plot(df7$Date[df7$State=="Delhi"],df7$Positive[df7$State == "Delhi"],
     pch = 19,
     cex = 1.5,
     ylab = "Positive",
     xlab = "Date",
     main = "Delhi (Positive cases)",
     col="turquoise3")

plot(df7$Date[df7$State=="Madhya Pradesh"],df7$Positive[df7$State == "Madhya Pradesh"],
     pch = 19,
     cex = 1.5,
     ylab = "Positive",
     xlab = "Date",
     main = "Madhya Pradesh (Positive cases)",
     col="palevioletred3")


plot(df7$Date[df7$State=="Tamil Nadu"],df7$Positive[df7$State == "Tamil Nadu"],
     pch = 19,
     cex = 1.5,
     ylab = "Positive",
     xlab = "Date",
     main = "Tamil Nadu (Positive cases)",
     col="palegreen3")


# Restore graphic parameter
par(mfrow=c(1, 1))



#---------------All States-----------------------------------


ggplot(data=df7,aes(y=Positive,x=Date,col=State))+geom_point()+
        theme_ipsum_pub()+
        scale_fill_viridis_d(option="plasma")+
        ggtitle(" Total Positive Cases State Wise\n From April to March 2020")





#-----------------------------------------------------

df8<-read.csv("new_data\\nation_level_daily.csv", header = TRUE)

#-------head-------

head(df8)
#--------------Tail
tail(df8)

#--------------

str(df8)
library(lubridate)       
ymd(df8$date)     
as.Date(df8$date)
parse_date_time2(df8$date,orders="mdy")         

par(mar=c(4,4,1,1))
plot(df8$totalconfirmed,
     ylim = c(500,100000),
     las=1,
     pch = 16,
     cex = 1.3,
     ylab = "Positive",
     xlab = "February To May 2020",
     main = "National Level (Positive cases)",
     col="deeppink4",
     
     )

#===================================================

df9<-read.csv("new_data\\state_level_latest.csv", header = TRUE)

head(df9)
df9<- df9[-c(1),]

df9%>%
        mutate(statecode=fct_reorder(statecode,desc(confirmed)))%>%
        ggplot(aes(x=statecode,y=confirmed))+
        geom_bar(stat = "identity",alpha=0.8,fill="darkslateblue")+
        theme_ipsum()+
        ggtitle("State Wise Total Confirmed Cases")
        
