#SET DIRECTORY
setwd("C:/Users/RV/Desktop/EXTRA CREDIT")
wd <- "C:/Users/RV/Desktop/EXTRA CREDIT"

# MAKE A DIRECTORY FOR THE SOLUTIONS
dir.create("CONFIRMED_MAPS")
tmpfolder1 <- 'CONFIRMED_MAPS'

dir.create("DEATH_MAPS")
tmpfolder2 <- 'DEATH_MAPS'

dir.create("RECOVERED_MAPS")
tmpfolder3 <- 'RECOVERED_MAPS'

dir.create("PLOTS")
tmpfolder4 <- 'PLOTS'


##########DOWNLOAD CONFIRMED CASES########
library(readr)
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv"
CONFIRM <- read_csv(url(urlfile))
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Deaths_archived_0325.csv"
DEATH <- read_csv(url(urlfile))
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Recovered_archived_0325.csv"
RECOVERED <- read_csv(url(urlfile))

COVID1 <- CONFIRM[CONFIRM$`Country/Region`=='US',]
COVID2 <- DEATH[DEATH$`Country/Region`=='US',]
COVID3 <- RECOVERED[DEATH$`Country/Region`=='US',]




# manipulate data to find new cases per day
mydata1 <- COVID1
mydatadifference11 <- COVID1[5:(ncol(mydata1)-1)]
mydatadifference12 <- COVID1[6:ncol(mydata1)]
difference1 <- mydatadifference12-mydatadifference11
#need to add back state info
difference1 <- cbind(COVID1[,c(1:2)],difference1[,c(1:ncol(difference1))])

# change new cases into percentage change
percentdiff11 <- difference1[3:(ncol(difference1)-1)]
percentdiff12 <- difference1[4:ncol(difference1)]

X<-percentdiff11
X<-replace(X,X==0,1)

percentdif1 <- ((percentdiff12 - percentdiff11)/X)*100
percentdif1 <- cbind(COVID1[,c(1:2)],percentdif1[,c(1:ncol(percentdif1))])

# manipulate data to find new cases per day
mydata2 <- COVID2
mydatadifference21 <- COVID2[5:(ncol(mydata2)-1)]
mydatadifference22 <- COVID2[6:ncol(mydata2)]
difference2 <- mydatadifference22-mydatadifference21
#need to add back state info
difference2 <- cbind(COVID2[,c(1:2)],difference2[,c(1:ncol(difference2))])

# change new cases into percentage change
percentdiff21 <- difference2[3:(ncol(difference2)-1)]
percentdiff22 <- difference2[4:ncol(difference2)]

Y<-percentdiff21
Y<-replace(Y,Y==0,1)

percentdif2 <- ((percentdiff22 - percentdiff21)/Y)*100
percentdif2 <- cbind(COVID2[,c(1:2)],percentdif2[,c(1:ncol(percentdif2))])


# manipulate data to find new cases per day
mydata3 <- COVID3
mydatadifference31 <- COVID3[5:(ncol(mydata3)-1)]
mydatadifference32 <- COVID3[6:ncol(mydata3)]
difference3 <- mydatadifference32-mydatadifference31
#need to add back state info
difference3 <- cbind(COVID3[,c(1:2)],difference3[,c(1:ncol(difference3))])

# change new cases into percentage change
percentdiff31 <- difference3[3:(ncol(difference3)-1)]
percentdiff32 <- difference3[4:ncol(difference3)]

Y<-percentdiff31
Y<-replace(Y,Y==0,1)

percentdif3 <- ((percentdiff32 - percentdiff31)/Y)*100
percentdif3 <- cbind(COVID3[,c(1:2)],percentdif3[,c(1:ncol(percentdif3))])

######READING MAP DATA#######
library(rgdal)
USA <- readOGR(paste0(wd,"/tl_2019_us_state"),
               "tl_2019_us_state")

plot(USA, col='red')

######DOWNLOAD AND SUBSET STATES OF INTERESTS
lower48 <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
lower48 <- read_csv(url(lower48))

#DROPPING AK, HI, DC
lower48 <- lower48[lower48$Abbreviation!="HI",]
lower48 <- lower48[lower48$Abbreviation!="AK",]
lower48 <- lower48[lower48$Abbreviation!="DC",]
USA@data$NAME
#MERGE .CSV TO SHAPE FILE
USA <- merge(x=USA,
             y=lower48,
             by.x="NAME",
             by.y="State")

#DROP MISSING DATA
USA <- USA[!is.na(USA@data$Abbreviation),]

#PLOTTING USA MAP
plot(USA, col='pink',
     main="My First Map")






#####MAIN QUESTION IN ASSIGNMENT 4######
library(ggplot2)
library(tmap)
library(RColorBrewer)

#MERGE WITH COVID DATA
Corona1<- merge(x=lower48,
                y=COVID1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA1 <- merge(USA,
              Corona1,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona2<- merge(x=lower48,
                y=difference1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA2 <- merge(USA,
              Corona2,
              by.x="NAME",
              by.y='State',
              all.x=T)

Corona3<- merge(x=lower48,
                y=percentdif1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA3 <- merge(USA,
              Corona3,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona4<- merge(x=lower48,
                y=COVID2,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA4 <- merge(USA,
              Corona4,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona5<- merge(x=lower48,
                y=difference2,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA5 <- merge(USA,
              Corona5,
              by.x="NAME",
              by.y='State',
              all.x=T)

Corona6<- merge(x=lower48,
                y=percentdif2,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA6 <- merge(USA,
              Corona6,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona7<- merge(x=lower48,
                y=COVID3,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA7 <- merge(USA,
              Corona7,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona8<- merge(x=lower48,
                y=difference3,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA8 <- merge(USA,
              Corona8,
              by.x="NAME",
              by.y='State',
              all.x=T)

Corona9 <- merge(x=lower48,
                y=percentdif3,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA9 <- merge(USA,
              Corona9,
              by.x="NAME",
              by.y='State',
              all.x=T)

#####MAPS ON TOTAL REPORTED CONFIRMED CASES DAILY#####
x <- USA1@data

a <- 75
for(a in 20:ncol(USA1)){
  map <-   tm_shape(USA1)+
    tm_fill(names(USA1@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("TOTAL REPORTED  COVID-19 CONFIRMED  CASES DAILY ")
  
  tmpName <- gsub("/", "-", names(USA1)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder1,"/",
                   tmpName,"TOTAL",".png"))
  
}
#####MAPS ON NEW REPORTED CONFIRMED CASES PER DAY#####
x <- USA2@data

a <- 75
for(a in 18:ncol(USA2)){
  map <-   tm_shape(USA2)+
    tm_fill(names(USA2@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("NEW REPORTED COVID-19 CONFIRMED  CASES PER DAY")
  
  tmpName <- gsub("/", "-", names(USA2)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder1,"/",
                   tmpName,"DIFF",".png"))
  
}
#####MAPS ON PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY#####
x <- USA3@data

a <- 75
for(a in 18:ncol(USA3)){
  map <-   tm_shape(USA3)+
    tm_fill(names(USA3@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY")
  
  tmpName <- gsub("/", "-", names(USA3)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder1,"/",
                   tmpName,"PERCDIF",".png"))
  
}
#####MAPS ON TOTAL DEATH CASES DAILY#####
x <- USA4@data

a <- 75
for(a in 20:ncol(USA4)){
  map <-   tm_shape(USA4)+
    tm_fill(names(USA4@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("TOTAL COVID-19 DEATH CASES DAILY")
  
  tmpName <- gsub("/", "-", names(USA4)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/",
                   tmpName,"TOTAL",".png"))
  
}
#####MAPS ON NEW REPORTED DEATH CASES PER DAY#####
x <- USA5@data

a <- 75
for(a in 18:ncol(USA5)){
  map <-   tm_shape(USA5)+
    tm_fill(names(USA5@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("NEW REPORTED COVID-19 DEATH CASES PER DAY")
  
  tmpName <- gsub("/", "-", names(USA5)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/",
                   tmpName,"DIFF",".png"))
  
}
#####MAPS ON DEATH PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY#####
x <- USA6@data

a <- 75
for(a in 18:ncol(USA6)){
  map <-   tm_shape(USA6)+
    tm_fill(names(USA6@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("COVID-19 DEATH PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY")
  
  tmpName <- gsub("/", "-", names(USA6)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/",
                   tmpName,"PERCDIF",".png"))
  
}
#####MAPS ON TOTAL RECOVERED CASES DAILY#####
x <- USA7@data

a <- 75
for(a in 20:ncol(USA7)){
  map <-   tm_shape(USA7)+
    tm_fill(names(USA7@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("TOTAL COVID-19 RECOVERED CASES DAILY")
  
  tmpName <- gsub("/", "-", names(USA7)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder3,"/",
                   tmpName,"TOTAL",".png"))
  
}
#####MAPS ON NEW REPORTED RECOVERED CASES PER DAY#####
x <- USA8@data

a <- 75
for(a in 18:ncol(USA8)){
  map <-   tm_shape(USA8)+
    tm_fill(names(USA8@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("NEW REPORTED COVID-19 RECOVERED CASES PER DAY")
  
  tmpName <- gsub("/", "-", names(USA8)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder3,"/",
                   tmpName,"DIFF",".png"))
  
}
#####MAPS ON RECOVERED PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY#####
x <- USA9@data

a <- 75
for(a in 18:ncol(USA9)){
  map <-   tm_shape(USA9)+
    tm_fill(names(USA9@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("COVID-19 RECOVERED PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY")
  
  tmpName <- gsub("/", "-", names(USA9)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder3,"/",
                   tmpName,"PERCDIF",".png"))
  
}


#####QUESTION 2: COMPARISON OF RESULTS OF COVID-19?######


a <- 1
for(a in 1:nrow(COVID1))
{
  #NAMES FOR PLOTS
  tmpdata.State <- COVID1[a,1]
  tmpdata.Country <- COVID1[a,2]
  # MISSING VALUES
  if(is.na(tmpdata.State)){
    #NAMING WITH COUNTRY
    tmpdata.Loc <- tmpdata.Country
  } else{ #PASTING STATE WITH COUNTRY
    tmpdata.Loc <- paste0(tmpdata.State,
                          "_",
                          tmpdata.Country)
  }
  # ROW DATA
  tmpdata <- COVID1[a,5:(ncol(COVID1)-1)]
  tmpdata1 <- COVID2[a,5:(ncol(COVID2)-1)]
  tmpdata2 <- COVID3[a,5:(ncol(COVID3)-1)]
  
  
  # take the transpose
  tmpdata <- t(tmpdata)
  tmpdata1 <-t(tmpdata1)
  tmpdata2<- t(tmpdata2)
 
  
  
  #SPECIAL CHARACTER
  tmpdata.Loc.clean <- gsub("[[:punct:]]", # PUNCTUATION REMOVAL
                            "", 
                            tmpdata.Loc) 
  
  # NAMING THE PLOT
  pltName <- paste0(wd,"/",tmpfolder4,"/",tmpdata.Loc.clean,"TOTAL PLOTS", ".png")
  
  #SAVING FILE NAME
  png(pltName) 
  
  #PLOT
  par(mfrow=c(1,3))
  {plot(tmpdata,
        main=tmpdata.Loc,
        pch=19,
        col='yellow',
        xlab="Days Since Start",
        ylab="COVID-19 TOTAL CONFIRMED CASES")
    
    plot(tmpdata,
         main=tmpdata.Loc,
         pch=19,
         col='red',
         xlab="Days Since Start",
         ylab="COVID-19 TOTAL DEATH CASES")
    
    plot(tmpdata,
         main=tmpdata.Loc,
         pch=19,
         col='green',
         xlab="Days Since Start",
         ylab="COVID-19 TOTAL RECOVERED CASES")
    
  
  dev.off() #TURN OFF THE PLOT AND SAVE IT 

  }
}
  
  
  a <- 1
  for(a in 1:nrow(difference1))
  {
    #NAMES FOR PLOTS
    tmpdata.State <- difference1[a,1]
    tmpdata.Country <- difference1[a,2]
    # MISSING VALUES
    if(is.na(tmpdata.State)){
      #NAMING WITH COUNTRY
      tmpdata.Loc <- tmpdata.Country
    } else{ #PASTING STATE WITH COUNTRY
      tmpdata.Loc <- paste0(tmpdata.State,
                            "_",
                            tmpdata.Country)
    }
    # ROW DATA
    tmpdata <- difference1[a, 3:(ncol(difference1)-1)]
    tmpdata1 <- difference2[a, 3:(ncol(difference2)-1)]
    tmpdata2 <- difference3[a, 3:(ncol(difference3)-1)]

    
    
    # take the transpose
    tmpdata <- t(tmpdata)
    tmpdata1 <-t(tmpdata1)
    tmpdata2<- t(tmpdata2)
    
    
    
    #SPECIAL CHARACTER
    tmpdata.Loc.clean <- gsub("[[:punct:]]", # PUNCTUATION REMOVAL
                              "", 
                              tmpdata.Loc) 
    
    # NAMING THE PLOT
    pltName <- paste0(wd,"/",tmpfolder4,"/",tmpdata.Loc.clean,"GROWTH PLOTS", ".png")
    
    #SAVING FILE NAME
    png(pltName) 
    
    #PLOT
    par(mfrow=c(1,3))
    {plot(tmpdata,
          main=tmpdata.Loc,
          pch=19,
          col='yellow',
          xlab="Days Since Start",
          ylab="COVID-19 CONFIRMED CASES GROWTH PER DAY")
      
      plot(tmpdata,
           main=tmpdata.Loc,
           pch=19,
           col='red',
           xlab="Days Since Start",
           ylab="COVID-19 DEATH CASES GROWTH PER DAY")
      
      plot(tmpdata,
           main=tmpdata.Loc,
           pch=19,
           col='green',
           xlab="Days Since Start",
           ylab="COVID-19 RECOVERED CASES GROWTH PER DAY ")
      
      
      dev.off() #TURN OFF THE PLOT AND SAVE IT 
    }
    
  }
    
    a <- 1
    for(a in 1:nrow(percentdif1))
    {
      #NAMES FOR PLOTS
      tmpdata.State <- percentdif1[a,1]
      tmpdata.Country <- percentdif1[a,2]
      # MISSING VALUES
      if(is.na(tmpdata.State)){
        #NAMING WITH COUNTRY
        tmpdata.Loc <- tmpdata.Country
      } else{ #PASTING STATE WITH COUNTRY
        tmpdata.Loc <- paste0(tmpdata.State,
                              "_",
                              tmpdata.Country)
      }
      # ROW DATA
      tmpdata <-  percentdif1[a, 3:(ncol(percentdif1)-1)]
      tmpdata1 <-  percentdif2[a, 3:(ncol(percentdif2)-1)]
      tmpdata2 <-  percentdif3[a, 3:(ncol(percentdif3)-1)]
      
      
      # take the transpose
      tmpdata <- t(tmpdata)
      tmpdata1 <-t(tmpdata1)
      tmpdata2<- t(tmpdata2)
      
      
      
      #SPECIAL CHARACTER
      tmpdata.Loc.clean <- gsub("[[:punct:]]", # PUNCTUATION REMOVAL
                                "", 
                                tmpdata.Loc) 
      
      # NAMING THE PLOT
      pltName <- paste0(wd,"/",tmpfolder4,"/",tmpdata.Loc.clean,"RATE PLOTS", ".png")
      
      #SAVING FILE NAME
      png(pltName) 
      
      #PLOT
      par(mfrow=c(1,3))
      {plot(tmpdata,
            main=tmpdata.Loc,
            pch=19,
            col='yellow',
            xlab="Days Since Start",
            ylab="COVID-19 Confirmed cases Growth rate")
        
        plot(tmpdata,
             main=tmpdata.Loc,
             pch=19,
             col='red',
             xlab="Days Since Start",
             ylab="COVID-19 Death cases Growth rate")
        
        plot(tmpdata,
             main=tmpdata.Loc,
             pch=19,
             col='green',
             xlab="Days Since Start",
             ylab="COVID-19 Recocvered Cases Growth rate")
        
        
        dev.off() #TURN OFF THE PLOT AND SAVE IT 
      }
      
      
    }   