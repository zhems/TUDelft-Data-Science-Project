# Importing libraries and reading data
library(ggplot2)
library(gridExtra)
library(reshape)
library(lubridate)
library(zoo)
library(plyr)
library(tidyr)
m46 <- read.csv("data/MP46.csv")
m60 <- read.csv("data/MP60.csv")
switch <- read.csv("data/switching_data.csv")

# Transforming date and time variables
m46$StartTime <- as.character(m46$StartTime)
m60$StartTime <- as.character(m60$StartTime)
wrongtime <- "26/03/2017 2:50:00"
correcttime <- "26/03/2017 1:50:00"
m46[m46$StartTime == wrongtime, "StartTime"] <- correcttime
m60[m60$StartTime == wrongtime, "StartTime"] <- correcttime
m46$StartTime <- as.POSIXct(m46$StartTime, format = "%d/%m/%Y %H:%M:%S")
m60$StartTime <- as.POSIXct(m60$StartTime, format = "%d/%m/%Y %H:%M:%S")

switch$datetime <- paste(switch[,1], switch$Time)
switch$datetime <- as.POSIXct(switch$datetime, format = "%d/%m/%Y %H:%M:%S")
switch$date <- format(switch[,1], format = "%d/%m/%Y")
switch$time <- format(switch$Time, format = "%H:%M:%S")
switch[,1] <- NULL
switch$Time <- NULL
switch$datetime10mins <- floor_date(switch$datetime, "10 minutes")

# Separating dataset based on filters
B11 <- switch[switch$Element=="MDC_MDC AC Filter 11 VS",]
B12 <- switch[switch$Element=="MDC_MDC AC Filter 12 VS",]
B21 <- switch[switch$Element=="MDC_MDC AC Filter 21 VS",]
B22 <- switch[switch$Element=="MDC_MDC AC Filter 22 VS",]

# Adding name for filter as variable
B11$B11 <- B11$status
B12$B12 <- B12$status
B21$B21 <- B21$status
B22$B22 <- B22$status

# Removing second of each repeated status
B11 <- B11[seq(1, nrow(B11), 2),] 
B12 <- B12[seq(1, nrow(B12), 2),] 
B21 <- B21[seq(1, nrow(B21), 2),] 
B22 <- B22[seq(1, nrow(B22), 2),]

# Removing switch timings mulitple entries per 10 mins 
B11extra <- data.frame(table(B11$datetime10mins))
B11extra <- B11extra[B11extra$Freq>1,]
B11extra$Var1 <- as.POSIXct(B11extra$Var1)
B11extra <- B11[B11$datetime10mins %in% B11extra$Var1,]
B11extra <- B11extra[-c(7, 9, 13, 15, 17),] #choosing the rows to be removed
B11 <- B11[!(rownames(B11) %in% rownames(B11extra)),]

# Removing switch timings mulitple entries per 10 mins 
B12extra <- data.frame(table(B12$datetime10mins))
B12extra <- B12extra[B12extra$Freq>1,]
B12extra$Var1 <- as.POSIXct(B12extra$Var1)
B12extra <- B12[B12$datetime10mins %in% B12extra$Var1,]
B12extra <- B12extra[-c(2, 4, 6),] #choosing the rows to be removed
B12 <- B12[!(rownames(B12) %in% rownames(B12extra)),]

# Removing switch timings mulitple entries per 10 mins 
B21extra <- data.frame(table(B21$datetime10mins))
B21extra <- B21extra[B21extra$Freq>1,]
B21extra$Var1 <- as.POSIXct(B21extra$Var1)
B21extra <- B21[B21$datetime10mins %in% B21extra$Var1,]
B21extra <- B21extra[-c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 41, 43),] #choosing the rows to be removed
B21 <- B21[!(rownames(B21) %in% rownames(B21extra)),]

# Removing switch timings mulitple entries per 10 mins 
B22extra <- data.frame(table(B22$datetime10mins))
B22extra <- B22extra[B22extra$Freq>1,]
B22extra$Var1 <- as.POSIXct(B22extra$Var1)
B22extra <- B22[B22$datetime10mins %in% B22extra$Var1,]
B22extra <- B22extra[-c(2, 4, 6, 8, 10, 12),] #choosing the rows to be removed
B22 <- B22[!(rownames(B22) %in% rownames(B22extra)),]

# Choosing the columns to keep
B11 <- B11[,c(3,6,7)]
B12 <- B12[,c(3,6,7)]
B21 <- B21[,c(3,6,7)]
B22 <- B22[,c(3,6,7)]

# Merging with Voltage data, m46
B11$B11time <- B11$datetime
B11$datetime <- NULL
m46 <- merge(x=m46, y=B11, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)

B12$B12time <- B12$datetime
B12$datetime <- NULL
m46 <- merge(x=m46, y=B12, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)

B21$B21time <- B21$datetime
B21$datetime <- NULL
m46 <- merge(x=m46, y=B21, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)

B22$B22time <- B22$datetime
B22$datetime <- NULL
m46 <- merge(x=m46, y=B22, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)
m46 <- m46 %>% fill(B11, B11time, B12, B12time, B21, B21time, B22, B22time)

# Merging with Voltage data, m60
m60 <- merge(x=m60, y=B11, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)
m60 <- merge(x=m60, y=B12, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)
m60 <- merge(x=m60, y=B21, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)
m60 <- merge(x=m60, y=B22, by.x="StartTime", by.y="datetime10mins", all.x = TRUE)
m60 <- m60 %>% fill(B11, B11time, B12, B12time, B21, B21time, B22, B22time)

# Filling missing values using last activated time
B11 <- B11[B11$datetime10mins < m46[1,1],]
B12 <- B12[B12$datetime10mins < m46[1,1],]
B21 <- B21[B21$datetime10mins < m46[1,1],]
B22 <- B22[B22$datetime10mins < m46[1,1],]

B11 <- B11[nrow(B11),]
B12 <- B12[nrow(B12),]
B21 <- B21[nrow(B21),]
B22 <- B22[nrow(B22),]

m46[is.na(m46$B11), "B11"] <- B11$B11
m46[is.na(m46$B11time), "B11time"] <- B11$B11time
m46[is.na(m46$B12), "B12"] <- B12$B12
m46[is.na(m46$B12time), "B12time"] <- B12$B12time
m46[is.na(m46$B21), "B21"] <- B21$B21
m46[is.na(m46$B21time), "B21time"] <- B21$B21time
m46[is.na(m46$B22), "B22"] <- B22$B22
m46[is.na(m46$B22time), "B22time"] <- B22$B22time

m60[is.na(m60$B11), "B11"] <- B11$B11
m60[is.na(m60$B11time), "B11time"] <- B11$B11time
m60[is.na(m60$B12), "B12"] <- B12$B12
m60[is.na(m60$B12time), "B12time"] <- B12$B12time
m60[is.na(m60$B21), "B21"] <- B21$B21
m60[is.na(m60$B21time), "B21time"] <- B21$B21time
m60[is.na(m60$B22), "B22"] <- B22$B22
m60[is.na(m60$B22time), "B22time"] <- B22$B22time

write.csv(m46, "data/combined/46combined.csv", row.names=FALSE)
write.csv(m60, "data/combined/60combined.csv", row.names=FALSE)
write.csv(B11extra, "data/combined/B11extra.csv", row.names=FALSE)
write.csv(B12extra, "data/combined/B12extra.csv", row.names=FALSE)
write.csv(B21extra, "data/combined/B21extra.csv", row.names=FALSE)
write.csv(B22extra, "data/combined/B22extra.csv", row.names=FALSE)