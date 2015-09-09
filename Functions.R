#Just pseudocode functions for now, to get an idea of what I want to be able to do with this room pull
#Packages:
#Get med clinic rooms, third sheet in the excel files.
#Get operating hours all set so I know when things should be turning up.
#
#install.packages("foreach")
#install.packages("plyr")
#Data Structures:
# test_df<-working_df[working_df$location == "591 - Holyoke" & as.Date(working_df$startDateTime) == "2015-03-06" & working_df$roomNumber == 1,]

#Import Data
#Input -> (filename[text])
#Output -> (working_df)[data frame]
# Input is a filename pointing to some .csv on disk that has data from room pull from eHana.
# Output is a data frame with all that data.
importData <- function(filename)
{
	#read data from given filename
	working_df<-read.csv(c("Z:\\Access to Care\\Room Usage\\Data Analysis\\Data\\RoomUsageTrimmed.csv"),colClasses=c("POSIXct","POSIXct","character","numeric"))
	return(working_df)
}

#Get Number of Unique Rooms
#Input -> (working_df[data frame])
#Output -> (int)
getNumUniqueRooms <- function(working_df)
{
	#access working_df, use getUnique function on roomNumber?
	uniqueRooms = working_df[!duplicated(working_df[c("location","roomNumber")]),]
	numUniqueRooms = nrow(uniqueRooms)
	return(numUniqueRooms)
}

#Get list of Unique Rooms
#Input -> (working_df[data frame])
#Output -> (uniqueRooms[list])
getUniqueRooms <- function(working_df)
{
	#access working_df, use getUnique function on roomNumber?
	uniqueRooms = working_df[!duplicated(working_df[c("location","roomNumber")]),c("location", "roomNumber")]
	return(uniqueRooms)
}

#Get Number of Unique Rooms
#Input -> (working_df[data frame])
#Output -> (uniqueRooms[vector])
getUniqueDates <- function(working_df)
{
	uniqueDates <- as.Date(working_df[!duplicated(as.Date(working_df[,"startDateTime"])),"startDateTime"])
	uniqueDates = sort(uniqueDates)
	return(uniqueDates)
}

#Get Num Room Uses
#Input -> (location[text], working_df[data frame])
#Output -> (roomUses_df[data frame])
# Takes a data frame of all the appointments
# Produces a data frame with an additional column V1 which is number of appointments for that room.
getNumRoomUses <- function(working_df)
{
	#Might want to add conditional options like
	# - Date range
	# - Specific location only
	roomUses_df <- ddply(working_df,.(location, roomNumber),nrow)
	return(roomUses_df)
}

#Get Hours in Use
#Input -> (working_df[data frame])
#Output -> (hoursInUseD_df[data frame])
# Takes input of working_df. 
# Produces the total number of hours all rooms have been booked for each day in the time period
# of the working_df
getHoursInUseByDate <- function(working_df)
{
	t = Sys.time()
	i = 1
	#Get each unique date
	uniqueDate <- getUniqueDates(working_df)
	#Allocate vector for hoursUsed
	hoursUsed <- difftime(rep(t,length(uniqueDate)), t, units = 'hours')
	#step through the unique dates
	foreach(uniqueDate = uniqueDate) %do% {
		#subset the data frame to consider only the startDateTime and endDateTime from the date
		#	currently under consideration
		startEndSubset_df <- working_df[as.Date(working_df[,"startDateTime"])==uniqueDate,c("startDateTime","endDateTime")]
		#do difftime on the columns of this subset
		timeDiffs <- difftime(startEndSubset_df$endDateTime,startEndSubset_df$startDateTime, units = 'hours')
		#get sum of timeDiffs
		totalTimeDiff <- sum(timeDiffs)
		hoursUsed[i] <- totalTimeDiff
		i <- i+1
	}		
	uniqueDate <- getUniqueDates(working_df)
	hoursInUseD_df <- data.frame(uniqueDate,round(hoursUsed,1))
	return(hoursInUseD_df)
}

#Get hours in use by room
#Input -> (working_df[data frame])
#Output -> (hoursInUseR_df[data frame])
# Takes input of working_df. 
# Produces the total number of hours each room has been booked for the time period
# of the working_df
#Definitively not the best way to do this, take a second look later.
getHoursInUseByRoom <- function(working_df)
{
	t = Sys.time()
	i = 1
	#Get each unique date
	uniqueRoom <- getUniqueRooms(working_df)
	uniqueRoomPasted <- paste(uniqueRoom$location, uniqueRoom$roomNumber)
	#Allocate vector for hoursUsed
	hoursUsed <- difftime(rep(t,length(uniqueRoom)), t, units = 'hours')
	#step through the unique dates
	foreach(uniqueRoomPasted = uniqueRoomPasted) %do% {
		#subset the data frame to consider only the startDateTime and endDateTime from the date
		#	currently under consideration
		startEndSubset_df <- working_df[paste(working_df$location,working_df$roomNumber) == uniqueRoomPasted,c("startDateTime","endDateTime")]
		#do difftime on the columns of this subset
		timeDiffs <- difftime(startEndSubset_df$endDateTime,startEndSubset_df$startDateTime, units = 'hours')
		#get sum of timeDiffs
		totalTimeDiff <- sum(timeDiffs)
		hoursUsed[i] <- totalTimeDiff
		i <- i+1
	}		
	
	uniqueRoom <- getUniqueRooms(working_df)
	hoursInUseR_df <- data.frame(uniqueRoom,round(hoursUsed,1))
	return(hoursInUseR_df)
}
