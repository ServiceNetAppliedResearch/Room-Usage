#Create Schedule "heatmap"
#Input -> (working_df = data frame)
#Output -> (heatmap_df[Room, Date, Schedule] = data frame[num, chr, list])
# To construct heatmap_df, we need to first instantiate the df with room and date.
#	There should be a pairing of each room and every date (9/01/14 - 100, 9/01/14 - 101, etc.)
#	Once we have that, we need to add a column to the df which will consist of a list of
#	24-numeral vectors. Each of the numerals in the vector will represent the number of
#	scheduled appointments in an hour. With the first numeral representing the number of 
#	appointments at Midnight, and the last representing the number of appointments at 23(11:00pm). 
#	A 0 will represent no appointments.
getScheduleHeatmap <- function(working_df){
	#!Want to get schedules first, work off of that restricted data frame.
	#instantiate date list
	date <- as.Date(date(),"%Y:%m:%d")
	room <- NULL
	location <- as.character(NULL)
	schedule <- NULL
	for(i in 1:nrow(working_df)){
		date <- c(date, as.Date(working_df$startDateTime[i]))
		location <- c(location, as.character(working_df$location[i]))
		room <- c(room, working_df$roomNumber[i])
		schedule <- c(schedule, list(getSchedule(working_df[i,])))
	}
	# remove Null start from lists
	date <- date[!is.na(date)]
	location <- location[!is.na(location)]
	room <- room[!is.na(room)]
	# create data frame
	heatmap_df <- data.frame(date,location,room, stringsAsFactors = FALSE)
	heatmap_df$schedule <- schedule
	return(heatmap_df)
}

#Create a "schedule" for each room/time in the data frame
#Input -> (working_df = one row subset of a data frame)
#Output -> (schedule = c(num[24]))
# Produces a list of 24 numbers. Numbers are occupied hours for each row of df
getSchedule <- function(working_df_ss){
	schedule <- rep(0, 24)
	start <- as.integer(format(working_df_ss[1,]$startDateTime, "%H"))
	end <- as.integer(format(working_df_ss[1,]$endDateTime, "%H"))
	if(end-start > 1){
		schedule[start:end-1] <- 1
	}else{schedule[start] <- 1}
	return(schedule)
}

#Direction to head:
#Reduce("+", test$schedule) works perfectly. BUT, it does it over a whole data frame.
# So what I should probably do is get something that basically says
# first: here are all the rows with duplicated date/location/time
# second: for each unique duplicated date/location/time, create a subset_df
# add date/location/room to return vectors that I use to build up return df
# use Reduce("+", subset_df$schedule), add result to schedule vector used in return df.
# I think this weird thing should basically make everything work.
#Add together schedules from the same location/room/date.
#Input -> (working_df = data frame)
#Output -> (scheduleCompressed = data frame with only unqiue room/date/locations)
#Super clean this up, there's so much that could obviously be done more effectively
# with variables.
compressSchedules <- function(working_df, compressBy = c("date","location","room")){
	date <- as.Date(date(),"%Y:%m:%d")
	room <- NULL
	location <- as.character(NULL)
	schedule <- NULL

	if(all(compressBy == c(""))){
		duplicateEntries <- working_df[duplicated(working_df[c("date","location","room")]) | duplicated(working_df[c("date","location","room")], fromLast=TRUE),]
		uniqueDupeEntries <- working_df[!duplicated(working_df[c("date","location","room")]),]
	}
	if(all(compressBy == c("date","location"))){
		duplicateEntries <- working_df[duplicated(working_df[c("date","location")]) | duplicated(working_df[c("date","location")], fromLast=TRUE),]
		uniqueDupeEntries <- working_df[!duplicated(working_df[c("date","location")]),]
	}
	if(all(compressBy == c("date"))){
		duplicateEntries <- working_df[duplicated(working_df[c("date")]) | duplicated(working_df[c("date")], fromLast=TRUE),]
		uniqueDupeEntries <- working_df[!duplicated(working_df[c("date")]),]
	}
	if(all(compressBy == c("location"))){
		duplicateEntries <- working_df[duplicated(working_df[c("location")]) | duplicated(working_df[c("location")], fromLast=TRUE),]
		uniqueDupeEntries <- working_df[!duplicated(working_df[c("location")]),]
	}
	if(all(compressBy == c("room"))){
		duplicateEntries <- working_df[duplicated(working_df[c("room")]) | duplicated(working_df[c("room")], fromLast=TRUE),]
		uniqueDupeEntries <- working_df[!duplicated(working_df[c("room")]),]
	}
	for(i in 1:nrow(uniqueDupeEntries)){
		#See if you can do this more sensibly, with variables.
		#if(length(compressBy)>=3){subset <- working_df[which(working_df[,compressBy[1]] == uniqueDupeEntries[i,compressBy[1]] & working_df[,compressBy[2]] == uniqueDupeEntries[i,compressBy[2]] & working_df[,compressBy[3]] == uniqueDupeEntries[i,compressBy[3]]),]}
		#if(length(compressBy)==2){subset <- working_df[which(working_df[,compressBy[1]] == uniqueDupeEntries[i,compressBy[1]] & working_df[,compressBy[2]] == uniqueDupeEntries[i,compressBy[2]]),]}
		#if(length(compressBy)==1){subset <- working_df[which(working_df[,compressBy[1]] == uniqueDupeEntries[i,compressBy[1]]),]}
		#if(length(compressBy)<1){subset <- working_df[which(working_df[,compressBy[1]] == uniqueDupeEntries[i,compressBy[1]] & working_df[,compressBy[2]] == uniqueDupeEntries[i,compressBy[2]] & working_df[,compressBy[3]] == uniqueDupeEntries[i,compressBy[3]]),]}
		#print(subset)

		if(all(compressBy == c(""))){subset <- working_df[which(working_df$date == uniqueDupeEntries[i,]$date & working_df$location == uniqueDupeEntries[i,]$location & working_df$room == uniqueDupeEntries[i,]$room),]}
		if(length(compressBy)==3){subset <- working_df[which(working_df$date == uniqueDupeEntries[i,]$date & working_df$location == uniqueDupeEntries[i,]$location & working_df$room == uniqueDupeEntries[i,]$room),]}
		if(all(compressBy == c("date", "location"))){subset <- working_df[which(working_df$date == uniqueDupeEntries[i,]$date & working_df$location == uniqueDupeEntries[i,]$location),]}
		if(all(compressBy == c("date"))){subset <- working_df[which(working_df$date == uniqueDupeEntries[i,]$date),]}
		if(all(compressBy == c("location"))){subset <- working_df[which(working_df$location == uniqueDupeEntries[i,]$location),]}
		if(all(compressBy == c("room"))){subset <- working_df[which(working_df$room == uniqueDupeEntries[i,]$room),]}

		date <- c(date, as.Date(subset$date[1]))
		location <- c(location, as.character(subset$location[1]))
		room <- c(room, subset$room[1])
		schedule <- c(schedule, list(Reduce("+", subset$schedule)))
	}
	# remove Null start from lists
	date <- date[!is.na(date)]
	location <- location[!is.na(location)]
	room <- room[!is.na(room)]
	# create data frame
	if(all(compressBy == c(""))){scheduleCompressed <- data.frame(date,location,room, stringsAsFactors = FALSE)}
	if(length(compressBy)==3){scheduleCompressed <- data.frame(date,location,room, stringsAsFactors = FALSE)}
	if(all(compressBy == c("date", "location"))){scheduleCompressed <- data.frame(date,location, stringsAsFactors = FALSE)}
	if(all(compressBy == c("date"))){scheduleCompressed <- data.frame(date, stringsAsFactors = FALSE)}
	if(all(compressBy == c("location"))){scheduleCompressed <- data.frame(location, stringsAsFactors = FALSE)}
	if(all(compressBy == c("room"))){scheduleCompressed <- data.frame(room, stringsAsFactors = FALSE)}

	scheduleCompressed$schedule <- schedule
	return(scheduleCompressed)
}