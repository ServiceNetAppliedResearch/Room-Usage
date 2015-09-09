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
	#instantiate date list
	date <- as.Date(date(),"%Y:%m:%d")
	room <- NULL
	location <- as.character(NULL)
	schedule <- NULL
	for(i in 1:nrow(working_df)){
		date <- c(date, as.Date(working_df$startDateTime[i]))
		location <- c(location, as.character(working_df$location[i]))
		room <- c(room, working_df$roomNumber[i])
		schedule <- c(schedule, list(getSchedule(working_df)))
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

getSchedule <- function(working_df){
	schedule <- c(1:24)
	return(schedule)
}