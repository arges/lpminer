# Launchpad Data mining in R
#
# 2012(c)
# Written by Chris J Arges <christopherarges@gmail.com>
#

# Clean environment.
rm(list=ls())

#limit <- "LIMIT 5000"
limit<-""

# GATHER data from database. -------------------------------------------------------
rename = "bugtask.id AS id,
	bug.id AS bug_id,
	bugtask.assignee AS bugtask_assignee,
	bugtask.date_created AS bugtask_date_created,
	bugtask.date_assigned AS bugtask_date_assigned,
	bugtask.date_closed AS bugtask_date_closed,
	bugtask.date_confirmed AS bugtask_date_confirmed,
	bugtask.date_fix_committed AS bugtask_date_fix_committed,
	bugtask.date_fix_released AS bugtask_date_fix_released,
	bugtask.date_incomplete AS bugtask_date_incomplete,
	bugtask.date_in_progress AS bugtask_date_in_progress,
	bugtask.date_left_closed AS bugtask_date_left_closed,
	bugtask.date_left_new AS bugtask_date_left_new,
	bugtask.date_triaged AS bugtask_date_triaged,
	bugtask.owner AS bugtask_owner,
	bugtask.status AS bugtask_status,
	bugtask.importance AS bugtask_importance,
	bugtask.target AS bugtask_target,
	bugtask.target_name AS bugtask_target_name,
	bug.date_created AS bug_date_created,
	bug.date_last_message AS bug_date_last_message,
	bug.date_last_updated AS bug_date_last_updated,
	bug.description AS bug_description,
	bug.duplicate_of AS bug_duplicate_of,
	bug.heat AS bug_heat,
	bug.latest_patch_uploaded AS bug_latest_patch_uploaded,
	bug.message_count AS bug_message_count,
	bug.number_of_duplicates AS bug_number_of_duplicates,
	bug.other_users_affected_count_with_dupes AS bug_other_users_affected_count_with_dupes,
	bug.owner AS bug_owner,
	bug.tags AS bug_tags,
	bug.title AS bug_title,
	bug.users_affected_count AS bug_users_affected_count,
	bug.users_affected_count_with_dupes AS bug_users_affected_count_with_dupes,
	bugtarget.name as bugtarget_name"
	
sql_query = paste("SELECT * FROM (SELECT", rename, "FROM bugtask ",
  "JOIN bug ON bugtask.bug = bug.id ",
  "JOIN bugtarget ON bugtask.target = bugtarget.id ",
  "ORDER BY bug.id) as bugtaskbug WHERE bug_id > 1",limit)
  
person_bugtask_assignee_totals_query = "
  SELECT person.id as id, person.name as name, count(*) as total FROM person,bugtask WHERE bugtask.assignee = person.id GROUP BY person.id ORDER BY total DESC"
  
person_query = "SELECT * FROM person"

require(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"), dbname = "shadowdb")

# Get all closed bugs, and all bugs.
bd_all <- dbGetQuery(con, sql_query)

# Get all bug statuses/importances.
bug_statuses <- dbGetQuery(con, "SELECT * FROM bugstatus")
bug_importances <- dbGetQuery(con, "SELECT * FROM bugimportance")

# Get totals of bugtask assignee totals per person
person_stats <- dbGetQuery(con, person_bugtask_assignee_totals_query)
people <- dbGetQuery(con, person_query)

# Get package mapping from .csv file.
pkg_mapping<-read.csv("package-team-mapping.csv")

# Helper Functions ------------------------------------------------------------

# Lookup package type from name.
lookup_package <- function(name) {
  # This function is a bit funky. If we can lookup directly return this,
  # Otherwise, strip version info and return that.
  result <- pkg_mapping[which(pkg_mapping$package == name),]
  if (length(result$team) == 0){
    f_name = unlist(strsplit(name, "-"))[1] # Strip version stuff.
    result <- pkg_mapping[which(pkg_mapping$package == f_name),]
    ret <- as.character(result$team)
  }
  ret <- as.character(result$team)
  if (length(ret) == 0) {
    return("other") # Default value.
  }
  return(ret)
}

conv_to_numeric_days <- function(timediff) { return(as.numeric(trunc(timediff, "day"))/60/60/24) }
remove_negative_values <- function(x) { return(x[which(x>0)]) }
remove_values_below <- function(x, cutoff) { return(x[which(x>cutoff)]) }

# From the help page for ?toupper
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

safeName <- function(x) {
  s<-gsub(" ","_",tolower(x))
  return(s)
}

calculate_person_stats_open <- function(person_id, field, value) {
  all_assigned <- which(bd_open$bugtask_assignee == person_id)
  return(length(which(bd_open[all_assigned,][[field]] == value)))
}

# CALCULATE additional data. ------------------------------------------------------

calculate_extra_data <- function(bd) {
  # What are some of the intervals between the bug states?
  bd$days_from_created_to_close <- conv_to_numeric_days(bd$bugtask_date_closed - bd$bugtask_date_created)
  bd$days_from_created_to_fix_released <- conv_to_numeric_days(bd$bugtask_date_fix_released - bd$bugtask_date_created)
  bd$days_from_created_to_triaged <- conv_to_numeric_days(bd$bugtask_date_triaged - bd$bugtask_date_created)
  bd$days_from_created_to_assigned <- conv_to_numeric_days(bd$bugtask_date_assigned - bd$bugtask_date_created)
  bd$days_from_triaged_to_assigned <- conv_to_numeric_days(bd$bugtask_date_assigned - bd$bugtask_date_triaged)
  bd$days_from_assigned_to_closed <- conv_to_numeric_days(bd$bugtask_date_closed - bd$bugtask_date_assigned)

  # Convert into catagorical variables. (sapply would be nice here)
  for (s in bug_statuses$id) {
    bd[[paste("is_",safeName(bug_statuses[s,]$name),sep="")]] <- bd$bugtask_status == s
  }

  # Which days of the week are these transitions most likely?
  bd$bugtask_weekday_created <- weekdays(bd$bugtask_date_created)
  bd$bugtask_weekday_closed <- weekdays(bd$bugtask_date_closed)
  bd$bugtask_weekday_assigned <- weekdays(bd$bugtask_date_assigned)

  # Calculate the numbers of letters in the title and description.
  bd$description_wc <- nchar(bd$bug_description)
  bd$title_wc <- nchar(bd$bug_title)

  # Create package mapping row
  bd$package_type <- sapply(bd$bugtarget_name, lookup_package)
  
  # return modified value
  return(bd)
}

bd_all <- calculate_extra_data(bd_all)
bd_closed <- bd_all[which(!is.na(bd_all$bugtask_date_closed)),]
bd_open <- bd_all[which(is.na(bd_all$bugtask_date_closed)),]

# determine current load for person stats
person_stats[["total_open_cases"]] <- sapply(person_stats$id, y <- function(person_id) {length(which(bd_open$bugtask_assignee == person_id))})
person_stats[["total_in_progress_cases"]] <- sapply(person_stats$id, calculate_person_stats_open, field="bugtask_status", value=9)

