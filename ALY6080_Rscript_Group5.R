#Load the excel file 
#Step 1- Getting the excel data files
att_rev_avail <- read_excel("Att_Rev_Availability.xlsx")
lacp_ranking <- read_excel("/Users/shanu/Desktop/LACP_Ranking.xlsx")
lacp_avail_code <- read_excel("/Users/shanu/Desktop/LACP_Availability_Codes.xlsx")

#Step 2- Merging the data
merged_data <- lacp_ranking %>%
  +     left_join(att_rev_avail, by = c("attendee_name", "reviewer_name")) %>%
  +     left_join(lacp_avail_code, by = "availability_id")

#Step 3- Define the AM and PM slots
am_slots <- c("08:30", "09:00", "09:30", "10:15", "10:45", "11:15", "12:00", "12:30")
pm_slots <- c("13:00", "13:30", "14:00", "14:45", "15:15", "15:45", "16:30", "17:00")

#Step 4- Function to check if two time slots overlap
is_overlapping <- function(slot1, slot2) {
  +     slot1_start <- as.POSIXct(paste("2023-06-22", slot1), format = "%Y-%m-%d %H:%M")
  +     slot1_end <- slot1_start + minutes(30)
  +     slot2_start <- as.POSIXct(paste("2023-06-22", slot2), format = "%Y-%m-%d %H:%M")
  +     slot2_end <- slot2_start + minutes(30)
  +     
    +     return(slot1_start <= slot2_end && slot2_start <= slot1_end)
  + }

#Step 5- Function to find available reviewers for an artist
> find_available_reviewers <- function(artist_row) {
  +     reviewers <- merged_data %>%
    +         filter(
      +             reviewer_id != artist_row$reviewer_id,  # Exclude the artist itself
      +             reviews %in% artist_row$reviews,  # Match the required number of reviews
      +             rv_availability == artist_row$at_availability,  # Match the availability
      +             at_availability == artist_row$rv_availability,  # Match the reviewer availability
      +             reviewer_name %in% unique(merged_data$reviewer_name)  # Exclude already assigned reviewers
      +         )
  +     
    +     return(reviewers)
  + }


#Step 6- Schedule the artists with reviewers
scheduled_data <- merged_data %>% mutate(scheduled = FALSE)

for (i in 1:nrow(scheduled_data)) {
    +     artist_row <- scheduled_data[i, ]
    +     
      +     if (!artist_row$scheduled) {
        +         available_reviewers <- find_available_reviewers(artist_row)
        +         
          +         if (nrow(available_reviewers) > 0) {
            +             # Find the preferred reviewer based on ranking
              +             preferred_reviewer <- available_reviewers %>%
                +                 filter(ranking == min(ranking))
              +             
                +             if (nrow(preferred_reviewer) > 1) {
                  +                 # If multiple preferred reviewers, choose the one with 8 reviews preference
                    +                 preferred_reviewer <- preferred_reviewer %>%
                      +                     filter(reviews == max(reviews))
                    +             }
              +             
                +             if (nrow(preferred_reviewer) > 1) {
                  +                 # If still multiple reviewers, choose the one alphabetically first
                    +                 preferred_reviewer <- preferred_reviewer %>%
                      +                     filter(reviewer_name == min(reviewer_name))
                    +             }
              +             
                +             if (nrow(preferred_reviewer) > 0) {
                  +                 # Assign the artist to the preferred reviewer
                    +                 scheduled_data[i, "scheduled"] <- TRUE
                    +                 scheduled_data[i, "assigned_reviewer_id"] <- preferred_reviewer$reviewer_id
                    +                 scheduled_data[i, "assigned_reviewer_name"] <- preferred_reviewer$reviewer_name
                    +             }
              +         }
        +     }
    + }


#Step 7- Output the scheduled data
scheduled_data <- scheduled_data %>%
  +     select(attendee_id, attendee_name, assigned_reviewer_id, assigned_reviewer_name) %>%
  +     filter(scheduled) %>%
  +     arrange(attendee_name)


#Step 8- Printing the Schedule
print(scheduled_data)

#Step 9 Create and print a schedule table
schedule_table <- data.frame(
  +     Artist = scheduled_data$attendee_name,
  +     Reviewer = scheduled_data$assigned_reviewer_name
  + )

print(schedule_table)
