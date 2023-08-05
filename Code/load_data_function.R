library(dplyr)

file_path <- '~/OMSA/MGT6203/GroupProject/Team-39/Data/hotel_bookings.csv'

load_cleaned_df <- function(file_path) {
  
  df <- read.csv(file_path)
  
  factor_cols <- c(
        "hotel",
        "meal",
        "market_segment",
        "distribution_channel",
        "reserved_room_type",
        "assigned_room_type",
        "deposit_type",
        "customer_type",
        "arrival_date_month"
      )
  logical_cols <- c(
    'is_repeated_guest',
    'is_canceled'
  )
  numeric_cols <- c(
      'lead_time',
      'stays_in_weekend_nights',
      'stays_in_week_nights',
      'adults',
      'children',
      'babies',
      'previous_cancellations',
      'previous_bookings_not_canceled',
      'booking_changes',
      'days_in_waiting_list',
      'adr',
      'required_car_parking_spaces',
      'total_of_special_requests'
    )
  
  all_cols_used <- c(logical_cols, numeric_cols, factor_cols)
  
  df2 <- df %>%
    select(all_of(all_cols_used)) %>%
    mutate(is_reserved_and_assigned_room_same = reserved_room_type == assigned_room_type) %>%
    filter(adr < 4000, 
           adults <= 4,
           babies <= 2,
           booking_changes <= 5,
           children <= 4, 
           days_in_waiting_list <= 400,
           previous_bookings_not_canceled <= 10,
           previous_cancellations <= 10, 
           required_car_parking_spaces <= 3,
           days_in_waiting_list <= 20,
           stays_in_weekend_nights <= 5,
           total_of_special_requests <= 6,
           !(reserved_room_type %in% c('P', 'L')),
           !(assigned_room_type %in% c('P', 'L')),
           distribution_channel != 'Undefined') %>%
    mutate_at(factor_cols, as.factor) %>%
    mutate_at(logical_cols, as.logical)
  
  return(df2)
}
