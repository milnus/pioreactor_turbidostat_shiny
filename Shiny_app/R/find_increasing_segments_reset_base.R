find_increasing_segments_reset_base <- function(measurements, time_stamps, delta, time_delta, 
                                                min_segment_length, index_for_exp_growth_end) {
  ### Function to find segments of exponential growth across a set of turbidostat dilutions ###
  time_stamps <- time_stamps[!is.na(measurements)]
  measurements <- measurements[!is.na(measurements)]
  # allowed_decreases <- 10
  
  n <- length(measurements)
  segments <- numeric(n)
  segment_id <- 2
  segment_start <- index_for_exp_growth_end+1
  increasing_count <- 0
  # decreasing_count <- 0
  decrease_sum <- 0
  last_top_index <- segment_start
  last_top <- measurements[last_top_index]
  cum_decrease_last_top <- 0
  
  # Set the initial exponential growth as the first segment
  segments[1:index_for_exp_growth_end] <- 1
  
  for (i in 2:n) {
    # Check for new top
    if (measurements[i] >= last_top | i > last_top_index+5){
      last_top <- measurements[i]
      last_top_index <- i
      cum_decrease_last_top <- 0
    }
    
    if (measurements[i] >= measurements[i-1]) {
      increasing_count <- increasing_count + 1
      
      # Store the current measurement as the new top value and index
      # last_top <- measurements[i]
      # last_top_index <- i
      # cum_decrease_last_top <- 0
      
      decrease_sum <- 0
      # decreasing_count <- 0
      # Subtract the increase from the decreasing sum
      # decrease_sum <- decrease_sum - abs(measurements[i] - measurements[i-1])
      # decrease_sum <- ifelse(decrease_sum < 0, 0, decrease_sum)
    } else {
      cum_decrease_last_top <- cum_decrease_last_top + abs(last_top - measurements[i])
      # decreasing_count <- decreasing_count + 1
      # Increase the cumulative decrease sum
      decrease_sum <- decrease_sum + abs(measurements[i] - measurements[i-1])
      
      # Check if the decrease from previous measurement exceeds delta
      if (measurements[i-1] - measurements[i] >= delta) {
        print("Delta cut")
        # End current segment and start a new one
        segments[segment_start:(i-1)] <- segment_id
        segment_id <- segment_id + 1
        segment_start <- i
        increasing_count <- 0
        # decreasing_count <- 0
        decrease_sum <- 0
        cum_decrease_last_top <- 0
        last_top <- measurements[i-1]
        last_top_index <- i-1
      } else {
        # Check if too many decreases
        # allowed_decreases <- floor(increasing_count / 3) + base_allowed_decreasing_count
        # if (decreasing_count > allowed_decreases) {
        #   print("Accumulation cut")
        #   # End current segment and start a new one
        #   segments[segment_start:(i-1)] <- segment_id
        #   segment_id <- segment_id + 1
        #   segment_start <- i
        #   # increasing_count <- 0
        #   decreasing_count <- 0
        # Check if accumulated decrease is larger than delta
        if (decrease_sum > delta) {
          print("Sum cut")
          # End current segment and start a new one
          segments[segment_start:(i-1)] <- segment_id
          segment_id <- segment_id + 1
          segment_start <- i
          
          # Reset the sumarised decrease
          decrease_sum <- 0
          cum_decrease_last_top <- 0
          last_top <- measurements[i-1]
          last_top_index <- i-1
        } else {
          # print(paste("cum_decrease_last_top:", cum_decrease_last_top))
          # check if the cumulative deacrease since last peak is larger than given value
          if (cum_decrease_last_top > 1.0 *delta) { # 2.0 worked well
            print("Size last peak break")
            # End current segment and start a new one
            # segments[segment_start:(last_top_index)] <- segment_id
            # segment_id <- segment_id + 1
            # segment_start <- last_top_index + 1
            # 
            # # Reset the sumarised decrease
            # decrease_sum <- 0
            # cum_decrease_last_top <- 0
            # last_top <- measurements[i]
            # last_top_index <- i
          } else {
            # Check if the time delta and od delta are large and can be called a segment.
            if (time_stamps[i] - time_stamps[i-1] >= time_delta & measurements[i-1] - measurements[i] >= 0.5 * delta) {
              print("Time break")
              
              # End current segment and start a new one
              segments[segment_start:(i-1)] <- segment_id
              segment_id <- segment_id + 1
              segment_start <- i
              
              # Reset the sumarised decrease
              decrease_sum <- 0
              cum_decrease_last_top <- 0
              last_top <- measurements[i-1]
              last_top_index <- i-1
            }
            
          }
        }
      }
    }
  }
  
  # Assign segment ID to the last segment
  segments[segment_start:n] <- segment_id
  
  # Merge short segments
  merged_segments <- merge_short_segments(segments = segments, 
                                          measurements = measurements, time_stamps = time_stamps, 
                                          min_segment_length = min_segment_length, 
                                          delta = delta, time_delta = time_delta)
  
  # merged_segments <- merge_leveled_segments(segments = merged_segments, measurements = measurements, delta)
  
  return(data.frame(time_stamps, merged_segments))
}