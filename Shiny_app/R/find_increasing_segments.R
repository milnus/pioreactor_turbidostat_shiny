find_increasing_segments <- function(measurements, time_stamps, delta, min_segment_length) {
  time_stamps <- time_stamps[!is.na(measurements)]
  measurements <- measurements[!is.na(measurements)]
  base_allowed_decreasing_count <- 40
  
  n <- length(measurements)
  segments <- numeric(n)
  segment_id <- 1
  segment_start <- 1
  increasing_count <- 0
  decreasing_count <- 0
  
  for (i in 2:n) {
    if (measurements[i] >= measurements[i-1]) {
      increasing_count <- increasing_count + 1
    } else {
      decreasing_count <- decreasing_count + 1
      
      # Check if decrease exceeds delta
      if (measurements[i-1] - measurements[i] >= delta) {
        print("Delta cut")
        # End current segment and start a new one
        segments[segment_start:(i-1)] <- segment_id
        segment_id <- segment_id + 1
        segment_start <- i
        increasing_count <- 0
        decreasing_count <- 0
      } else {
        # Check if too many decreases
        allowed_decreases <- floor(increasing_count / 2) + base_allowed_decreasing_count
        #if (decreasing_count > allowed_decreases) {
        #  print("Accumulation cut")
        #  # End current segment and start a new one
        #  segments[segment_start:(i-1)] <- segment_id
        #  segment_id <- segment_id + 1
        #  segment_start <- i
        #  increasing_count <- 0
        #  decreasing_count <- 0
        #}
      }
    }
  }
  
  # Assign segment ID to the last segment
  segments[segment_start:n] <- segment_id
  
  print(paste("pre merge:", length(unique(segments))))
  
  # Merge short segments
  merged_segments <- merge_short_segments(segments, measurements, min_segment_length, delta)
  print(paste("Segments past short merge:", length(unique(merged_segments))))
  
  # Merge segments that don't violate the delta rule
  merged_segments <- merge_segments(merged_segments, measurements, delta)
  print(paste("Segments post both mergers:", length(unique(merged_segments))))
  
  return(data.frame(time_stamps, merged_segments))
}