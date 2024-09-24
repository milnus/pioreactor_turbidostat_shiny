merge_segments <- function(segments, measurements, delta) {
  unique_segments <- unique(segments)
  n_segments <- length(unique_segments)
  
  # We'll use a logical vector to keep track of which segments have been merged
  merged <- logical(n_segments)
  
  for (i in 1:(n_segments - 1)) {
    if (merged[i]) next  # Skip if this segment has already been merged
    
    current_seg <- unique_segments[i]
    next_seg <- unique_segments[i + 1]
    
    current_indices <- which(segments == current_seg)
    next_indices <- which(segments == next_seg)
    
    # Check if merging would violate the delta rule
    last_current <- max(current_indices)
    first_next <- min(next_indices)
    
    if (measurements[first_next] - measurements[last_current] >= -delta) {
      # Merge the segments
      segments[current_indices] <- current_seg
      segments[next_indices] <- current_seg
      
      # If there are points between segments, include them in the merged segment
      if (first_next - last_current > 1) {
        segments[(last_current + 1):(first_next - 1)] <- current_seg
      }
      
      # Mark the next segment as merged
      merged[i + 1] <- TRUE
    }
  }
  
  return(segments)
}