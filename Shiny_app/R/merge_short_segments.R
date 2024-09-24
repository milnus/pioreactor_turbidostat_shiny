merge_short_segments <- function(segments, measurements, time_stamps, min_segment_length, delta, time_delta) {

  ### Function to merge short segments that are identified and potentially part of larger segments ###

  unique_segments <- unique(segments)
  for (seg in unique_segments) {
    seg_indices <- which(segments == seg)
    if (length(seg_indices) < min_segment_length) {
      if (seg_indices[1] > 1) {
        # Try merging with previous segment
        prev_seg <- segments[seg_indices[1] - 1]
        prev_seg_indices <- which(segments == prev_seg)
        if (measurements[seg_indices[1]] - measurements[prev_seg_indices[length(prev_seg_indices)]] < delta & 
            time_stamps[seg_indices[1]] - time_stamps[prev_seg_indices[length(prev_seg_indices)]] < time_delta) {
          # Merge with previous segment, including points in between
          merge_start <- prev_seg_indices[length(prev_seg_indices)]
          merge_end <- seg_indices[length(seg_indices)]
          segments[merge_start:merge_end] <- prev_seg
        } else if (seg_indices[length(seg_indices)] < length(segments)) {
          # Try merging with next segment
          next_seg <- segments[seg_indices[length(seg_indices)] + 1]
          next_seg_indices <- which(segments == next_seg)
          if (measurements[next_seg_indices[1]] - measurements[seg_indices[length(seg_indices)]] < delta & 
              time_stamps[next_seg_indices[1]] - time_stamps[seg_indices[length(seg_indices)]] < time_delta) {
            # Merge with next segment, including points in between
            merge_start <- seg_indices[1]
            merge_end <- next_seg_indices[length(next_seg_indices)]
            segments[merge_start:merge_end] <- next_seg
          }
        }
      }
    }
  }
  return(segments)
}