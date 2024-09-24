# maximum and minimum detection algorithm from: https://gist.github.com/dgromer/ea5929435b8b8c728193
# Algorithm adapted from this blogpost: https://billauer.co.il/blog/2009/01/peakdet-matlab-octave/
peakdet <- function(v, delta, x = NULL){
  maxtab <- NULL
  mintab <- NULL

  if (is.null(x))
  {
    x <- seq_along(v)
  }

  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have the same length")
  }

  if (!is.numeric(delta))
  {
    stop("Input argument delta must be numeric")
  }

  if (delta <= 0)
  {
    stop("Input argument delta must be positive")
  }

  mn <- Inf
  mx <- -Inf

  mnpos <- NA
  mxpos <- NA

  lookformax <- TRUE

  for(i in seq_along(v))
  {
    this <- v[i]

    if (!is.na(this) & this > mx)
    {
      mx <- this
      mxpos <- x[i]
    }

    if (!is.na(this) & this < mn)
    {
      mn <- this
      mnpos <- x[i]
    }

    if (lookformax)
    {
      if (!is.na(this) & this < mx - delta)
      {
        maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))

        mn <- this
        mnpos <- x[i]

        lookformax <- FALSE
      }
    }
    else
    {
      if (!is.na(this) & this > mn + delta)
      {
        mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))

        mx <- this
        mxpos <- x[i]

        lookformax <- TRUE
      }
    }
  }

  list(maxtab = maxtab, mintab = mintab)
}