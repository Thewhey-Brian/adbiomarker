# the function to match data

# map all marker data to diagnosis visits
# for each diagnosis visit, assign the closest markers
# within a 730-day-each-side two-sided window
# if no marker satisfies assignment criteria, mark as missing
# if two marker satisfy assignmetn criteria, take first

#' Map Biomarkers Data to Diagnosis Visits
#'
#' @param xid Id in diagnosis visit table
#' @param xdate Date of visit
#' @param dat Table includes biomarkers
#' @param yidname Id in the table includes biomarkers
#' @param ydatename Name of date in the table includes biomarkers
#'
#' @return The matched biomarker data to diagnosis visits
#' @export
#'
#' @examples
#' \dontrun{
#' tcog <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
#' xdate = x$date,
#' dat = dat_cog, yidname = COG$id, ydatename = "date")
#' }
mymatch <- function(xid, xdate, dat, yidname, ydatename) {
  idtemp <- dat[dat[[yidname]] == xid, ]
  datediff <- as.numeric(idtemp[[ydatename]] - xdate)
  if (length(datediff[!is.na(datediff)]) != 0) {
    if (any(abs(datediff) <= 730)) {
      temp <- idtemp[which(abs(datediff) == min(abs(datediff))),
                     , drop = F][1, ]
    } else {
      temp <- dat[1, ]
      temp[yidname] <- xid
      temp[setdiff(names(temp), yidname)] <- NA
    }
  } else {
    temp <- dat[1, ]
    temp[yidname] <- xid
    temp[setdiff(names(temp), yidname)] <- NA
  }
  return(temp)
}
