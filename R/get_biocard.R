# function to get information from biocard

#' Get BIOCARD Data
#'
#' @param path The path where the biocard data stored
#' @return a table with merged biocard data
#' @export
#'
#' @import gdata
#' @examples
#' \dontrun{
#' path = "/Users/name/Documents/R/adbiomarker/Data/BIOCARD"
#' dt_biocard <- get_biocard(path)
#' }
get_biocard <- function(path) {
  dat_cog <- read.xls(xls = paste(path, "BIOCARD_CognitiveData_2020.04.22.xls", sep = ""))
  dat_dx <- read.xls(xls = paste(path, "BIOCARD_Diagnosis_2020.05.03.xls", sep = ""))
  dat_csf <- read.xls(xls = paste(path, "FINAL_CSF_wdup 20140422.xls", sep = ""))
  dat_cog$VISITDATE <- as.Date(dat_cog$VISITDATE, "%Y-%m-%d")
  dat_dx$DIAGDATE <- as.Date(dat_dx$DIAGDATE, "%Y-%m-%d")
  dat_csf$Date <- as.Date(dat_csf$Date, "%m/%d/%y")
  names(dat_csf)[7:9] <- c("tau", "abeta", "ptau")

  # MRI hippocampus
  hippo_dat <- read.xls(xls = paste(path, "BIOCARD_Hippocampus_MRI_Measures_08022013.xlsx", sep = ""))[-c(1:2), ]
  hippo_dat$Scan.Date <- as.Date(hippo_dat$Scan.Date, "%d-%B-%Y")
  hippo_dat$Intracranial.Volume <- as.numeric(hippo_dat$Intracranial.Volume)
  hippo_dat$Left.Hippocampus <- as.numeric(hippo_dat$Left.Hippocampus)
  hippo_dat$Right.Hippocampus <- as.numeric(hippo_dat$Right.Hippocampus)
  hippo_dat$bihippo <- (hippo_dat$Left.Hippocampus + hippo_dat$Right.Hippocampus)/2

  # MRI amygdala
  amy_dat <- read.xls(xls = paste(path, "BIOCARD_Amygdala_MRI_Measures_08022013.xlsx", sep = ""))[-c(1:2), ]
  amy_dat$Scan.Date <- as.Date(amy_dat$Scan.Date, "%d-%B-%Y")
  amy_dat$Intracranial.Volume <- as.numeric(amy_dat$Intracranial.Volume)
  amy_dat$Left.Amygdala <- as.numeric(amy_dat$Left.Amygdala)
  amy_dat$Right.Amygdala <- as.numeric(amy_dat$Right.Amygdala)
  amy_dat$biamy <- (amy_dat$Left.Amygdala + amy_dat$Right.Amygdala) / 2

  # MRI EC volume
  ec_dat <- read.xls(xls = paste(path, "BIOCARD_Entorhinal_Cortex_MRI_Measures_08022013_including_thickness.xlsx", sep = ""))[-c(1:2), ]
  ec_dat$Scan.Date <- as.Date(ec_dat$Scan.Date, "%d-%B-%Y")
  ec_dat$Intracranial.Volume <- as.numeric(ec_dat$Intracranial.Volume)
  ec_dat$Left.Entorhinal.Cortex.Volume..cu..mm. <- as.numeric(ec_dat$Left.Entorhinal.Cortex.Volume..cu..mm.)
  ec_dat$Right.Entorhinal.Cortex.Volume..cu..mm. <- as.numeric(ec_dat$Right.Entorhinal.Cortex.Volume..cu..mm.)
  ec_dat$biecvol <- (ec_dat$Left.Entorhinal.Cortex.Volume..cu..mm. + ec_dat$Right.Entorhinal.Cortex.Volume..cu..mm.) / 2
  ec_dat$Left.Entorhinal.Cortex.Thickness...mm. <- as.numeric(ec_dat$Left.Entorhinal.Cortex.Thickness...mm.)
  ec_dat$Right.Entorhinal.Cortex.Thickness..mm. <- as.numeric(ec_dat$Right.Entorhinal.Cortex.Thickness..mm.)
  ec_dat$biecthick <- (ec_dat$Left.Entorhinal.Cortex.Thickness...mm. + ec_dat$Right.Entorhinal.Cortex.Thickness..mm.) / 2

  # map all marker data to diagnosis visits
  # for each diagnosis visit, assign the closest markers
  # within a 730-day-each-side two-sided window
  # if no marker satisfies assignment criteria, mark as missing
  # if two marker satisfy assignmetn criteria, take first

  mymatch <- function(xid, xdate, dat, yidname, ydatename) {
    idtemp <- dat[dat[[yidname]] == xid, ]
    datediff <- as.numeric(idtemp[[ydatename]] - xdate)
    if (length(datediff[!is.na(datediff)]) != 0) {
      if (any(abs(datediff) <= 730)) {
        temp <- idtemp[which(abs(datediff) == min(abs(datediff))), , drop = F][1, ]
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

  dat <- data.frame()
  for (i in 1:nrow(dat_dx)) {
    x <- dat_dx[i, ]
    tcog <- mymatch(xid = x$SUBJECT_ID, xdate = x$DIAGDATE,
                    dat = dat_cog, yidname = "SUBJECT_ID", ydatename = "VISITDATE")
    tcsf <- mymatch(xid = x$SUBJECT_ID, xdate = x$DIAGDATE,
                    dat = dat_csf, yidname = "ID", ydatename = "Date")
    thippo <- mymatch(xid = x$SUBJECT_ID, xdate = x$DIAGDATE,
                      dat = hippo_dat, yidname = "Study.ID", ydatename = "Scan.Date")
    tamy <- mymatch(xid = x$SUBJECT_ID, xdate = x$DIAGDATE,
                    dat = amy_dat, yidname = "Study.ID", ydatename = "Scan.Date")
    tec <- mymatch(xid = x$SUBJECT_ID, xdate = x$DIAGDATE,
                   dat = ec_dat, yidname = "Study.ID", ydatename = "Scan.Date")
    # merge data for output
    out <- merge(x, tcog, by = c("SUBJECT_ID"))
    out <- merge(out, tcsf, by.x = "SUBJECT_ID", by.y = "ID")
    out <- merge(out, thippo, by.x = "SUBJECT_ID", by.y = "Study.ID")
    out <- merge(out, tamy, by.x = "SUBJECT_ID", by.y = "Study.ID")
    out <- merge(out, tec, by.x = "SUBJECT_ID", by.y = "Study.ID")
    dat <- rbind(dat, out)
  }


  # exclude subjects from list A and list B
  listA <- read.xls(xls = paste(path, "LIST_A_Subjects Not Enrolled-Jan 2020.xlsx", sep = ""))
  listB <- read.xls(xls = paste(path, "LIST_B_IMPAIRED_AT_BASELINE.09.22.2015.xlsx", sep = ""))
  exid <- c(listA$STUDY_ID, listB$STUDY_ID)

  return(dat)

}
