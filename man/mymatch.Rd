% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymatch.R
\name{mymatch}
\alias{mymatch}
\title{Map Biomarkers Data to Diagnosis Visits}
\usage{
mymatch(xid, xdate, dat, yidname, ydatename)
}
\arguments{
\item{xid}{Id in diagnosis visit table}

\item{xdate}{Date of visit}

\item{dat}{Table includes biomarkers}

\item{yidname}{Id in the table includes biomarkers}

\item{ydatename}{Name of date in the table includes biomarkers}
}
\value{
The matched biomarker data to diagnosis visits
}
\description{
Map Biomarkers Data to Diagnosis Visits
}
\examples{
\dontrun{
tcog <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
xdate = x$date,
dat = dat_cog, yidname = COG$id, ydatename = "date")
}
}
