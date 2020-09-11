# clear working space
rm(list = ls())
# read updated data following Alden's do file
dat_vitals <- read.csv(file = "data/VITALS.csv", na.strings = -4)  # for ADNI1,2,3,GO
dat_vitals$EXAMDATE <- as.Date(dat_vitals$EXAMDATE, "%Y-%m-%d")
dat_vitals$USERDATE <- as.Date(dat_vitals$USERDATE, "%Y-%m-%d")
dat_vitals$USERDATE2 <- as.Date(dat_vitals$USERDATE2, "%Y-%m-%d")
dat_fdg <- read.csv(file = "data/UCBERKELEYFDG_07_30_15.csv", na.strings = -4)  # for ADNI1,2,3,GO
dat_fdg$EXAMDATE <- as.Date(dat_fdg$EXAMDATE, "%Y-%m-%d")
dat_av45 <- read.csv(file = "data/UCBERKELEYAV45_11_14_17.csv", na.strings = -4)  # for ADNI1,2,3,GO; MRI AV45 scan
dat_av45$EXAMDATE <- as.Date(dat_av45$EXAMDATE, "%Y-%m-%d")
dat_wmh1 <- read.csv(file = "data/UCD_ADNI1_WMH.csv", na.strings = -4)  # ADNI1
dat_wmh1$Phase <- "ADNI1"
dat_wmh1$EXAMDATE <- as.Date(dat_wmh1$EXAMDATE, "%Y-%m-%d")
dat_wmh2 <- read.csv(file = "data/UCD_ADNI2_WMH_10_26_15.csv", na.strings = -4)  # ADNI2
dat_wmh2$Phase <- "ADNI2"
dat_wmh2$EXAMDATE <- as.Date(dat_wmh2$EXAMDATE, "%Y-%m-%d")
dat_rar <- read.csv(file = "data/UCSFATRPHY.csv", na.strings = -4)  # ADNI1, MRI
dat_rar$Phase <- "ADNI1"
dat_rar$EXAMDATE1 <- as.Date(dat_rar$EXAMDATE1, "%Y-%m-%d")
dat_rar$EXAMDATE2 <- as.Date(dat_rar$EXAMDATE2, "%Y-%m-%d")
dat_fs1 <- read.csv(file = "data/UCSFFSX51_05_04_16.csv", na.strings = -4)  # mostly ADNI2,GO
dat_fs1$EXAMDATE <- as.Date(dat_fs1$EXAMDATE, "%Y-%m-%d")
dat_fs2 <- read.csv(file = "data/UCSFFSX_11_02_15.csv", na.strings = -4) # ADNI1
dat_fs2$EXAMDATE <- as.Date(dat_fs2$EXAMDATE, "%Y-%m-%d")
dat_fsl <- read.csv(file = "data/UCSFFSL51ALL_05_04_16.csv", na.strings = -4) # mostly ADNI2, some ADNIGO
dat_fsl$EXAMDATE <- as.Date(dat_fsl$EXAMDATE, "%Y-%m-%d")
dat_fs1y <- read.csv(file = "data/UCSFFSL51Y1_08_01_16.csv", na.strings = -4) # mostly ADNI2, some ADNIGO
dat_fs1y$EXAMDATE <- as.Date(dat_fs1y$EXAMDATE, "%Y-%m-%d")

# no fs data for ADNI3
dat_hippo <- read.csv(file = "data/UCSFSNTVOL.csv", na.strings = -4) # ADNI1; found for ADNI1 only
dat_hippo$Phase <- "ADNI1"
dat_hippo$EXAMDATE <- as.Date(dat_hippo$EXAMDATE, "%Y-%m-%d")
dat_csfm <- read.csv(file = "data/UPENNBIOMK_MASTER.csv", na.strings = -4) # ADNI1,2,GO
dat_csfm$DRWDTE <- as.Date(dat_csfm$DRWDTE, "%Y-%m-%d")
# many subjects have CSF draws but sample has not been processed to give abeta, tau, ptau values
dat_demo <- read.csv(file = "data/PTDEMOG.csv", na.strings = -4)
dat_demo$DOB <- do.call("c", lapply(1:nrow(dat_demo), function(i) {
      out <- as.Date(paste0(dat_demo$PTDOBYY[i], "-", dat_demo$PTDOBMM[i], "-01"), 
                     "%Y-%m-%d")
      return(out)
}))
dat_demo$update_stamp <- as.Date(dat_demo$update_stamp, "%Y-%m-%d")
dat_demo <- do.call(rbind, lapply(split(dat_demo, dat_demo$RID), function(x) {
      return(x[x$update_stamp == max(x$update_stamp), ])
}))
dat_cog <- read.csv(file = "data/NEUROBAT.csv", na.strings = -4)  # ADNI1,2,GO
dat_cog$EXAMDATE <- as.Date(dat_cog$EXAMDATE, "%Y-%m-%d")
dat_cog$USERDATE <- as.Date(dat_cog$USERDATE, "%Y-%m-%d")
dat_cog$USERDATE2 <- as.Date(dat_cog$USERDATE2, "%Y-%m-%d")
# LIMMTOTAL: Logical Memory - Immediate Recall Total Number of Story Units Recalled
dat_mmse <- read.csv(file = "data/MMSE.csv", na.strings = -4) # mmse score
dat_mmse$EXAMDATE <- as.Date(dat_mmse$EXAMDATE, "%Y-%m-%d")
dat_mmse$USERDATE <- as.Date(dat_mmse$USERDATE, "%Y-%m-%d")
dat_mmse$USERDATE2 <- as.Date(dat_mmse$USERDATE2, "%Y-%m-%d")
dat_faq <- read.csv(file = "data/FAQ.csv", na.strings = -4) # faq score
dat_faq$EXAMDATE <- as.Date(dat_faq$EXAMDATE, "%Y-%m-%d")
dat_faq$USERDATE <- as.Date(dat_faq$USERDATE, "%Y-%m-%d")
dat_faq$USERDATE2 <- as.Date(dat_faq$USERDATE2, "%Y-%m-%d")
dat_cdr <- read.csv(file = "data/CDR.csv", na.strings = -4) # cdr score; CDGLOBAL
dat_cdr$EXAMDATE <- as.Date(dat_cdr$EXAMDATE, "%Y-%m-%d")
dat_cdr$USERDATE <- as.Date(dat_cdr$USERDATE, "%Y-%m-%d")
dat_cdr$USERDATE2 <- as.Date(dat_cdr$USERDATE2, "%Y-%m-%d")
dat_dx <- read.csv(file = "data/DXSUM_PDXCONV_ADNIALL.csv", na.strings = -4)
dat_dx$EXAMDATE <- as.Date(dat_dx$EXAMDATE, "%Y-%m-%d")
dat_arm <- read.csv(file = "data/ARM.csv", na.strings = -4) # additional diagnosis info
dat_arm$dxarm <- NA
dat_arm$dxarm[dat_arm$ARM %in% c(1, 4, 7)] <- 1
dat_arm$dxarm[dat_arm$ARM %in% c(2, 5, 8) | dat_arm$RID == 739] <- 2
dat_arm$dxarm[dat_arm$ARM %in% c(3, 6, 9) | 
                 dat_arm$RID %in% c(78, 190, 332, 995, 1154, 1226)] <- 3
dat_arm$USERDATE <- as.Date(dat_arm$USERDATE, "%Y-%m-%d")
dat_dx$dx <- dat_dx$DXCURREN
dat_dx$dx[is.na(dat_dx$dx) & dat_dx$DXCHANGE %in% c(1, 7, 9)] <- 1
dat_dx$dx[is.na(dat_dx$dx) & dat_dx$DXCHANGE %in% c(2, 4, 8)] <- 2
dat_dx$dx[is.na(dat_dx$dx) & dat_dx$DXCHANGE %in% c(3, 5, 6)] <- 3



# boston naming test correct percent
dat_item <- read.csv(file = "data/ITEM.csv", na.strings = 999)
BNT_names <- grep("BosNam_Q.", names(dat_item))[-1]
BNT_correct <- apply(dat_item[, BNT_names], 2, function(x) {
      return(as.numeric(x %in% c(1))) # no cue correct 
})
dat_item$BNTPCT <- rowMeans(BNT_correct) * 100
dat_item$BosNam_ExamDate <- as.Date(dat_item$BosNam_ExamDate, "%m/%d/%Y")

# depression variable
dat_bl <- read.csv("data/BLCHANGE.csv")
dat_bl$EXAMDATE <- as.Date(dat_bl$EXAMDATE, "%Y-%m-%d")
dat_bl$USERDATE <- as.Date(dat_bl$USERDATE, "%Y-%m-%d")
dat_bl$USERDATE2 <- as.Date(dat_bl$USERDATE2, "%Y-%m-%d")

# add ADAS-COG
dat_adas_adni1 <- read.csv(file = "ADAS_ADNI1.csv")
dat_adas_adni1$USERDATE <- as.Date(dat_adas_adni1$USERDATE, "%Y-%m-%d")
dat_adas_adni1$EXAMDATE <- as.Date(dat_adas_adni1$EXAMDATE, "%Y-%m-%d")
# use EXAMDATE

dat_adas_adni23go <- read.csv(file = "ADAS_ADNIGO23.csv")
dat_adas_adni23go$USERDATE <- as.Date(dat_adas_adni23go$USERDATE, "%Y-%m-%d")
dat_adas_adni23go$USERDATE2 <- as.Date(dat_adas_adni23go$USERDATE2, "%Y-%m-%d")
# use USERDATE

dat_adas_score <- read.csv(file = "ADASSCORES.csv")
dat_adas_score$EXAMDATE <- as.Date(dat_adas_score$EXAMDATE, "%Y-%m-%d")
dat_adas_score$USERDATE <- as.Date(dat_adas_score$USERDATE, "%Y-%m-%d")
dat_adas_score$USERDATE2 <- as.Date(dat_adas_score$USERDATE2, "%Y-%m-%d")
# use EXAMDATE

# map all marker data to cognitive visits
# for each cognitive visit, assign the closest markers/cognitive visit
# within a 30-day-each-side two-sided window
# if no marker satisfies assignment criteria, mark as missing
# if two marker satisfy assignmetn criteria, take first
mymatch <- function(xid, xdate, dat, yidname, ydatename) {
      idtemp <- dat[dat[[yidname]] == xid, ]
      datediff <- as.numeric(idtemp[[ydatename]] - xdate)
      if (length(datediff[!is.na(datediff)]) != 0) {
            if (any(abs(datediff) <= 730, na.rm = T)) {
                  temp <- idtemp[which(abs(datediff) == min(abs(datediff), na.rm = T)), , drop = F][1, ]
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
for (i in 1:nrow(dat_cog)) {
      # print(i)
      x <- dat_cog[i, ]
      tvitals <- mymatch(xid = x$RID, xdate = x$USERDATE,
                        dat = dat_vitals, yidname = "RID", ydatename = "USERDATE")
      tfdg <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_fdg, yidname = "RID", ydatename = "EXAMDATE")
      tav45 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_av45, yidname = "RID", ydatename = "EXAMDATE")
      twmh1 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_wmh1, yidname = "RID", ydatename = "EXAMDATE")
      twmh2 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_wmh2, yidname = "RID", ydatename = "EXAMDATE")
      trar <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_rar, yidname = "RID", ydatename = "EXAMDATE1")
      tfs1 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_fs1, yidname = "RID", ydatename = "EXAMDATE")
      tfs2 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_fs2, yidname = "RID", ydatename = "EXAMDATE")
      tfsl <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_fsl, yidname = "RID", ydatename = "EXAMDATE")
      tfs1y <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_fs1y, yidname = "RID", ydatename = "EXAMDATE")
      thippo <- mymatch(xid = x$RID, xdate = x$USERDATE,
                        dat = dat_hippo, yidname = "RID", ydatename = "EXAMDATE")
      tcsfm <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_csfm, yidname = "RID", ydatename = "DRWDTE")
      tdx <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_dx, yidname = "RID", ydatename = "EXAMDATE")
      tmmse <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_mmse, yidname = "RID", ydatename = "USERDATE")
      tfaq <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_faq, yidname = "RID", ydatename = "USERDATE")
      tcdr <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_cdr, yidname = "RID", ydatename = "USERDATE")
      tbos <- mymatch(xid = x$RID, xdate = x$USERDATE,
                      dat = dat_item, yidname = "RID", ydatename = "BosNam_ExamDate")
      tarm <- mymatch(xid = x$RID, xdate = x$USERDATE,
                       dat = dat_arm, yidname = "RID", ydatename = "USERDATE")
      tbl <- mymatch(xid = x$RID, xdate = x$USERDATE,
                     dat = dat_bl, yidname = "RID", ydatename = "USERDATE")
      
      tadas1 <- mymatch(xid = x$RID, xdate = x$USERDATE,
                        dat = dat_adas_adni1, yidname = "RID", ydatename = "EXAMDATE")
      tadas23go <- mymatch(xid = x$RID, xdate = x$USERDATE,
                           dat = dat_adas_adni23go, yidname = "RID", ydatename = "USERDATE")
      tadasscore <- mymatch(xid = x$RID, xdate = x$USERDATE,
                           dat = dat_adas_score, yidname = "RID", ydatename = "EXAMDATE")
      
      tall <- list(tvitals, tfdg, tav45, twmh1, twmh2, trar, tfs1, tfs2,
                   tfsl, tfs1y, thippo, tcsfm, tdx, tmmse, tfaq, tcdr, tbos, 
                   tarm, tbl, tadas1, tadas23go, tadasscore)
      out <- x
      for (j in 1:length(tall)) {
            out <- merge(out, as.data.frame(tall[[j]]), by = "RID", all = T)
      }
      
      dat <- rbind(dat, out)
}

dat_apoe <- read.csv(file = "data/APOERES.csv", na.strings = -4)
dat <- merge(dat, dat_demo, by = "RID", all.x = T)
dat <- merge(dat, dat_apoe, by = "RID", all.x = T)

dat$DXCURREN <- ifelse(!is.na(dat$DXCURREN), dat$DXCURREN, ifelse(!is.na(dat$dxarm), 
                                                                  dat$dxarm, NA))

# sort data by EXAMEDATE.x within RID
dat <- do.call(rbind, lapply(split(dat, dat$RID), function(x) {
      return(x[order(x$USERDATE.x), ])
}))

# remove subjects with seeminly administrative error
dat <- dat[!(dat$PTDOBYY %in% c(2017, 2018)), ]

# rename variables
# demographic
dat$age <- as.numeric(dat$USERDATE.x - dat$DOB) / 365.25
dat$education <- dat$PTEDUCAT
# cognitive
dat$logmem <- dat$LDELTOTAL # logical memory delayed
dat$AVLTTOTL <- dat$AVTOT1 + dat$AVTOT2 + dat$AVTOT3 + dat$AVTOT4 + dat$AVTOT5
dat$DSST <- dat$DIGITSCOR # digital symbol
# MRI
dat$ICV <- rowMeans(dat[, c("ST10CV.x", "ST10CV.y")], na.rm = T)
dat$left.hippo.vol <- rowMeans(dat[, c("ST29SV.x", "ST29SV.y")], na.rm = T)
dat$right.hippo.vol <- rowMeans(dat[, c("ST88SV.x", "ST88SV.y")], na.rm = T)
dat$bihippo <- rowMeans(dat[, c("left.hippo.vol", "right.hippo.vol")])
dat$left.ec.vol <- rowMeans(dat[, c("ST24CV.x", "ST24CV.y")], na.rm = T)
dat$right.ec.vol <- rowMeans(dat[, c("ST83CV.x", "ST83CV.y")], na.rm = T)
dat$biec.vol <- rowMeans(dat[, c("left.ec.vol", "right.ec.vol")])
# adjust volume for ICV
for (volname in c("left.hippo.vol", "right.hippo.vol", "bihippo",
                  "left.ec.vol", "right.ec.vol", "biec.vol")) {
      dat[[paste0(volname, ".adj1")]] <- dat[[volname]] / dat$ICV
      dat[[paste0(volname, ".adj2")]][!is.na(dat[[volname]]) & !is.na(dat$ICV)] <-
            (lm(dat[[volname]] ~ dat$ICV)$residuals)
}
dat$left.ec.thk <- rowMeans(dat[, c("ST24TA.x", "ST24TA.y", "ST24TA.x.1", "ST24TA.y.1")], na.rm = T)
dat$right.ec.thk <- rowMeans(dat[, c("ST83TA.x", "ST83TA.y", "ST83TA.x.1", "ST83TA.y.1")], na.rm = T)
dat$biec.thik <- rowMeans(dat[, c("left.ec.thk", "right.ec.thk")])



# create apoe-4 carrier variables
dat$apoe <- apply(dat[, c("APGEN1", "APGEN2")], 1, function(x) {
      x <- as.numeric(x)
      temp <- x[1] * 10 + x[2]
      if (is.na(temp)) {
            out <- NA
      } else {
            if (temp == 24) {
                  out <- NA
            } else {
                  out <- as.numeric(temp %in% c(34, 44))
            }
            return(out)
      }
})
dat$apoe_dich <- apply(dat[, c("APGEN1", "APGEN2")], 1, function(x) {
      x <- as.numeric(x)
      temp <- x[1] * 10 + x[2]
      if (is.na(temp)) {
            out <- NA
      } else {
            out <- as.numeric(temp %in% c(34, 44))
      }
      return(out)
})
dat$apoe_cat_no24 <- apply(dat[, c("APGEN1", "APGEN2")], 1, function(x) {
      x <- as.numeric(x)
      temp <- x[1] * 10 + x[2]
      if (is.na(temp)) {
            out <- NA
      } else {
            if (temp == 24) {
                  out <- NA
            } else if (temp %in% c(34, 42, 43)) {
                  out <- 1
            } else if (temp == 44) {
                  out <- 2
            } else {
                  out <- 0
            }
      }
      return(out)
})
dat$apoe_cat<- apply(dat[, c("APGEN1", "APGEN2")], 1, function(x) {
      x <- as.numeric(x)
      temp <- x[1] * 10 + x[2]
      if (is.na(temp)) {
            out <- NA
      } else {
            if (temp %in% c(24, 34, 42, 43)) {
                  out <- 1
            } else if (temp == 44) {
                  out <- 2
            } else {
                  out <- 0
            }
      }
      return(out)
})

# create ADAS11 variable
dat$ADAS11 <- rowMeans(dat[, c("TOTSCORE", "TOTAL11")], na.rm = T)

dat_ADNI <- dat


# calibrate AVLTTOTL to BIOCARD CVLTTOTL
# use as equating samples: baseline ADNI and latest BIOCARD
# adjust for diagnosis type through weighting, calibrate to BIOCARD
# linear equating
baseline_AVLTTOTL <- do.call(rbind, lapply(split(dat_ADNI, dat_ADNI$RID), function(x) {
      if (any(!is.na(x$AVLTTOTL) & !is.na(x$DXCURREN))) {
            x <- x[!is.na(x$AVLTTOTL) & !is.na(x$DXCURREN), , drop = F]
            x <- x[order(x$USERDATE.x), , drop = F]
            out <- x[1, c("AVLTTOTL", "DXCURREN")]
      } else {
            out <- NULL
      }
      return(out)
}))



load("BIOCARD_mergedtodx_all.rda")
recent_CVLTTOTL <- do.call(rbind, lapply(split(dat, dat$Study_ID), function(x) {
      if (any(!is.na(x$C1A117) & !is.na(x$DIAG))) {
            x <- x[!is.na(x$C1A117) & !is.na(x$DIAG), , drop = F]
            x <- x[order(x$DIAGDATE), , drop = F]
            out <- x[nrow(x), c("C1A117", "DIAG")]
      } else {
            out <- NULL
      }
      return(out)
}))
mean_AVLTTOTL <- mean(baseline_AVLTTOTL$AVLTTOTL)
sd_AVLTTOTL <- sd(baseline_AVLTTOTL$AVLTTOTL)
mean_CVLTTOTL_normal <- mean(recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG %in% 
                              c("NORMAL", "IMPAIRED NOT MCI")])
mean_CVLTTOTL_mci <- mean(recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG == "MCI"])
mean_CVLTTOTL_ad <- mean(recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG == "DEMENTIA"])
mean_CVLTTOTL_adjusted <- 
      mean_CVLTTOTL_normal * mean(baseline_AVLTTOTL$DXCURREN == 1) + 
      mean_CVLTTOTL_mci * mean(baseline_AVLTTOTL$DXCURREN == 2) +
      mean_CVLTTOTL_ad * mean(baseline_AVLTTOTL$DXCURREN == 3)
m2_CVLTTOTL_normal <- mean((recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG %in% 
                                                        c("NORMAL", "IMPAIRED NOT MCI")])^2)
m2_CVLTTOTL_mci <- mean_CVLTTOTL_mci <- mean((recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG == "MCI"])^2)
m2_CVLTTOTL_ad <- mean((recent_CVLTTOTL$C1A117[recent_CVLTTOTL$DIAG == "DEMENTIA"])^2)
m2_CVLTTOTL_adjusted <-  m2_CVLTTOTL_normal * mean(baseline_AVLTTOTL$DXCURREN == 1) + 
      m2_CVLTTOTL_mci * mean(baseline_AVLTTOTL$DXCURREN == 2) +
      m2_CVLTTOTL_ad * mean(baseline_AVLTTOTL$DXCURREN == 3)
sd_CVLTTOTL_adjusted <- sqrt(m2_CVLTTOTL_adjusted - mean_CVLTTOTL_adjusted^2)
dat_ADNI$AVLTTOTL_adjusted <- (sd_CVLTTOTL_adjusted / sd_AVLTTOTL) * 
      (dat_ADNI$AVLTTOTL - mean_AVLTTOTL) + mean_CVLTTOTL_adjusted


# add survival info
dat_surv <- do.call(rbind, lapply(split(dat_ADNI, dat_ADNI$RID), function(x) {
      x <- x[complete.cases(x[, c("DXCURREN", "age")]), , drop = F]
      x <- x[order(x$USERDATE.x), , drop = F]
      baseline.age <- x$age[1]
      if (rev(x$DXCURREN)[1] %in% c(2, 3)) {
            d <- 1
            onset.pos <- which(x$DXCURREN %in% c(2, 3))[1]
            onset.age <- x$age[onset.pos]
      } else {
            d <- 0
            onset.age <- rev(x$age)[1]
      }
      time <- onset.age - baseline.age
      out <- data.frame(RID = x$RID[1], baseline.age = baseline.age,
                        onset.age = onset.age, time = time, d = d)
      return(out)
}))



dat_ADNI <- merge(dat_ADNI, dat_surv, by = "RID", all.x = T)


save(list = c("dat_ADNI"), file = "ADNImerge04232019.rda")

sum(complete.cases(dat_ADNI[, c("age", "education", "apoe",
                                "logmem", "AVLTTOTL",
                                "DSST", "BNTPCT",
                                "left.hippo.vol", "right.hippo.vol",
                                "bihippo", "bihippo.adj1", "bihippo.adj2",
                                "left.ec.vol", "right.ec.vol", "biec.vol",
                                "left.ec.thk", "right.ec.thk", "biec.thik",
                                "ABETA", "TAU", "PTAU", "DXCURREN")]))

length(unique(dat_ADNI$RID[complete.cases(dat_ADNI[, c("age", "education", "apoe",
                                         "logmem", "AVLTTOTL","BNTPCT", "DSST", 
                                         "left.hippo.vol", "right.hippo.vol", "bihippo",
                                         "left.ec.vol", "right.ec.vol", "biec.vol",
                                         "left.ec.thk", "right.ec.thk", "biec.thik",
                                         "ABETA", "TAU", "PTAU", "DXCURREN")])]))

write.csv(dat_ADNI, file = "dat_ADNI_04232019.csv")



sum(complete.cases(dat_ADNI[, c("ABETA", "TAU", "PTAU")]))


length(unique(dat_ADNI$RID[(complete.cases(dat_ADNI[, c("ABETA", "TAU", "PTAU")]))]))
