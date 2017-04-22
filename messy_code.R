library(readxl)

file <- file.choose()

shelby_data2 <- read_excel(file, sheet = 2)
shelby_data3 <- read_excel(file, sheet = 3)

colnames(shelby_data2) <- paste0('col', 1:ncol(shelby_data))

colnames(shelby_data2)[26:31] <- c('first symptom date',
                                   'first symptom diff',
                                   'first dr appt date',
                                   'diff first symptom diagnose',
                                   'date of diagnosis',
                                   'age of diagnosis')

na_rows <- which(is.na(shelby_data2$`date of diagnosis`))
shelby_na <- shelby_data2[-na_rows,]

shelb_na$`diff first symptom diagnose` gsub

clean_text <- function(dataset = NULL, column = NULL) {
  
  Col <- dataset[,column]
  Col <- sapply(X = 1:nrow(Col),
                FUN = function(x) gsub('&lt; 1 months', '0-1', Col[x,]))
  Col <- as.data.frame(Col)
  
  Col <- sapply(X = 1:nrow(Col), 
                 FUN = function(x) gsub('&lt; 1 month', '0-1', Col[x,]))
  Col <- as.data.frame(Col)
  
  Col <- sapply(X = 1:nrow(Col),
               FUN = function(x) gsub('&gt; 25 years', '25-Inf', Col[x,]))
  Col <- as.data.frame(Col)
  # Col <- sapply(X = 1:length(Col),
  #                FUN = function(x) gsub(' months', '', Col[x]))
  
  years <- sapply(X = 1:nrow(Col),
                  FUN = function(x) grepl('years',Col[x,]))
  as.data.frame(Col)
  
  Col[which(years),] <- 
    sapply(X = which(years),
           FUN = function(x) {
            gsub(' years', '', Col[x,])
         })
  col
  gsub('', '3-6', Col)
  gsub(' mos', '', Col)
  
}

patient <- shelb_na$col1
date_dr <- shelb_na$`first dr appt date`
date_diag <- shelb_na$`date of diagnosis`

date_dr <- gsub('skipped', NA, date_dr)
date_diag <- gsub('skipped', NA, date_diag)

date.frame <- data.frame(patient, date_dr, date_diag)

na_rows <- which(sapply(X = 1:nrow(date.frame),
                  FUN = function(x) any(is.na(date.frame[x,]))))

date.frame <- date.frame[-na_rows,]

Date_dr   <- as.Date.character(date.frame[,2], format = '%m/%d/%Y')
Date_diag <- as.Date.character(date.frame[,3], format = '%m/%d/%Y')

Diff <- Date_diag - Date_dr

Diff_pos <- Diff[which(Diff > 0)] 
patients <- as.character(date.frame[which(Diff > 0),1,])
Patients <- sapply(X = 1:length(patients), 
                   FUN = function(x) which(patients[x]==shelb_na[,1]))

good_data <- shelb_na[Patients,]
good_data$diff <- Diff_pos 
as.POSIXct(date_dr, format = '%m-%d-%Y')

library(survival)

test_surv <- Surv(good_data$diff, rep(1, nrow(good_data)), type = 'right')

survreg(test_surv ~ as.factor(good_data$col3), data = good_data)
