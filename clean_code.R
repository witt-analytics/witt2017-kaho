needs::needs(readxl, magrittr, qdap, survival, fitdistrplus, data.table)

file <- 'Shelby_K_Data.xlsx'

data2 <- data.table(read_excel(file, sheet = 2), key = 'Foreign Key')
data3 <- data.table(read_excel(file, sheet = 3), key = 'Foreign Key')

## Merge data from both sheets

Data <- data2[data3]

colnames(Data) <- paste0('col', 1:ncol(Data))

colnames(Data)[c(1,26:31)] <- c('Foreign Key',
                                 'first symptom date',
                                 'first symptom diff',
                                 'first dr appt date',
                                 'diff first symptom diagnose',
                                 'date of diagnosis',
                                 'age of diagnosis')

# We need the ages, but must first convert to 
# Numeric values
old.ages <- c(' years old', 
              'Less than 4 years old', 
              'Greater than 65 years',
              'skipped',
              "Don't know")

new.ages <- c('', '4', '65', 'NA', 'NA')

suppressWarnings({
ages <- 
  qdap::mgsub(old.ages, new.ages, Data$`age of diagnosis`) %>%
  as.numeric
})

#data2 %<>% subset(!is.na(`date of diagnosis`))

date.frame <- 
  data.frame(p.id = Data$`Foreign Key`, 
             symp = as.Date(Data$`first symptom date`, '%m/%d/%Y'),
             doct = as.Date(Data$`first dr appt date`, '%m/%d/%Y'),
             diag = as.Date(Data$`date of diagnosis`, '%m/%d/%Y'),
             ages = ages,
             stringsAsFactors = F)

# Check for errors in the dates
# If a patient's record includes multiple
# NA dates - remove the record from the data

multi.na <- by(date.frame[,2:4],
               date.frame[,1],
               FUN = function(x) sum(is.na(x)) > 1)

date.frame %<>% subset(!multi.na)

# Fill in data with reasonable values
#
# If symptom date is missing set
# symptom_date == doctor_date
# However this is a left-truncated observation
# So we need to add a column for truncation

date.frame$truncation = 'none'

trunc <- which(is.na(date.frame$symp))

date.frame$symp[trunc] = date.frame$doct[trunc]
date.frame$truncation[trunc] = 'left'

# Check patient records for erroneous dates 
# If diagnosis is before symptom that is wrong

date.frame[,2] > date.frame[,4] & date.frame[,3] < date.frame[,4]

symp_diff <- Date_diag - Date_symp
doct_diff <- Date_doct - Date_symp
diag_diff <- Date_diag - Date_doct







good_data <- shelb_na[Patients,]
good_data$diff <- Diff_pos 
as.POSIXct(date_dr, format = '%m-%d-%Y')

test_surv <- Surv(good_data$diff, rep(1, nrow(good_data)), type = 'right')

survreg(test_surv ~ as.factor(good_data$col3), data = good_data)
