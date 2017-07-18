pkgs = list('readxl', 'magrittr', 'qdap', 'survival', 'fitdistrplus', 'data.table')
lapply(X = 1:length(pkgs),
       FUN = function(x) do.call('library', args = list(pkgs[x], quietly = T)))

file <- 'Shelby_K_Data.xlsx'

data2 <- data.table(read_excel(file, sheet = 2), key = 'Foreign Key')
data3 <- data.table(read_excel(file, sheet = 3), key = 'Foreign Key')

## Merge data from both sheets

Data <- data2[data3]

#save('Data', file = 'shelby.rda', compression_level = 9, compress = T)

colnames(Data) <- paste0('col', 1:ncol(Data))

colnames(Data)[c(1,26:32,65)] <- c('Foreign Key',
                                   'first symptom date',
                                   'first symptom diff',
                                   'first dr appt date',
                                   'diff first symptom diagnose',
                                   'date of diagnosis',
                                   'age of diagnosis',
                                   'type of diagnosis',
                                   'date of birth')

## Clean up type of diagnosis as 1's & 0's

diagnose <- c('Clinical diagnosis by physician based on symptoms',
              'Clinical diagnosis by physician based on family history',
              'Diagnosis by healthcare professional other than a physician')

DIAG <- sapply(X = 1:nrow(Data),
               FUN = function(x) {
                 `if`(Data$`type of diagnosis`[x]%in% diagnose,1,0)
               })

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
             dob  = as.Date(Data$`date of birth`, '%m/%d/%Y'),
             symp = as.Date(Data$`first symptom date`, '%m/%d/%Y'),
             doct = as.Date(Data$`first dr appt date`, '%m/%d/%Y'),
             diag = as.Date(Data$`date of diagnosis`, '%m/%d/%Y'),
             ages = ages,
             type = DIAG,
             stringsAsFactors = F)

# Check for errors in the dates
# If a patient's record includes multiple
# NA dates - remove the record from the data

multi.na <- by(date.frame[,3:5],
               date.frame[,1],
               FUN = function(x) sum(is.na(x)) > 1)

date.frame %<>% subset(!multi.na)

# Fill in data with reasonable values
#
# If symptom date is missing set
# symptom_date == doctor_date
# However this is a left-censored observation
# So we need to add a column for censoring

date.frame$censor = 'none'

cens <- which(is.na(date.frame$symp))

date.frame$symp[cens] = date.frame$doct[cens]
date.frame$censor[cens] = 'left'

cens2 <- sapply(X = 1:nrow(date.frame),
                   FUN = function(x) {
                     `if`(date.frame$type[x]==0,
                          'right',
                          date.frame$censor[x])
                      })
date.frame$censor <- cens2

# Check patient records for erroneous dates 
# If diagnosis is before symptom that is wrong

bad <- date.frame[,3] > date.frame[,5] 

date.frame %<>% subset(!bad)

diff <- sapply(X = 1:nrow(date.frame),
               FUN = function(x) {
                 `if`(date.frame$censor[x]=='left',
                      date.frame$diag[x] - date.frame$dob[x],
                      date.frame$diag[x] - date.frame$symp[x])
               })


## subset patients who have been diagnosed
celiac <- subset(date.frame, type==1)

diff <- celiac$diag-celiac$symp

## histogram of differences

hist(as.numeric(diff),
     breaks = 100,
     las = 1, 
     col = 'steelblue', 
     border = 'white')

## MAXIMUM LIKELIHOOD ESTIMATION

diff <- as.numeric(diff) + rnorm(n = length(diff), 0, sd = 0.00001)^2

## lognormal dist

lnorm <- fitdist(as.numeric(diff), distr = 'lnorm')

## normal dist
pbisa <- teachingApps::pbisa
dbisa <- teachingApps::dbisa
 bisa <- fitdist(as.numeric(diff), distr = 'bisa', start = list(shape = 1, scale = 10))

## Weibull dist

lweib <- fitdist(diff, distr = 'weibull')

## loglogistic
dllogis <- actuar::dllogis
pllogis <- actuar::pllogis

llog <- fitdist(diff, distr = 'llogis')

## CREATE TABLE OF RESULTS

llike <- c(lnorm$loglik,      lweib$loglik, llog$loglik)
aic   <- c(lnorm$aic,         lweib$aic,    llog$aic)
bic   <- c(lnorm$bic,         lweib$bic,    llog$bic)
parm1 <- c(lnorm$estimate[1], lweib$est[1], llog$est[1])
parm2 <- c(lnorm$estimate[2], lweib$est[2], llog$est[2])
sdp1  <- c(lnorm$sd[1],       lweib$sd[1],  llog$sd[1])
sdp2  <- c(lnorm$sd[2],       lweib$sd[2],  llog$sd[2])

result <- data.frame(llike, aic, bic, parm1, parm2, sdp1, sdp2)

hist(diff, 
     breaks = 50, 
     las = 1, 
     col = 'steelblue', 
     border = 'white',
     probability = T)
curve(dweibull(x, lweib$est[1], lweib$est[2]), 
      xlim = c(0.001,25000),
      col = 'red',
      lwd = 2, 
      add = T)
curve(dllogis(x, llog$est[1], 1/llog$est[2]), 
      xlim = c(0.001,25000),
      col = 'green',
      lwd = 2, 
      add = T)
curve(dlnorm(x, lnorm$est[1], lnorm$est[2]), 
      xlim = c(0.001,25000),
      col = 'orange',
      lwd = 2, 
      add = T)
legend('topright',
       legend = c('Weibull','Loglogistic','Lognormal'),
       col = c('red','green','orange'),
       lwd = 2,
       bty = 'n')


## Plot 2

plot(ecdf(diff), las = 1, xlim = c(0,25000))
curve(pweibull(x, lweib$est[1], lweib$est[2]), 
      xlim = c(0.001,25000),
      col = 'red',
      lwd = 2, 
      add = T)
curve(pllogis(x, llog$est[1], 1/llog$est[2]), 
      xlim = c(0.001,25000),
      col = 'green',
      lwd = 2, 
      add = T)
curve(plnorm(x, lnorm$est[1], lnorm$est[2]), 
      xlim = c(0.001,25000),
      col = 'orange',
      lwd = 2, 
      add = T)
legend('bottomright',
       legend = c('Weibull','Loglogistic','Lognormal'),
       col = c('red','green','orange'),
       lwd = 2,
       bty = 'n')


celiac$diff <- diff
model <- lm(diff^0.275 ~ ages, data = celiac)
summary(model)
plot(model)




symp_diff <- Date_diag - Date_symp
doct_diff <- Date_doct - Date_symp
diag_diff <- Date_diag - Date_doct


good_data <- shelb_na[Patients,]
good_data$diff <- Diff_pos 
as.POSIXct(date_dr, format = '%m-%d-%Y')

test_surv <- Surv(good_data$diff, rep(1, nrow(good_data)), type = 'right')

survreg(test_surv ~ as.factor(good_data$col3), data = good_data)
