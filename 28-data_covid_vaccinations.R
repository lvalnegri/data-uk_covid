##########################################
# CoViD-19 UK * DATA COVID - VACCINATION #
##########################################

# load packages
pkgs <- c('data.table', 'openxlsx')
lapply(pkgs, require, char = TRUE)

# retrieve file links
url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/'
html <- paste(readLines(url), collapse = '\n')
matched <- stringr::str_match_all(html, "<a href=\"(.*?xlsx)\"")
links <- matched[[1]][, 2]

# monthly report


# Weekly report
tmp = tempfile()
fn <- links[grepl('weekly', tolower(links))][1]
wkd <- as.Date(gsub( ".*-(\\d{1,2}-.*-\\d{4}).*", "\\1", fn), '%d-%B-%Y')
download.file(fn, destfile = tmp)
y <- getSheetNames(tmp)
y <- y[grepl('MSOA', y)]
y <- setDT(read.xlsx(tmp, sheet = y, startRow = 12))
y <- y[, -c(1:4, 6)]
setnames(y, c('MSOA', 'V_16_64', 'V_65_69', 'V_70_74', 'V_75_79', 'V_80_99'))
y <- y[!is.na(MSOA)]
cols <- names(y)[2:ncol(y)]
y[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
y[, 'V_16_99' := as.integer(rowSums(.SD)), .SDcols = cols]
yp <- fread(paste0('./uk_covid/data/pop_msoa.csv'))
y <- yp[y, on = 'MSOA']
y[, `:=`(
    S_16_64 = V_16_64 / P_16_64,
    S_65_69 = V_65_69 / P_65_69,
    S_70_74 = V_70_74 / P_70_74,
    S_75_79 = V_75_79 / P_75_79,
    S_80_99 = V_80_99 / P_80_99,
    S_16_99 = V_16_99 / P_16_99
)]
fwrite(y, paste0('./uk_covid/data/vaccine_msoa_', wkd, '.csv'))
unlink(tmp)

# Daily report





# Clean and Exit
rm(list = ls())
gc()
