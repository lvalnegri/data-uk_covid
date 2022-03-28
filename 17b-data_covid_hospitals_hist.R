###################################################
# CoViD-19 UK HOSPITAL ACTIVITY * HISTORICAL DATA #
###################################################

dmpkg.funs::load_pkgs(dmp = FALSE, 'data.table')

down <- function(x, tm){
    if(tm %in% c('w', 'm', 's')) lkp <- fread(file.path(datauk_path, 'covid', 'hospitals', 'codes.csv'))[report == tm]
    tmpf = tempfile()
    download.file(x, tmpf)
    sn <- if(tm == 'd') c(12, 27, 42, 57, 73, 88, 103) else lkp[, sh_name]
    skp <- 12 + 2 * (tm == 'w')
    idv <- if(tm == 'w') 1:4 else 1:3
    y <- rbindlist(lapply(
            sn, 
            \(s){
                switch(tm,
                    'd' = { 
                            data.table(
                                which(s == sn) + 100,
                                melt(as.data.table(readxl::read_xlsx(tmpf, 2, skip = s, n_max = 9))[-1], id.vars = 1, variable.factor = FALSE, variable.name = 'datefield', na.rm = TRUE)
                            )
                        },
                    's' = { 
                            data.table(
                                lkp[sh_name == s, code],
                                melt(as.data.table(readxl::read_xlsx(tmpf, s, skip = 12))[-(1:2)], id.vars = 1, variable.factor = FALSE, variable.name = 'datefield', na.rm = TRUE)
                            )
                        },
                    { 
                        message('Processing Sheet: ', s)
                        tryCatch({
                                y <- setDT(readxl::read_xlsx(tmpf, sheet = s, skip = skp))
                                y <- y[!(is.na(Code) | Code == '-')]
                                data.table( lkp[sh_name == s, code], melt(y, id.vars = idv, variable.factor = FALSE, variable.name = 'datefield', na.rm = TRUE) )
                            },  
                            error = function(err) { 
                                message(' > > > > Sheet Not found!')
                                return(NULL) 
                            }
                        )
                    }               
                )
            }
    ))
    unlink(tmpf)
    y <- y[, datefield := as.Date(as.integer(datefield), origin = "1899-12-30")]
    y
}

y <- list()

# daily report -----
yd <- rbindlist(list(
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/COVID-19-daily-admissions-and-beds-20210406-DQnotes.xlsx', 'd'),
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/COVID-19-daily-admissions-and-beds-20211207-20210407-20210930-DQnotes.xlsx', 'd')
))
setnames(yd, c('code', 'RGN', 'datefield', 'value'))

# weekly -----
yw <- rbindlist(list(
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406-DQnotes.xlsx', 'w'),
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Weekly-covid-admissions-and-beds-publication-211209-210407-210930-DQnotes.xlsx', 'w')
))
yt <- unique(yw[, 2:5])
yw <- yw[value > 0][, c(2, 3, 5) := NULL]
setnames(yw, c('code', 'TST', 'datefield', 'value'))

# monthly -----
ym <- rbindlist(list(
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Covid-Publication-06-04-2021-up-to-200731-DQnotes.xlsx', 'm'),
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Covid-Publication-06-04-2021-200801-210406-DQnotes.xlsx', 'm'),
        down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/02/Covid-Publication-30-09-2021-DQnotes.xlsx', 'm')
))
ytm <- unique(ym[, 2:4])
if(nrow(ytm[!Code %chin% yt$Code]) > 0) yt <- rbindlist(list(yt, ytm[!Code %chin% yt$Code]), fill = TRUE)
ym <- ym[value > 0][, c(2, 4) := NULL]
setnames(ym, c('code', 'TST', 'datefield', 'value'))

# supplement -----
ys <- down('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Primary-Diagnosis-Supplement-20211209-20210618-20210930.xlsx', 's')
setnames(ys, c('code', 'RGN', 'datefield', 'value'))

# trusts
setnames(yt, c('is_acute', 'RGN', 'TST', 'name'))
setcolorder(yt, c('TST', 'name', 'RGN'))

# save all as a single list
saveRDS(
    list(
        'd' = yd[order(code, datefield, RGN)],
        'w' = yw[order(code, datefield, TST)],
        'm' = ym[order(code, datefield, TST)],
        's' = ys[order(code, datefield, RGN)],
        't' = yt[order(TST)]
    ),
    file.path(datauk_path, 'covid', 'hospitals', 'datasets')
)

# Clean and Exit
rm(list = ls())
gc()
