#################################
# CoViD-19 UK HOSPITAL ACTIVITY #
#################################

dmpkg.funs::load_pkgs('data.table')

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
                                melt(as.data.table(readxl::read_xlsx(tmpf, 'Daily publication', skip = s, n_max = 9))[-1], id.vars = 1, variable.factor = FALSE, variable.name = 'datefield', na.rm = TRUE)
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

dts <- readRDS(file.path(datauk_path, 'covid', 'hospitals', 'datasets'))

html <- paste(readLines('https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'), collapse = '\n')
matched <- stringr::str_match_all(html, "<a href=\"(.*?xlsx)\"")
links <- matched[[1]][, 2]

# daily
y <- down(links[grepl('daily', tolower(links))][3], 'd')
yo <- dts[['d']][datefield < min(y$datefield)]
dts[['d']] <- rbindlist(list(yo, y), use.names = FALSE)[value > 0][order(code, datefield, RGN)]

# weekly
y <- down(links[grepl('weekly', tolower(links))][3], 'w')
yo <- dts[['w']][datefield < min(y$datefield)]
yt <- unique(y[, 2:5])
y <- y[, c(2, 3, 5) := NULL]
dts[['w']] <- rbindlist(list(yo, y), use.names = FALSE)[value > 0][order(code, datefield, TST)]

# monthly
y <- down(links[grepl('publication', tolower(links))][4], 'm')
yo <- dts[['m']][datefield < min(y$datefield)]
ytm <- unique(y[, 2:4])[!Code %chin% yt$Code]
if(nrow(ytm) > 0) yt <- rbindlist(list( yt, ytm ), fill = TRUE)
y <- y[, c(2, 4) := NULL]
dts[['m']] <- rbindlist(list(yo, y), use.names = FALSE)[value > 0][order(code, datefield, TST)]

# check trusts
if(nrow(dts[['t']][!TST %in% yt$Code]) > 0){
    stop('You need to finish the code to append new trusts!')
    dts[['t']] <- dts[['t']]
}

# supplement
y <- down(links[grepl('primary', tolower(links))][2], 's')
yo <- dts[['s']][datefield < min(y$datefield)]
dts[['s']] <- rbindlist(list(yo, y), use.names = FALSE)[value > 0][order(code, datefield, RGN)]

# Save, Clean and Exit
saveRDS(dts, file.path(datauk_path, 'covid', 'hospitals', 'datasets'))
rm(list = ls())
gc()
