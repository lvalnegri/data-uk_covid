############################################################
# CoViD-19 UK * ONS WEEKLY DEATHS (LAD) COVID + ALL CAUSES #
############################################################

message('Validating update...')
ons_url <- 'https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard'
x1 <- as.Date( 
    sub('[^0-9]+', '', rvest::html_text( rvest::html_node( xml2::read_html(ons_url), '.meta__item:nth-child(2)') ) ),
    '%d %B %Y'
)
x2 <- readLines(file.path(dmpkg.funs::datauk_path, 'covid', 'deaths', 'weekly_ons.date'))
if(is.na(x1)) q(save = 'no')
if(x1==x2) q(save = 'no')

dmpkg.funs::load_pkgs(c('data.table'))

lnks <- paste0('https://www.ons.gov.uk/', rvest::html_attr( rvest::html_nodes( xml2::read_html(ons_url), '.btn--thick'), 'href') )

get_data <- function(x){
  
    message('\nProcessing year: ', 2022 - x, '...')
  
    message(' - Downloading file...')
    y <- tempfile()
    download.file(lnks[x], y, quiet = TRUE)
    
    message(' - Reading datasets..')
    y <- rbindlist(list( 
            data.table( 2022 - x, 'REG', openxlsx::read.xlsx(y, 'Registrations - All data', startRow = 5, colNames = FALSE)),
            data.table( 2022 - x, 'OCC', openxlsx::read.xlsx(y, 'Occurrences - All data', startRow = 5, colNames = FALSE))
    ))
    
    message(' - Data Engineering...')
    y <- y[!grepl('health', tolower(X2))][, c('X2', 'X3') := NULL]
    setnames(y, c('year', 'date_type', 'LAD', 'cause', 'week', 'place', 'n_deaths'))
    setcolorder(y, c('year', 'week'))
    y[, cause := ifelse(grepl('COVID', cause), 'COVID', 'ALL')]
    
}

y <- rbindlist(list( get_data(1), get_data(2) ))

message('\nAdding weekdate')
yd <- rbindlist(list(
    data.table('year' = 2020, week = 1:53, 'weekdate' = as.Date('2020-01-03') + 7 * 0:52),
    data.table('year' = 2021, week = 1:52, 'weekdate' = as.Date('2021-01-08') + 7 * 0:51)
))
y <- yd[y, on = c('year', 'week')][order(weekdate)]

ye <- y[place %in% c('Elsewhere', 'Other communal establishment')][
        , .(place = 'Elsewhere', n_deaths = sum(n_deaths)), setdiff(names(y), c('place', 'n_deaths'))]
y <- rbindlist(list( y[!place %in% c('Elsewhere', 'Other communal establishment')], ye ))

y[place == 'Care home', place := 'Care Home']
y[, place := factor(place, levels = c('Hospital', 'Care Home', 'Home', 'Hospice', 'Elsewhere'))]

message('Writing dataset to files...')
fst::write_fst(y, file.path(datauk_path,'covid', 'deaths', 'weekly_ons'))
writeLines(as.character(x1), file.path(datauk_path, 'covid', 'deaths', 'weekly_ons.date'))

message('Done!')
rm(list = ls())
gc()
