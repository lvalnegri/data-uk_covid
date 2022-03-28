#############################################
# CoViD-19 UK * ONS WEEKLY Infection Survey #
#############################################

message('Validating update...')
ons_url <- 'https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata'
x1 <- as.Date( 
    sub('[^0-9]+', '', rvest::html_text( rvest::html_node( xml2::read_html(ons_url), '.meta__item:nth-child(2)') ) ),
    '%d %B %Y'
)
x2 <- readLines(file.path(dmpkg.funs::datauk_path, 'covid', 'cases', 'weekly_ons_survey.date'))
if(is.na(x1)) q(save = 'no')
if(x1==x2) q(save = 'no')

dmpkg.funs::load_pkgs(c('data.table'))

lnks <- paste0('https://www.ons.gov.uk/', rvest::html_attr( rvest::html_nodes( xml2::read_html(ons_url), '.btn--thick'), 'href') )

message('Downloading file...')
y <- tempfile()
download.file(lnks[1], y, quiet = TRUE)

message('Data Engineering...')
y <- rbindlist(list(
    openxlsx::read.xlsx(y, '1l', startRow = 7, colNames = FALSE, cols = c(1:4, 7)),
    openxlsx::read.xlsx(y, '3f', startRow = 7, colNames = FALSE, cols = c(1:4, 7)),
    openxlsx::read.xlsx(y, '4f', startRow = 7, colNames = FALSE, cols = c(1:4, 7)),
    openxlsx::read.xlsx(y, '5f', startRow = 7, colNames = FALSE, cols = c(1:4, 7))
))
setnames(y, c('CIS', 'RGN', 'CISn', 'pct_pos', 'ratio_pos'))
setcolorder(y, c('CIS', 'CISn'))
y <- y[!is.na(pct_pos)]
y[, ratio_pos := as.integer(sub('1 in ', '', ratio_pos))]

message('Writing dataset to files...')
fst::write_fst(y[order(CIS)], file.path(datauk_path,'covid', 'cases', 'weekly_ons_survey'))
writeLines(as.character(x1), file.path(datauk_path, 'covid', 'deaths', 'weekly_ons_survey.date'))

message('Done!')
rm(list = ls())
gc()

# y <- fst::read_fst(file.path(dmpkg.funs::datauk_path, 'covid', 'cases', 'weekly_ons_survey'))
# bnd <- sf::st_read(file.path(dmpkg.funs::ext_path, "uk", "geography", "boundaries", 'shp'), 'CIS') %>% 
#             dplyr::inner_join(y) %>% 
#             rmapshaper::ms_simplify()
# pal <- leaflet::colorNumeric('YlGnBu', bnd$pct_pos)
# dmpkg.funs::basemap() %>% 
#     leaflet::addPolygons(
#         data = bnd, 
#         color = ~pal(pct_pos), 
#         stroke = FALSE, 
#         fillOpacity = 0.8,
#         label = ~dmpkg.funs::add_pct(pct_pos, 2),
#         labelOptions = dmpkg.funs::lbl.options,
#         popup = ~CISn
# )
