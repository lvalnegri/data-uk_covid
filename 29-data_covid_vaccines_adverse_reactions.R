########################################################
# CoViD-19 UK * DATA COVID - VACCINE ADVERSE REACTIONS #
########################################################

message('Validating update...')
url <- 'https://www.gov.uk/government/publications/coronavirus-covid-19-vaccine-adverse-reactions/coronavirus-vaccine-summary-of-yellow-card-reporting'
x1 <- as.Date( 
        gsub('Updated ', '', rvest::html_text(rvest::html_node(xml2::read_html(url),'.publication-header__last-changed'))),
        '%d %B %Y'
)
x2 <- readLines(file.path(dmpkg.funs::datauk_path, 'covid', 'vaccine', 'vaccine_adverse_reactions.date'))
if(is.na(x1)) q(save = 'no')
if(x1==x2) q(save = 'no')

message('Loading packages...')
dmpkg.funs::load_pkgs(c('data.table', 'rvest'))

file.copy(
    file.path(datauk_path,'covid', 'vaccine', 'vaccine_adverse_reactions'), 
    file.path(datauk_path,'covid', 'vaccine', paste0('vaccine_adverse_reactions_', x2))
)

lnks <- xml2::read_html(url) %>% html_nodes('.govuk-link') %>% html_attr('href')
brands <- c( 
    'Pfizer' = lnks[grepl('pfizer.*pdf', tolower(lnks))], 
    'AstraZeneca' = lnks[grepl('astra.*pdf', tolower(lnks))],
    'Moderna' = gsub('draft-', '', lnks[grepl('moderna.*pdf', tolower(lnks))]) 
)

get_data <- function(x){
  
    message('\nProcessing brand: ', names(x), '...')
  
    message(' - Downloading file and extracting tables...')
    y <- tabulizer::extract_tables(x, method = 'stream')
    
    message(' - Processing output... ')
    ydt <- data.table()
    for(idx in 1:length(y)){
        yt <- y[[idx]]
        if(length(yt) > 1){
            if(ncol(yt) > 3){
                if(ncol(yt) == 4){
                    if(yt[1,2] == ''){
                        yt <- yt[, c(1, 3, 4)]
                    } else if(yt[1,4] == ''){
                        yt <- yt[, c(1, 2, 4)]
                    } else {
                        message('Format not recognized!')
                    }
                } else if(ncol(yt) == 5){
                    yt <- yt[, c(1, 3, 5)]
                } else {
                    message('Too many columns!')
                }
            }
            yt <- if(nrow(yt) == 3) data.table(t(yt[-(1:2), ])) else data.table(yt[-(1:2), ])
            ydt <- rbindlist(list( ydt, yt), use.names = FALSE)
        }
    }
    
    message(' - Data Engineering... ')
    ydt <- ydt[!grepl('^TOTAL', V1)]
    ydt[, V4 := ifelse(V3 == '', V1, NA)][, V4 := zoo::na.locf(V4)]
    ydt[, V5 := ifelse(grepl('TOTAL$', V1), V1, NA)][, V5 := zoo::na.locf(V5, fromLast = TRUE)]
    ydt <- ydt[!(V3 == '' | grepl('TOTAL$', V1))]
    ydt[, `:=`( V2 = as.integer(V2), V3 = as.integer(V3), 'Brand' = names(x) )]
    setcolorder(ydt, c('Brand', 'V5', 'V4'))
    setnames(ydt, c('Brand', 'Class', 'Group', 'Reaction', 'Total', 'Fatal'))
    ydt[, Class := gsub(' SOC TOTAL', '', Class)]

}

ydt <- rbindlist(list( get_data(brands[1]), get_data(brands[2]), get_data(brands[3]) ))

message('Check totals...')
ydt[, .(Total = sum(Total), Fatal = sum(Fatal)), Brand]
ydt[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand, Class)]

message('Fixing some values... ')
# ydt <- rbindlist(list( ydt, data.table( 'Pfizer', 'General disorders', 'Death', 'Death', 101, 101) ), use.names = FALSE)
# ydt[
#     Brand == 'AstraZeneca' & Class == 'General disorders' & Group == 'Death and sudden death' & Reaction == 'Death', 
#     `:=`( Total = 141, Fatal = 141 )
# ]

message('Add Rank over Total by Class... ')
ydt[, rnk := frank(-Total, ties.method = 'random'), .(Brand, Class)]

message('Writing datasets to files...')
fst::write_fst(ydt, file.path(datauk_path,'covid', 'vaccine', 'vaccine_adverse_reactions'))
writeLines(as.character(x1), file.path(datauk_path, 'covid', 'vaccine', 'vaccine_adverse_reactions.date'))

message('Done!')
rm(list = ls())
gc()
