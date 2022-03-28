########################################################
# CoViD-19 UK * DATA COVID - HOSPITAL ACTIVITY - PLOTS #
########################################################

dmpkg.funs::load_pkgs('data.table', 'ggplot2')

# daily
plot_covid_hosp <- function(x, date_start = '2021-10-01', dgr = NA, dbrk = '2 week', dlbl = '%d %b'){
    library(ggplot2)
    Name <- 'RGN'
    lbl <- fread(file.path(datauk_path, 'covid', 'hospitals', 'codes.csv'))[code == x]
    y <- readRDS(file.path(datauk_path, 'covid', 'hospitals', 'datasets'))
    y <- y[['d']][code == x & datefield >= date_start]
    yg <- ggplot(y, aes(datefield, value)) + 
              geom_line(data = y[datefield <= max(datefield) - 6], color = '#A6A6A6') + 
              geom_line(data = y[datefield >= max(datefield) - 6], color = 'red')
    if(!is.na(dgr)) yg <- yg + geom_smooth(method = 'lm', formula =  y ~ poly(x, degree = dgr), size = 0.5)
    yg + 
      facet_wrap(~paste0(get(Name), ' (', formatC(y[datefield >= max(datefield), sum(value), get(Name)][, V1], big.mark = ','), ')')) +
      labs(
          title = paste0(lbl$description, ' (England: ', 
              formatC(y[datefield >= max(datefield), sum(value)], format = 'fg', big.mark = ','), ')'
          ), 
          subtitle = paste0(
              'Date of report: ', format(Sys.Date(), '%d %B'), '\n',
              'Last available data: ', format(max(y$datefield), '%d %B')
          ),
          x = 'Date', 
          y = 'Admissions',
          caption = 'Source: NHS COVID-19 Hospital Activity https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = dbrk, date_labels = dlbl) +
      scale_y_continuous(label = scales::comma)
}
plot_covid_hosp(101)
