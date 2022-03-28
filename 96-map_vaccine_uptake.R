dmpkg.funs::load_pkgs(c('data.table', 'dplyr', 'htmltools', 'htmlwidgets', 'leaflet', 'leaflet.extras', 'sf'))

dpath <- '/home/datamaps/datasets/uk_covid/data'

pal_cols <- c('PuBu', 'Greens', 'Blues', 'YlGnBu', 'Purples', 'Greys')
age_classes <- c(
    'Total_Adults' = 'S_16_99',
    '16_to_64'     = 'S_16_64',
    '65_to_69'     = 'S_65_69',
    '70_to_74'     = 'S_70_74',
    '75_to_79'     = 'S_75_79',
    '80_and_Older' = 'S_80_99'
)
classes <- substring(age_classes, 2)

fn <- list.files(dpath)
fn <- fn[grep('vaccine_msoa', fn)]
wkd <- as.Date(gsub('.*(\\d{4}.*)\\..*', '\\1', fn))

message('Reading data...')
y <- fread(file.path(dpath, fn))

message('Reading boundaries...')
bnd <- readRDS(file.path(bnduk_path, 'rds', 's20', 'MSOA'))
bnd <- st_as_sf(bnd)
bnd <- bnd %>% filter(substr(id, 1, 1) == 'E')
bnd <- bnd %>% inner_join(y, by = c('id' = 'MSOA'))
bnd <- bnd %>% st_transform(4326)


message('Building base map layer...')
mp <- leaflet() %>% 
        addProviderTiles(providers$OpenStreetMap) %>% 
        addSearchOSM() %>% 
        addResetMapButton() %>% 
        addFullscreenControl() %>% 
        addMiniMap(width = 100, height = 80, zoomLevelOffset = -4, toggleDisplay = TRUE) %>% 
        fitBounds(-8.178225, 49.958709, 1.76368, 55.325)

for(idx in 1:length(age_classes)){
    yg <- age_classes[idx]
    message('Building polygons layer for age: ', names(yg))
    pal <- colorNumeric(pal_cols[idx],bnd[[yg]])
    mp <- mp %>% addPolygons(
        data = bnd,
        group = names(yg),
        smoothFactor = 0.2,
        weight = 1,
        opacity = 0.1,
        color = "white",
        fillColor = ~pal(bnd[[yg]]),
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
            color = 'white',
            weight = 5,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = TRUE
        ),
        label = lapply(
                    1:nrow(bnd),
                    function(x)
                        HTML(paste0(
                            '<font size="+1"><b>Vaccine uptake: ', add_pct(bnd[[yg]][x]), 
                                '</b></font> (', add_Kcomma(bnd[[paste0('V', classes[idx])]][x]), ' people)', '<br><br>',
                            '<b>MSOA</b>: ', bnd$MSOAn[x], '<br>',
                            '<b>LTLA</b>: ', bnd$LTLAn[x], '<br>',
                            '<b>Region</b>: ', bnd$RGNn[x], '<br>',
                            '<b>Population PHE</b>: ', add_Kcomma(bnd[[paste0('P', classes[idx])]][x]),
                                ' (<b>ONS 2019</b>: ', add_Kcomma(bnd[[paste0('ONS', classes[idx])]][x]), ')<br>'
                        ))
        ), 
        labelOptions = lbl.options
    ) %>%
    addLegend(
        group = names(yg),
        className = paste('info legend', names(yg)),
        title = 'Uptake',
        position = 'bottomright',
        opacity = 0.8,
        pal = pal,
        values = bnd[[yg]],
        labFormat=labelFormat(suffix = '%', transform = function(x) 100 * x)
    ) %>% 
    addControl(
        HTML(paste0(
		    '<div style="padding:0px;margin:0px;line-height:8px">',
    		    '<font size=-1>',
		            '<p>Covid Vaccination Uptake in England by MSOA', 
                        ifelse(grepl('Total', names(yg)), '', paste0(' and Age ', gsub('_', ' ', names(yg)))), '</p>',
                    '<p style="margin-bottom:16px">Last Update: ', format(wkd, '%d %b %Y'), '</p>',
    		    '</font>',
    		    '<font size=-4>',
                    '<p>Contains NHS, NIMS, and PHE data licensed under the 
                        <a href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/">
                            Open Government Licence v.3.0</a>', '</p>',
                    '<p>Contains OS data © Crown copyright and database right [2021]', '</p>',
                    '<p>Source: Office for National Statistics licensed under the 
                        <a href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/">
                            Open Government Licence v.3.0</a>', '</p>',
                    '<p>Contains Parliamentary information licensed under the 
                        <a href = "https://www.parliament.uk/site-information/copyright-parliament/open-parliament-licence/">
                            Open Parliament Licence v3.0</a>', '</p>',
		        '</font>',
		    '</div>'
        )), 
        className = paste('info legend', names(yg)),
        position = 'bottomleft'
    )

}

message('Adding Menu...')
mp <- mp %>% 
    addLayersControl(
        baseGroups = names(age_classes), 
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedGroup)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
    )

message('Saving as HTML...')
saveWidget(
    mp, 
    '/home/datamaps/temp/vaccine_uptake.html', 
    title = paste('Vaccinations Uptake in England by MSOA and Age Classes. Last Update:', format(wkd, '%d %b %Y'))
)
system('mv /home/datamaps/temp/vaccine_uptake.html /var/www/html/outputs/covid/vaccine_uptake.html')

message('Done!')
rm(list = ls())
gc()
