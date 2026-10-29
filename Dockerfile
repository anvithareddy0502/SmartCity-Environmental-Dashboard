
FROM rocker/shiny:4.4.2

RUN R -e "install.packages(c('shiny','ggplot2','plotly','leaflet','dplyr','tidyr','readr','lubridate','scales','viridis','DT','GGally'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
