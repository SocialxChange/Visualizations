# Cargamos credenciales de .Renviron
# API key se obtiene para este caso en
# https://datos.observatoriologistico.cl/developers/
api_key<-Sys.getenv("OBSLogkey")
## Usando paquete junr 
### install.packages("devtools")
### devtools::install_github("FvD/junr")
library("junr")
# https://mran.microsoft.com/snapshot/2017-02-04/web/packages/junr/README.html
# Descargamos datos del pib regional
base_url <- "https://api.datos.observatoriologistico.cl/api/v2/datastreams/"
dataOBSLog <- get_data(base_url, api_key,"PIB-REGIO-DE-CHILE-2010")

