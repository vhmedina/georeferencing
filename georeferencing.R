## Playing with ggmap
# Increasing default memory of rJava
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # If you are using windows
options(java.parameters = "-Xmx5000m")

# packages used
library(rJava)
library(pdftools)
library(tabulizer)
library(stringr)
library(xml2)
library(dplyr)
library(foreach)
library(ggmap)
library(ggplot2)

# Get the names of each place
Sys.setlocale("LC_ALL", "pt_PT.UTF-8")
data=read_xml("http://web.servel.cl/archivos.xml")
comunas=data %>% xml_find_all("//archcomuna")
nom_comunas=data %>% xml_find_all("//nomcomuna")
lista_nom_comunas=xml_text(nom_comunas)
head(lista_nom_comunas)
lista_comunas=xml_text(comunas)
head(lista_comunas)
url_base="http://web.servel.cl/padron/"

# Just an example of two counties
lista_nom_comunas=lista_nom_comunas[which(lista_nom_comunas %in% c("La Florida","Puerto Varas"))]
lista_comunas=lista_comunas[which(lista_nom_comunas %in% c("La Florida","Puerto Varas"))]

# Save .pdf in local machine
for(i in 1:length(lista_comunas)){
  archivo=lista_comunas[i]
  nombre=lista_nom_comunas[i]
  download.file(paste0(url_base,archivo),paste0(nombre,".pdf"),method="internal",mode="wb")
}
########################################################
# extract data from the .pdf and store them into a .Rdata
########################################################
foreach(i= 1:length(lista_nom_comunas)) %dopar%{
  aux_comuna=lista_nom_comunas[i]  
  out_aux=extract_tables(paste0(aux_comuna,".pdf"),encoding="UTF-8")
  aux_data=foreach(j=1:length(out_aux),.combine='rbind') %do% {
    data.frame("nombre"= gsub("^\\s+|\\s+$", "", out_aux[[j]][-c(1,2),][,1]),
               "prs_rut"=as.numeric(str_extract(gsub("^\\s+|\\s+$", "",
                                                     str_replace(str_replace(out_aux[[j]][-c(1,2),][,3],"\\.",""),"\\.","")),"\\d*")),
               "cod_rut"=str_extract(gsub("^\\s+|\\s+$", "",str_replace(str_replace(out_aux[[j]][-c(1,2),][,3],"\\.",""),"\\.","")),".$"),
               "sexo"=gsub("^\\s+|\\s+$", "", out_aux[[j]][-c(1,2),][,4]),
               "comuna"=aux_comuna,
               "direccion"=gsub("^\\s+|\\s+$", "", out_aux[[j]][-c(1,2),][,5]),
               "circunscripcion"=gsub("^\\s+|\\s+$", "", out_aux[[j]][-c(1,2),][,6]))
    
  }
  save(aux_data,file =paste0("./data/",aux_comuna,".RData"))  
}

########################################################
# Georeferencing with ggmap
########################################################

# Getting a sample from the data
sample=data_sample %>% 
  filter(direccion!="") %>% 
  sample_n(2300) %>% 
  mutate(address=paste0(direccion,", ",comuna))

# Get the latitud and longitud for each address 
geo_reply = geocode(sample$address, output='all', messaging=TRUE, override_limit=TRUE)
longitud=as.vector(NA)
latitud=as.vector(NA)
for (i in 1:nrow(sample)) {
  longitud[i]=geo_reply[[i]]$results[[1]]$geometry$location$lng 
  latitud[i]=geo_reply[[i]]$results[[1]]$geometry$location$lat 
}

# If you are using you API credential, you can create the following function to include it.
get_geocode = function(location, api_key){
  location = gsub(' ','+',location)
  geo_data =getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,sprintf("&key=%s",api_key), sep=""))
  geo_data=fromJSON(geo_data)
  geo_data$results[[1]]$geometry$location
}

# Add the variables to the base
sample$longitud=longitud
sample$latitud=latitud

# Plotting
santiago = get_map('santiago, chile', zoom = 14)
santiago_map = ggmap(santiago, extent = "device", legend = "topleft")

santiago_map + stat_density2d(aes(x=longitud,y=latitud, fill = ..level..,alpha=..level..),
                              size = 0.1, bins = 100, data = sample,geom = "polygon")+ theme(legend.position="none")+
geom_point(aes(x=longitud,y=latitud),size = .1,data = sample)

