## Playing with ggmap
# Increasing default memory of rJava
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
options(java.parameters = "-Xmx5000m")

# packages used
library(rJava)
library(githubinstall)
library(pdftools)
library(tabulizer)
library(stringr)
library(xml2)
library(dplyr)
library(foreach)
library(ggmap)

# Get the names of each places
data=read_xml("http://web.servel.cl/archivos.xml")
nom_comunas=data %>% xml_find_all("//nomcomuna")
lista_nom_comunas=xml_text(nom_comunas)

########################################################
# extract data from the .pdf and store them into a csv
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

# Get the latitud and longitud for each address.
geo_reply = geocode(sample$address, output='all', messaging=TRUE, override_limit=TRUE)

longitud=as.vector(NA)
latitud=as.vector(NA)
for (i in 1:nrow(sample)) {
  longitud[i]=geo_reply[[i]]$results[[1]]$geometry$location$lng 
  latitud[i]=geo_reply[[i]]$results[[1]]$geometry$location$lat 
}

# Add the variables to the base
sample$longitud=longitud
sample$latitud=latitud

# Plotting
santiagoMap=qmap("Comuna de Santiago,RM, Chile",zoom = 14, color = "bw", legend = "topleft")
santiago_map+geom_point(data=sample,aes(x=longitud,y=latitud, color=sexo), size=1, alpha=0.5)

santiago <- get_map('santiago, chile', zoom = 14)
santiago_map <- ggmap(santiago, extent = "device", legend = "topleft")

santiago_map + stat_density2d(aes(x=longitud,y=latitud, fill = ..level..,alpha=..level..),
                              size = 0.1, bins = 100, data = sample,geom = "polygon")+ theme(legend.position="none")

