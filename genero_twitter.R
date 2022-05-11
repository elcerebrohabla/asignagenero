## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)
# Paquetes
library(pacman)
p_load(readxl, writexl, tidyverse, dplyr, cowplot, janitor, lmtest, 
       sandwich, sjPlot, pander, pscl, haven, gridExtra, ggExtra, 
       hexbin, janitor, mosaicData, scales, ggthemes, tweetbotornot, rtweet,
       lubridate, hms, tidytext, wordcloud2, tm, SnowballC, htmlTable, kableExtra,
       magick, magrittr, pdftools)

#Conectarse a Twitter

token <- create_token(
  "My Application Name",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = ""
)
get_token()

#Crear las bases de datos a partir de los PDF
#Les recomiendo crear un proyecto, crear una carpeta "01_datos" y
#Poner ahí los PDF

tx <- pdf_text("01_datos/NombresFemeninosJulio2015.pdf")
tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
nombres_fem<-as.data.frame(tx3)
nombres_fem$V2<-NULL
nombres_fem$V3<-NULL
nombres_fem$V4<-NULL
nombres_fem$V5<-NULL 
nombres_fem = nombres_fem %>% 
  filter(!grepl("Fecha", V1) & !grepl("Listado", V1) & !grepl("Desde", V1))

tx <- pdf_text("01_datos/NombresMasculinosJulio2015.pdf")
tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)
nombres_masc<-as.data.frame(tx3)
nombres_masc$V2<-NULL
nombres_masc$V3<-NULL
nombres_masc$V4<-NULL
nombres_masc$V5<-NULL 
nombres_masc = nombres_masc %>% 
  filter(!grepl("Fecha", V1) & !grepl("Listado", V1) & !grepl("Desde", V1))

#Por si quieren guardar las bases de datos en excel (no es necesario
#para que funcione el código)

write_xlsx(nombres_fem, "01_datos/nombres_fem.xlsx")
write_xlsx(nombres_masc, "01_datos/nombres_masc.xlsx")

nombres_fem <- read_excel("01_datos/nombres_fem.xlsx")
nombres_masc <- read_excel("01_datos/nombres_masc.xlsx")

# Importar Tweets
tuits = search_tweets(q = "Debahni", n = 300000)

# Importar Tweets Python
tuits <- read_csv("01_datos/feminazi.csv") %>% 
  dplyr::rename(screen_name = User, text = Text, user_id = "Tweet Id")

glimpse(tuits)
name<- tuits %>% 
  dplyr::select(screen_name, user_id)
#Eliminamos nombres duplicados 
name_clean<-name[!duplicated(name$screen_name), ]
name_clean_no_num<- unlist(strsplit(name_clean$screen_name, split = '\\s+'))
name_no_numbers<- gsub('[0-9]+', '', name_clean_no_num)
name_no_numbers<- tolower(name_no_numbers)
name_clean_no_num<- data.frame(name_no_numbers)
name_split<- name_clean_no_num%>% 
  mutate(name_no_numbers = sub("_", ",_", name_no_numbers)) %>% 
  separate(name_no_numbers, into = c("letter", "extra"), sep = ",")
nombres_fem$V1<- tolower(nombres_fem$V1)
nombres_fem<- as.tibble(nombres_fem)
nombres_masc$V1<- tolower(nombres_masc$V1)
nombres_masc<- as.tibble(nombres_masc)

#Filtramos campos vacíos
nombres_fem <- nombres_fem %>% 
  filter(V1 != "") %>% 
  filter(V1 != "Total Nombres FEMENINOS Registrados:")

nombres_masc <- nombres_masc %>% 
  filter(V1 != "") %>%  
  filter(V1 != "Total Nombres FEMENINOS Registrados:")

#Convertimos las bases de datos a matrices
nom_masc <-  as.matrix(nombres_masc$V1)
nom_fem <-  as.matrix(nombres_fem$V1)

#Buscamos la correpondencia entre los nombres de usuario y la listas. Se crean
# dos columnas para ver a quienes los detecta como mujer, como hombre, y con NA
# aquellas cuentas que no se pudieron determinar
final <- tuits %>%
  tidyr::separate_rows(screen_name, sep = ",|\\s+") %>% 
  
  mutate(mujer = case_when(
    str_detect(screen_name, paste0(nom_fem,collapse = '|'), negate = F) ~ "Sí",
    TRUE  ~  "NO"
    )) %>%
  mutate(hombre = case_when(
    str_detect(screen_name, paste0(nom_masc,collapse = '|'), negate = F) ~ "Sí",
    TRUE  ~  "NO"
  )) %>% 
  # Hay casos que los detecta como ambos géneros, entonces vamos a eliminarlos
  # y dejar solo a los que se le asigna uno para mayor precisión.
  filter(mujer == "NO" | hombre == "NO") %>% 
  mutate(genero = case_when(
    hombre == "Sí" ~ "H",
    mujer == "Sí" ~ "M"
  )) %>% 
  separate(Datetime, into = c('date', 'time'), sep=' ', remove = FALSE) 
  

## Aquí pueden ver la relación final
final %>% 
  dplyr::select(screen_name, genero) %>% 
  view()

#Ejemplo de uso. Bigramas por género en el caso de Debahni:

# Stopwords ----
custom_stop_words <- as_tibble(tm::stopwords("es")) %>% 
  bind_rows(as_data_frame(c(
    "si","dijo","así","sólo", "dice", "pues","entonces",
    "ahí","digo","creo","que","en","la","ah","bueno", "bla","tan",
    "te", "iba", "he", "él", "t", "+", "de", "cómo", "su", "https", "t.co"
  ))) %>% 
  dplyr::rename(palabra = value)

stopwords <- as.matrix(custom_stop_words)

#BIGRAMAS

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

#Especificar género "M" o "H" 
genero <- final %>% 
  filter(genero == "H")

glimpse(final)

bigramas <- genero %>% 
  dplyr::select(text) %>% 
  #mutate(texto = limpiar(cuentas)) %>%
  #select(texto) %>%
  unnest_tokens(input = text, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE) %>% 
  separate(bigrama, c("palabra1", "palabra2"),
           sep = " ") %>% 
  filter(!palabra1 %in% stopwords) %>%
  filter(!palabra2 %in% stopwords) %>% 
  unite(palabra1, palabra2, col="bigrama", sep=" ") %>% 
  group_by(bigrama) %>% 
  count() %>% 
  arrange(desc(freq)) 

glimpse(bigramas)

bigramas %>% 
  top_n(100) %>% 
  kbl() %>%
  kable_styling()

#Contar por género

final %>% 
  filter(!is.na(genero)) %>% 
  ggplot(aes(genero))+
  geom_bar()

final %>% 
  filter(!is.na(genero)) %>% 
  dplyr::select(genero, date) %>% 
  group_by(date) %>% 
  count() %>%
  ungroup() %>% 
  ggplot(aes(date, freq, color=genero, group = 1))+
  geom_point()+
  geom_path()+
  labs(title = 'Frecuencia de menciones de la palabra "feminazi"',
       subtitle = '',
       caption = "Desarrollado por @elcerebrohabla",
       x = "",
       y = "")+
  #scale_y_continuous(breaks = c(1,2), limits = c(1, 2))+
  theme_economist()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=18),
    legend.position = "none",
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

glimpse(final)
