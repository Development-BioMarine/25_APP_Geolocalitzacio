#----------------Llibreries necesaries----------------------------------

library(shiny)

library(exifr)

library(dplyr)

library(leaflet)

library(xml2)

library(sf)

library(stringr)

library(terra)

library(base64enc)

library (lubridate)

library(magick)

library(leafpop)

library(knitr)

library(DT)



#----------------Configurem el tamany max dels arxius de l APP------------------

options(shiny.maxRequestSize = 999*1024^2)

#----------------Configurar exif----------------------------------------------


#Carregem exif portable ( nomes l executable, no la app) a una carpeta de l app

exifr::configure_exiftool(command = "./WWW/exif/exiftool.pl",

                          perl_path =  "./WWW/exif/perl.exe" ,quiet = FALSE)


################ Funcions necesaries per gestio del track  #####################

#-----------------funcio per sincronitzar gps i camara----------------------


syncro <- function (hora_gps,hora_camara,difer_seg_manual) {


  if (hora_gps == "0000/00/00 00:00:00" | hora_gps == "" | is.null(hora_camara )) {

    dfrn  <- as.integer(difer_seg_manual)

    return (dfrn)

  } else {

    dfrn <- (difftime( as_datetime(hora_camara),as_datetime(hora_gps),units = "sec"))

    return ( dfrn )

  }}


#-------------------funcio per obtenir el dataframe del gps amb temps-----------

track <-function(track_dir){

  x <- xml2::read_html(track_dir)

  gpx_tags <- xml2::xml_find_all(x, "//trkpt")

  gpx <- xml2::xml_attrs(gpx_tags)

  gpx.vec <- unlist(gpx,use.names = TRUE)

  lng <- as.numeric(gpx.vec[names(gpx.vec)=="lon"])

  lat <- as.numeric(gpx.vec[names(gpx.vec)=="lat"])

  gpx_tags_t <- xml_find_all(gpx_tags, ".//time")

  gpx_time <-with_tz( ymd_hms(as_datetime(xml_text( gpx_tags_t))),tzone = "Europe/Madrid")



  gpxdf <- data.frame(

    cbind(

      lng,

      lat),

    gpx_time)

  names(gpxdf) <- c("longitude","latitude","time")

  return(gpxdf)

}

#----------------Funcio per convertir df (del gps) a sf----------------------

gpxdf_sf <- function(df,crs){

  sf <- st_as_sf(df,coords= c(1,2),crs="+proj=longlat +datum=WGS84")

  y <- st_combine(sf)

  g <- st_cast(y, 'LINESTRING')

  return (g)

}

#-------funcio per tobar el minim df$time es el vector dels temps del track, data es el temps de la foto

posicio_foto <- function (hora_foto, df){

  psc <-df[ which.min(sapply (1:nrow(df), FUN=function(i) (abs(difftime(df$time[i],hora_foto, units ="sec"))))),1:3]

  return (psc)


}

#----------funcio per pasar de datetime a hora--------------------------------------

to_hour <- function(data) {

  hora_data <- as.character( hour(as_datetime(data)))

  minut_data<- as.character( minute(as_datetime(data)))

  segons_data<-as.character( second(as_datetime(data)))


  hora <- paste0(hora_data,":",minut_data,":",segons_data)

  return(hora)

}

#############################################################################



##############################################################################

##############################################################################



#################  AQUI COMENÇA L APP  #######################################



##############################################################################

#############################################################################



ui <- fluidPage(

  h3("GEOLOCALITZACIO DE FOTOGRAFIES"),

  br(),

  navlistPanel(

    id = "tabset","MENU",

    ################    PAGINA SINCRONITACIO D EQUIPS   ###############################

    #------------------Entrada sincro---------------------------------------------------

    tabPanel("Sincronitzacio d instruments", "Sincronitzacio de camara i gps/movil: Carreguem una fotografia del GPS o movil on aparegui la hora amb resolucio de segons o just en el moment que canvii la minutera o si s ajusta manualment potser qualsevol foto" ,

             sidebarLayout(

               sidebarPanel(

                 fluidRow(

                   fileInput("myrefcam", "Fotografia de referencia del gps/mobil", accept = c('image/png', 'image/jpeg'),multiple = FALSE),

                   br(),

                   textInput("hora_gps","Entra el valor de la data y hora que apareix a la foto del gps o mobil en format yyyy/mm/dd hh:mm:ss","0000/00/00 00:00:00"),

                   br(),

                   textInput("difer_seg_man","Si ho desitges pots sincronitzar directamenta fegint o treient segons","0000"),

                   br(),

                   actionButton("sincro","Sincronitzar camara/gps")

                 )),

               #-------------Sortida sincronitzacio--------------------------------------------------

               mainPanel(

                 div(id = "image-container", style = "display:flexbox"),

                 htmlOutput( outputId = "exif_ref"),

                 htmlOutput( outputId = "syncro"),

                 htmlOutput( outputId = "hora_ref_ajust")

               ) #tancament main Panel sincro

             )), #Tancament tabpanel

    ################    PAGINA DADES DEL TRACK ###############################

    #----------------Entrada track----------------------------------------------------

    tabPanel("Gestio del Track", "Comprobació del track, recorregut, hores inici i final i durada",

             sidebarLayout(

               sidebarPanel(

                 fluidRow(

                   fileInput("myTrack", "Escull arxiu track", accept = c('.gpx'),multiple = FALSE ),

                   actionButton("Data_track_button","Mapejar Track")

                 )),

               #--------------Sortida  track---------------------------------------------------

               mainPanel(

                 leafletOutput(outputId = 'map2'),

                 htmlOutput(outputId =  "data_track"),

                 htmlOutput( outputId ="temps_inicial_track"),

                 htmlOutput( outputId ="temps_final_track"),

                 htmlOutput( outputId ="durada_track"),

                 htmlOutput(outputId = "longitut_track")

               ) #Tancament mainPanel track

             )), #Tancament paneltab track



    ################    PAGINA GEOLOCALITZACIO ###############################

    #-------------Entrada geolocalitzacio  --------------------------------------

    tabPanel("Geolocalitzacio camara", "Geolocalitzacio de la fotografia" ,

             sidebarLayout(

               sidebarPanel(

                 fluidRow(

                   "Carregar la fotografia  que es vol geolocalitzar",

                   fileInput("myPic",label="", accept = c('image/png', 'image/jpeg'),multiple = TRUE),

                   "Geolocalitzar la fotografia i mostrar-la en plànol.",

                   actionButton("geoloc_button","Geolocalitzar"),

                   br(),

                   br(),

                   "Fixar posició, afegir al dataset de registres i  afegir a les metadades de la fotografia la posició",

                   actionButton("fix_button","Fixar i afegir Metadds"),

                   br(),

                   br(),

                   "Descarregar la foto amb metadades geolocalitzades un cop s ha ajustat completament la posició.",

                   downloadButton("descarrega")

                 )),

               #---------------------Sortida geolocalitzacio--------------------------------------------------

               mainPanel(

                 # textOutput( outputId = "temps_fotogeoloc"),

                 DTOutput(outputId = 'multiplefile'),

                 DTOutput(outputId = 'multiplefile_1'),

                 DTOutput(outputId = 'multiplefile_2'),

                 leafletOutput(outputId = 'map'),

                 textOutput( outputId = "multiplefile_item")

               ) #Tancament main panel

             )), #Tancament tabpanel geolocalitzacio


    #---Tancament del costat client------------------------------------

  ))


###############################################################################

#####################      SERVER     #########################################

##############################################################################


server <- function(input, output) {


  #############################################################################


  #Variables reactives sincronitzacio GPS/ Càmara


  #############################################################################



  #Per obtindre el path de la foto de referencia-------------------------------


  pic_ref <- reactive({

    if (is.null(input$myrefcam$datapath))

      return()

    as.character( input$myrefcam$datapath)

  })

  #Per obtindre les dades exif de la foto referencia-----------------------

  dades_foto_ref <- reactive ( { if (is.null(input$myrefcam$datapath)){


    return()}

    exifr::read_exif( pic_ref(), tags = 'DateTimeOriginal')} )



  #Del temps extret de l Exif-----------------------------------------------------


  time_foto_ref <- reactive (  { if (is.null(input$myrefcam$datapath)){


    return()}


    (ymd_hms(dades_foto_ref()[,2]) )})

  #Per calcular les diferencies de temps i sincronitzar-----------------------

  #fem servir la funcio syncro definida al inici

  t_syncro <-reactive (syncro(input$hora_gps ,as_datetime(time_foto_ref()),input$difer_seg_man))

  #############################################################################

  #Variables reactives  visalitzacio inicial del track

  #############################################################################

  #Obtencio del directori del track---------------------------------------------


  trck_path <- reactive({

    if (is.null( input$myTrack$datapath))

      return()

    as.character(input$myTrack$datapath)

  })


  #Dataframe del track--------------------------------------------------------

  #Fem servir la funcio track() definida a l inici

  track_df <- reactive({

    if (is.null(trck_path())){

      return()

    }

    track(trck_path())})


  #Obtenim ultim punt del track-----------------------------------------------

  n_punts <- reactive(track_df() %>% nrow())

  Temps_Inicial_Track <-reactive(as_datetime(track_df()[1,3]))

  Temps_Final_Track <-reactive(as_datetime(track_df()[n_punts(),3]))


  #Calculem durada del track---------------------------------------------------

  Durada_Track <- reactive(round(difftime(Temps_Final_Track(),Temps_Inicial_Track(),units = "min"),2))

  #Interval de temps del track------------------------------------------------

  Interval_track <- reactive (lubridate::interval( Temps_Inicial_Track(), Temps_Final_Track()))

  #Obtenim el shape del track--------------------------------------------------

  #Fem servir la funcio de l inici shape_track() a partir del dataframe

  shape_track <- reactive( gpxdf_sf(track_df()))

  #Obtenim la longitut del track a partir del Shape----------------------------

  Longitud_track <- reactive ( round( st_length(gpxdf_sf(track_df())),0))



  #/////////////////////////////////////////////////////////////////////////////

  #-----------------------------------------------------------------------------

  #-----------------------------Gestio sincronitzacio-----------------------------

  #///////////////////////////////////////////////////////////////////////////

  #=================Input foto de referencia====================================


  observeEvent(input$myrefcam, {  #Al carregar noves imatges cal esborrar les dades de les anteriors

    camRef <- input$myrefcam

    if (is.null(camRef))

      return()

    removeUI(selector = "#image-container > * ")

    b64 <- base64enc::dataURI(file = camRef$datapath, mime = "image/png")

    insertUI(

      selector = "#image-container",

      where = "afterBegin",

      ui = img(src = b64, width = 250, height = 250)

    )

    #--------Condicio foto amb metadades-------------------------------------

    if (ncol(dades_foto_ref())<2 ){

      showNotification("La foto no te metadades")

      return()}

    # -----Extraccio de la hora de la foto de referencia------------------------

    output$exif_ref <- renderUI(HTML(paste0("Les metadades de la imatge de referencia son:", "<br/>",

                                            "- Dia: ",

                                            as.character(date(as_datetime(time_foto_ref()))), "<br/>",

                                            "- Hora:  ",

                                            to_hour(time_foto_ref())
    ) )) #Tancament del RenderUI------------------------------------------------

  }) #=====Tancament de la carrega de la foto de referncia=========================


  #========================Boto de sincro========================================

  observeEvent(input$sincro, {

    #-----Condicio foto carregada-----------------------------------------------

    if (is.null(pic_ref()) ) {

      showNotification("Es necesari carregar foto de referencia")

      return() }

    #---Condicio foto amb metadades------------------------------------------------

    if (ncol(dades_foto_ref()) < 2 ) {

      showNotification("La foto no te metadades")

      return() }

    #-----Sortida de la info de sincronitazacio-------------------------------------

    output$syncro <- renderUI(HTML(paste0("<br/>","El temps a sincronitzar en segons es de: ",

                                          isolate( as.character(t_syncro())),"segons","<br/>" ) ))

    output$hora_ref_ajust <- renderUI(HTML(paste0("<br/>","El nou temps sincronitzat ajustat és:","<br/>",

                                                  "- Dia: ",

                                                  isolate( as.character(date(as_datetime(time_foto_ref() + t_syncro())))),"<br/>",


                                                  "- Hora: ",

                                                  isolate(to_hour(time_foto_ref() + t_syncro())))


    )) #Tancament de l output de render text


  }) #==========================Tancament boto sincro==========================



  #/////////////////////////////////////////////////////////////////////////////

  #-----------------------------------------------------------------------------

  #-----------------------------Gestio del track------------------------------

  #///////////////////////////////////////////////////////////////////////////


  #==========================Boto de dibuix del track=========================

  observeEvent(input$Data_track_button,{

    #---Condicio de track carregat per dibuixar-lo----------------------------

    if (is.null(trck_path()) ) {

      showNotification("No hi ha cap track carregat")

      removeModal()

      return()

    }

    #---------------Mapa del track-------------------------------------------------

    output$map2 <- (renderLeaflet({



      leaflet() %>%

        addPolylines(data = shape_track(),color = "red",weight = 2) %>%

        addProviderTiles(providers$Esri.WorldStreetMap,group = "WSMWorld Street Map") %>%

        addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%

        addLayersControl(baseGroups = c("Satel.lit","World Street Map" ))

    })) #tancament del mapa de track-------------------------------------------


    output$data_track <- renderUI(HTML(paste("<br/>","El track és va efectuar el dia :",isolate( as.character(date(as_datetime(Temps_Inicial_Track()) )) ),"<br/>")))

    output$temps_inicial_track <- renderUI( HTML(paste0("- La hora inicial del track és: ",

                                                        isolate(to_hour(Temps_Inicial_Track()))) ))


    output$temps_final_track <- renderUI(HTML(paste0( "- La hora final del track és: ",

                                                      isolate( to_hour( Temps_Final_Track() ) ) ) ))


    output$durada_track <- renderUI(HTML( paste("La durada del track és:",  isolate(as.character(Durada_Track())), "min")))

    output$longitut_track <- renderUI(HTML( paste("El recorregut del track son:", isolate(as.character( Longitud_track())), "m")))

  }) #====================tancament boto dibuixar track==========================


  #/////////////////////////////////////////////////////////////////////////////

  #-----------------------------------------------------------------------------

  #-----------------------------Gestio geolocalitazio fotos-----------------------------

  #///////////////////////////////////////////////////////////////////////////

  #-------------Preparacio de varilables per la geolocalitzacio multiple------------

  posicio_rv <- reactiveValues(datos = data.frame(id= 1,
                                                   #imagen=NULL,
                                                   SourceFile= "./WWW",
                                                   lng = 15,
                                                   lat = 35,
                                                   time_s_corr = 1,
                                                   time = 1,
                                                   intrack ="Out track"))



  coords_rv <- reactiveValues(data = data.frame(id= 1,
                                                 #imagen=NULL,
                                                 SourceFile= "./WWW",
                                                 lng = 15,
                                                 lat = 35,
                                                 time_s_corr = 1,
                                                 time= 1,
                                                 intrack= "Out track"))





  #================Carrega de les fotos a geolocalitzar=====================================


  observeEvent(input$myPic, {

    if (is.null(input$myPic$datapath))

      return()


    n_fotos <- reactive(nrow(input$myPic))

    assign("nfotos", n_fotos(), envir = globalenv())

    #Creem vector de temps de sincro per poder aplicar purr::

    t_syncro_v <- as.list(rep(t_syncro(), n_fotos()))

    #------Funcio per extreure el temps--------------------------------------------------

    foto_geoloc <- function(pat,sincro)  {

      time_s <- (as.data.frame(exifr::read_exif(pat, tags = 'DateTimeOriginal')))

      time_s_tz <- (force_tz( as_datetime( ymd_hms( time_s[,2])), tzone = "Europe/Madrid" ))

      time_s <- cbind(time_s,time_s_tz)

      time_s_corr <- (as_datetime(time_s[,3] + sincro))

      time_s <- cbind(time_s,time_s_corr)

      return(time_s)

    }

    #----------------------------------------------------------------------------

    #------Creem el DF que contindra tota la info----------------------------

   all_files <- ({

      req(input$myPic)

      purrr::map2_dfr(input$myPic$datapath,t_syncro_v,foto_geoloc)

    })


    #ordenem les dates per poder asignar el id de forma ordenada

    all_files <- all_files[order (all_files$time_s_corr),]

    #dplyr::arrange(all_files,time_s_corr)

# despres d ordenar asignem l ID

    id <- as.data.frame(seq_len(n_fotos()))

    names(id) <- c("id")

     all_files_1 <- cbind(all_files,id)

     #Creem la columna d´imatges


contenido <- sapply(all_files$SourceFile, function(x){paste0("data:image/",tools::file_ext(x), ";base64,",
                                                             base64encode(x))})

    all_files_1 <- cbind(all_files_1,contenido)


    imagen <- sapply(all_files_1$contenido,function(x){HTML(paste0("<img src='",x,"' width='100' height='100'>"))})
     all_files_1 <- cbind(all_files_1,imagen)



     #Posem id com a primera columna

    all_files_1 <- all_files_1 %>% select("id","SourceFile","DateTimeOriginal","time_s_tz","time_s_corr","imagen")

  #Creem un vector de interval de temps del track per poder-lo aplicar amb purr::

    Interval_track_v <- as.list(rep(Interval_track(), n_fotos()))


    #-----------Funcio per comprobar que el temps de les fotos estan dins l inetrval de temps del track


    intrack <- function(tgeoloc_corret, interval_track) {

      if (!((tgeoloc_corret)  %within% interval_track)) {



        intrack <- "Out track"

      }else{

        intrack <- "In track"

      }

      return(as.data.frame( intrack))

    }


    #apliquem la funcio anterior a totes les fotos carregades

    all_files_in_track <- ({

      req(input$myPic)

      purrr::map2_dfr(all_files_1$time_s_corr,Interval_track_v, intrack)

    })


    #Adjuntem el vector a la matriu total


    all_files_1 <- cbind(all_files_1,all_files_in_track)


    assign("all_files1", all_files_1, envir = globalenv())

    all_files_TABLE <- all_files_1 %>% select("id","imagen","DateTimeOriginal","time_s_tz","time_s_corr","intrack")

    output$multiplefile <- renderDT( all_files_TABLE,escape = FALSE)

  })

  #=============Geolocalitzacio de la foto carregada al apretar el boto=============

  observeEvent(input$geoloc_button,{

    #------condicio trackcarregat----------------------------------------------

    if (is.null (trck_path()) ){

      showNotification("No hi ha cap track carregat")

      return()}


    #-----condicio foto a georeferneciar carregada------------------------------


    if (is.null (input$myPic) ){

      showNotification("Es necesari carregar fotos a georeferenciar")

      removeModal()

      return()}

 #-----esborrem el mapa si n hi havia un


   #output$map<-NULL

    #coords_rv$data<-(posicio_rv$datos)

    # nde files del dataframe

    #Funcio per expreure les posicions de les fotos en cas que siguin dintre del track


    coordenada <- data.frame()


    for(i in 1:nfotos) {

       if (all_files1[i,7] == "Out track"){


    coordenada_item <- data.frame (longitude = NA,latitude = NA,time = ( all_files1[i,5]))

    }else{

    coordenada_item <- as.data.frame(posicio_foto(all_files1[i,5],track_df()))

}  #tancament ifelse

 coordenada <- bind_rows(coordenada, coordenada_item)

    } #tancament loop

    names(coordenada) <- c("lng","lat","time")

    all_files_33 <- (cbind(all_files1,coordenada))

    #Cal extreure els que no tenen long/lat

    all_files_3 <- subset(all_files_33, !is.na(lat))

    all_files_3 <- subset(all_files_33, !is.na(lng))


    #Creem un df amb els camps mes impoertants

    posicio_rv$datos <- (select(all_files_3,"id","SourceFile","lng","lat","time_s_corr","time","intrack","imagen"))


    ###############################################################

    #Planol de les observacions---------------------------------------------

    output$map <- renderLeaflet({

      leaflet() %>%
        addPolylines(data = shape_track (),color = "red",weight = 4) %>%
        addProviderTiles(providers$Esri.WorldStreetMap,group = "WSMWorld Street Map") %>%
        addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
        addLayersControl(baseGroups = c("Satel.lit","World Street Map"))%>%

        addMarkers(data = (posicio_rv$datos),

                   group = "dades_finals",

                   layerId = posicio_rv$datos$id,

                   lng= isolate(posicio_rv$datos$lng),

                   lat= isolate(posicio_rv$datos$lat),

                   options = markerOptions(draggable = TRUE),

                   popup =  paste ( "<br>",
                                    "ID: ", as.character(posicio_rv$datos$id),
                                    "<br>"))%>%

        addPopupImages(posicio_rv$datos$SourceFile,group = "dades_finals", width = 150)



    })

    ########################Tancament del map de geolocalit fotos####################

coords_rv<- reactiveValues(data = data.frame(isolate(posicio_rv$datos)))

output$multiplefile_1 <- renderDT({

    df_selecc <- coords_rv$data[, c("id","imagen","time_s_corr","time","lng","lat")]


  },escape = FALSE)


    #####################     Drag dels markers #####################################

    observeEvent(input$map_marker_dragend, {


      markerId <- input$map_marker_dragend$id

      lat <- input$map_marker_dragend$lat

      lng <- input$map_marker_dragend$lng

      coords_rv$data[coords_rv$data$id == markerId, c("lng", "lat")] <- c(lng, lat)


      leafletProxy('map') %>% clearMarkers() %>%

        addMarkers(data = coords_rv$data,

                   group = "dades_finals",

                   lng = ~lng, lat = ~lat,

                   layerId = ~id,

                   options = markerOptions(draggable = TRUE),

                   popup =  paste ( "<br>",
                                     "ID: ", as.character(coords_rv$data$id),
                                     "<br>")) %>%


        addPopupImages(coords_rv$data$SourceFile,group = "dades_finals", width = 150)
    })


    #=============Tancament de dragent=======================================

  }) # tancament boto geoloc


  #===============Tancament  boto de les fotos a geolocalitzar================


  #---------------------------------------------------------------------------

} #tanemnt server


shinyApp(ui = ui, server = server)
