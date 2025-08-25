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

    names(gpxdf) <- c("lng","lat","time")

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

#################  AQUI COMENÇA L APP

##############################################################################
#############################################################################

ui <- fluidPage(
    h3("GEOLOCALITZACIO DE FOTOGRAFIES"),
    br(),
    navlistPanel(
        id = "tabset","MENU",

################    PAGINA SINCRONITAZCIO D EQUIPS###############################

        #---------------------------------------------------------------------------------------

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

#----Sortida sincronitzacio--------------------------------------------------


                     mainPanel(

                         div(id = "image-container", style = "display:flexbox"),

                         htmlOutput( outputId = "exif_ref"),

                         htmlOutput( outputId = "syncro"),

                         htmlOutput( outputId = "hora_ref_ajust")

                     )

                 )
        ),

################    PAGINA DADES DEL TRACK ###############################

#--------------------------------------------------------------------------------



  tabPanel("Gestio del Track", "Comprobació del track, recorregut, hores inici i final i durada",
           sidebarLayout(

             sidebarPanel(
               fluidRow(
                 fileInput("myTrack", "Escull arxiu track", accept = c('.gpx'),multiple = FALSE ),

                 actionButton("Data_track_button","Mapejar Track")

               )
             ),

  #--------------Sortida gestio track--------------------------------------

             mainPanel(

               leafletOutput(outputId = 'map2'),

               htmlOutput(outputId =  "data_track"),

               htmlOutput( outputId ="temps_inicial_track"),

               htmlOutput( outputId ="temps_final_track"),

               htmlOutput( outputId ="durada_track"),

               htmlOutput(outputId = "longitut_track")

             )

           )),

################    PAGINA GEOLOCALITZACIO ###############################

#--------------------------------------------------------------------------------
tabPanel("Geolocalitzacio camara", "Geolocalitzacio de la fotografia" ,


         sidebarLayout(

           sidebarPanel(
             fluidRow(
               "Carregar la fotografia  que es vol geolocalitzar",

               fileInput("myPic",label="", accept = c('image/png', 'image/jpeg'),multiple = FALSE),

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



           #----Sortida geolocalitzacio--------------------------------------------------


           mainPanel(

             div(id = "image-container2", style = "display:flexbox"),

           textOutput( outputId = "temps_fotogeoloc"),

             textOutput( outputId = "temps_fotogeoloc_corr"),


             textOutput( outputId = "text5"),

             tableOutput('coordsTable1'),

            leafletOutput(outputId = 'map')
           )
         )),


################    PAGINA DF TOTAL ###############################

#--------------------------------------------------------------------------------



tabPanel("Resum de les observacions", "Represntacio de totes les observacions ",
         sidebarLayout(

           sidebarPanel(
             fluidRow(

               actionButton("afegir_button","Mapejar Track")





             )
           ),

           #--------------Sortida gestio track--------------------------------------

           mainPanel(

             leafletOutput(outputId = 'map3'),

             textOutput( outputId = "text10"),

             tableOutput('coordsTable2')

           )

         ))


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

  dades_foto_ref <- reactive ( { if (is.null(input$myrefcam$datapath))

                                return()

                                exifr::read_exif( pic_ref(), tags = 'DateTimeOriginal')} )

  #Del temps extret de l Exif-----------------------------------------------------

  time_foto_ref <- reactive (  { if (is.null(input$myrefcam$datapath))

                                return()


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


#############################################################################

  #Variables reactives geolocalitzacio de la foto

#############################################################################
  #Per obtindre el path de la foto a georefernciar-------------------------------

  pic_geo <- reactive({

    if (is.null(input$myPic$datapath))

      return()


    as.character( input$myPic$datapath)
  })



  dades_foto_geo <- reactive ( { if (is.null(input$myPic$datapath))

    return()

    exifr::read_exif( pic_geo(), tags = 'DateTimeOriginal')} )

  #Del temps extret de l Exif-----------------------------------------------------

  time_foto_geo <- reactive (  {

    if (is.null(input$myPic$datapath))

    {return()}

   force_tz( as_datetime( ymd_hms(dades_foto_geo()[,2])), tzone= "Europe/Madrid" ) })

  #Temps corretgit sobre el que apliquem la funcio

  time_geoloc_corr <-reactive(as_datetime(time_foto_geo()+t_syncro()))

  ###########################Funcio de minims#####################################

  coordenades <-reactive ({

    if (is.null(track_df())){
      return()
    }

   posicio_foto(time_geoloc_corr(),track_df())})


  posicio <- reactive({

    if (is.null(coordenades())){
      data.frame(lng =1.5,lat=35,time=1)
    }

    data.frame(lng =coordenades()$lng,lat = coordenades()$lat,time=coordenades()$time)



  })

  coords_rv <- reactiveValues(datos =isolate(posicio()))



#/////////////////////////////////////////////////////////////////////////////
 #-----------------------------------------------------------------------------
#-----------------------------Gestio sincronitzacio-----------------------------
#///////////////////////////////////////////////////////////////////////////

    observeEvent(input$myrefcam, {

  #Esborrarem totes les fotos que hi han a WWW-----------------------------

      directori_imatges <- ".\\WWW"

      llistat_imatges <-reactive( list.files(path= directori_imatges,pattern = "\\.(JPG|JPEG|PNG)$", full.names = TRUE))

      if (is.null(llistat_imatges()))

        return()

      file.remove(llistat_imatges())

#-----------------------------------------------------------------------------------

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



        if (ncol(dades_foto_ref())<2 ){
          showNotification("La foto no te metadades")
          removeModal()
          return()
        }

        output$exif_ref<-renderUI (HTML(paste0("Les metadades de la imatge de referencia son:", "<br/>",

                                          "- Dia: ",

                                          as.character(date(as_datetime(time_foto_ref ()))), "<br/>",

                                          "- Hora:  ",

                                          to_hour(time_foto_ref ())


                                                                                         ) ))


    })

    observeEvent(input$sincro, {


      if (is.null (pic_ref()) ){
        showNotification("Es necesari carregar foto de referencia")
        removeModal()
        return()
      }

      if (ncol(dades_foto_ref())<2 ){
        showNotification("La foto no te metadades")
        removeModal()
        return()
      }

       output$syncro <-renderUI (HTML(paste0("<br/>","El temps a sincronitzar en segons es de: ",

                                   isolate( as.character(t_syncro())),"segons","<br/>" ) ))

       output$hora_ref_ajust <-renderUI(HTML(paste0("<br/>","El nou temps sincronitzat ajustat és:","<br/>",

                                                    "- Dia: ",

                                     isolate( as.character(date(as_datetime(time_foto_ref()+t_syncro())))),"<br/>",

                                                   "- Hora: ",
                                                    isolate(to_hour(time_foto_ref()+t_syncro())))


                                      ))

  })



    #/////////////////////////////////////////////////////////////////////////////
    #-----------------------------------------------------------------------------
    #-----------------------------Gestio del track------------------------------
    #///////////////////////////////////////////////////////////////////////////

    observeEvent(input$Data_track_button,{

      if (is.null (trck_path()) ){
        showNotification("No hi ha cap track carregat")
        removeModal()
        return()
      }



      output$map2 <- (renderLeaflet({

        leaflet() %>%
          addPolylines(data = shape_track (),color = "red",weight = 2) %>%
          addProviderTiles(providers$Esri.WorldStreetMap,group = "WSMWorld Street Map") %>%
          addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
          addLayersControl(baseGroups = c("Satel.lit","World Street Map" ))

      }))

      output$data_track <- renderUI (HTML(paste ("<br/>","El track és va efectuar el dia :",isolate( as.character(date(as_datetime(Temps_Inicial_Track()) )) ),"<br/>")))

      output$temps_inicial_track <-renderUI ( HTML(paste0 ("- La hora inicial del track és: ",


                                                         isolate(to_hour(Temps_Inicial_Track()))) ))



      output$temps_final_track <-renderUI (HTML(paste0( "- La hora final del track és: ",

                                                       isolate( to_hour( Temps_Final_Track() ) ) ) ))


      output$durada_track <-renderUI(HTML( paste("La durada del track és:",  isolate(as.character(Durada_Track())), "min")))

      output$longitut_track <-renderUI(HTML( paste("El recorregut del track son:", isolate(as.character( Longitud_track())), "m")))


    })


    #/////////////////////////////////////////////////////////////////////////////
    #-----------------------------------------------------------------------------
    #-----------------------------Gestio geolocalitazio foto indiv-----------------------------
    #///////////////////////////////////////////////////////////////////////////

    #Inicialitzacio contador inicial de les observacions ID

    values <-reactiveValues(count=0)


#Carrega de la foto a geolocalitzar=======================================================

    observeEvent(input$myPic, {

      if (is.null(input$myPic$datapath))

        return()

      removeUI(selector = "#image-container2> * ")

      b64 <- base64enc::dataURI(file = pic_geo(), mime = "image/png")

      insertUI(
        selector = "#image-container2",
        where = "afterBegin",
        ui = img(src = b64, width = 250, height = 250)
      )


      if (ncol(dades_foto_geo())<2 ){
        showNotification("La foto no te metadades")
        removeModal()
        return()
      }

    output$temps_fotogeoloc <-renderText(paste("La hora de les metadades es :", to_hour (time_foto_geo()) ) )

    output$temps_fotogeoloc_corr <-renderText(paste("La hora ajustada es :",

                     to_hour (time_geoloc_corr())))

    })

    #Geolocalitzacio de la foto carregada=======================================================

    observeEvent(input$geoloc_button,{

     if (is.null (trck_path()) ){
        showNotification("No hi ha cap track carregat")
        removeModal()
        return()
     }

      #-----condicio foto a georeferneciar carregada------------------------------------------

           if (is.null (pic_geo()) ){
        showNotification("Es necesari carregar foto a georeferenciar")
        removeModal()
        return()
      }

      #-----condicio foto a georeferenciar amb metadades------------------------------------

      if (ncol(dades_foto_geo())<2 ){
        showNotification("La foto no te metadades")
        removeModal()
        return()
      }


      #-----Cal aplicar la condicio que el temps de la foto estigui dins l interval de temps

      if (!(time_geoloc_corr()  %within% Interval_track())){
        showNotification("Temps de la foto no esta dintre l interval de temps del track")
       removeModal()
       return()
      }




      isolate(posicio())

     output$map <- renderLeaflet({

       leaflet() %>%
         addPolylines(data = shape_track (),color = "red",weight = 2) %>%
         addProviderTiles(providers$Esri.WorldStreetMap,group = "WSMWorld Street Map") %>%
         addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
         addLayersControl(baseGroups = c("Satel.lit","World Street Map"))%>%
         addMarkers(lng= isolate( posicio()$lng) , lat= isolate( posicio()$lat),

          options = markerOptions(draggable = TRUE),
         popup =  paste ( "Longitud:",  as.character(isolate(posicio()[1,1])),
                        "<br>",
                        "Latitud:",as.character(isolate(posicio()[1,2] ))))

      })

     output$map3 <- NULL

    })

    #Index per seleccionar el df de sortida en funcio si s arrosego o no la xinxeta

    i <- reactiveValues(datos =0)   #si no s arrosega la xinxeta


    #Captacio de la posicio amb lobservacio de l event drag&drop

    observeEvent(input$map_marker_dragend, {

      i$datos=1  # S arrossega la xinxeta

      lat <- input$map_marker_dragend$lat

      lng <- input$map_marker_dragend$lng



      coords_rv$datos[1,1] <- lng

      coords_rv$datos[1,2] <- lat

      coords_rv$datos[1,3]<- posicio()[1,3]


#--------Modifiquem les chinchetes del planol al arrosegarles



      leafletProxy('map') %>% clearMarkers() %>%

        addMarkers(data= coords_rv$datos,

                   lng = coords_rv$datos[1,1] , lat = coords_rv$datos[1,2] ,


                   options = markerOptions(draggable = TRUE),

                   popup =  paste ( "Longitud:",as.character(coords_rv$datos[1,1]),
                                    "<br>",
                                    "Latitud:",as.character(coords_rv$datos[1,2] )))


}) # Tancament del draggent


#----------------------Per fixar la posicio individual--------------------------------------

  #definim dataframe de l observacio individual

    df_indiv<-reactiveValues(dades= c(
                                       "id"=integer(),
                                       "Longitut"=numeric() ,
                                       "Latitut"= numeric(),
                                       "Temps"=character(),
                                       "img"=character()))

    #definim dataframe de l observacions totals


    taula_total <- reactiveValues(dades= data.frame(
                                            "id"=integer(),
                                            "Longitut"=numeric(),
                                            "Latitut"=numeric(),
                                            "Temps"= character(),
                                            "img"= character()))

    foto_list <- reactiveValues (dades = list( character()))


  #DEpen si hem arrosegat o no fixem una posicio o l altre que carraguem al dataframe individul

  # Per la gestio de l imatge a la memoria cache.Cal obtindre el directori

    nom <-reactive(input$myPic$name)
    root <-reactive(pic_geo())
    nch <-reactive(nchar(root()))

    # reactive (substr(root(),nch()-4 ,nch())) #El nom de l axiu dins l app

    #Obtenim el directori per fer el canvi de nom d arixiu d ela foto

    dir <-reactive (str_sub(root() , 1, nchar(root())-5))



    #Fixar el dataframe definitiu=================================================

    observeEvent(input$fix_button,{


      values$count <- values$count + 1

      new_root<- reactive(paste0(dir(),values$count,".JPG"))

      file.rename(root(),  new_root() )

      root_WWW <- reactive(paste0(".\\WWW\\",values$count,".JPG"))

      file.rename(root(),  root_WWW() )


        if (i$datos==0) {

         df_indiv$dades$id <- values$count
         df_indiv$dades$Longitut <- posicio()$lng
         df_indiv$dades$Latitut <- posicio()$lat
         df_indiv$dades$Temps <- to_hour(posicio()$time)
         df_indiv$dades$img <-root_WWW()
          }

        else if (i$datos==1){

          df_indiv$dades$id <- values$count
          df_indiv$dades$Longitut <- coords_rv$datos[1,1]
          df_indiv$dades$Latitut <- coords_rv$datos[1,2]
          df_indiv$dades$Temps <- to_hour(coords_rv$datos[1,3])
          df_indiv$dades$img <-root_WWW()
          }

      df_indiv$dades <- as.data.frame(df_indiv$dades)

      output$text5 <-renderText(paste("Les dades i posicio de la fotografia son:", root_WWW()))

      output$coordsTable1<- renderTable(df_indiv$dades,align ="c" ,digits = 6)

    #Escritura de les metadades de geolocalitzacio en la fotografia un cop fixada posicio

    Lat.arg = paste0("-GPSLatitude=",as.character(df_indiv$dades$Latitut))
    Long.arg = paste0("-GPSLongitude=",as.character(df_indiv$dades$Longitut))
    exifr::exiftool_call(args = Lat.arg, fnames = new_root() )
    exifr::exiftool_call(args =  Long.arg, fnames = new_root())

    imatge <- reactive(image_convert( magick::image_read(new_root()),"jpg"))

    #Reescalem la imatge i la guardem a WWW


    imatge_reesc<- reactive( image_scale (image = imatge(), "50"))

    image_write (image =imatge_reesc(), path =  root_WWW())




    })
##--------------------dEscarrega fotografia geolocalitzada-------------------


    #Convertim l arxiu a ImagikPick per poder-la descarregar

    output$descarrega <- downloadHandler(
      filename = "descarrega.jpg",
      content = function(file) {
        magick::image_write(imatge(), file)}
    )


    #/////////////////////////////////////////////////////////////////////////////
    #-----------------------------------------------------------------------------
    #-----------------------------Gestio total observacions-----------------------------
    #//////////////////////////////////////////////////////////////////////////


    observeEvent(input$afegir_button,{

      #------------------------afegim les dades al data frame total--------------

   #   v<-values$count

    #  for (i in v) {
     #   taula_total$dades[i,5] <-str_replace_all(paste0(dir(),v,".JPG"),"'\'",'/')
    #  }

      directori_imatges <- ".\\WWW"

      llistat_fotos <- reactive(list.files(path= directori_imatges,pattern = "\\.(JPG|JPEG|PNG)$", full.names = TRUE))

     if (is.null(llistat_fotos()))

        return()

    taula_total$dades <-rbind(taula_total$dades,df_indiv$dades)

      #foto_list$dades <- taula_total$dades[,5]

      #append(foto_list$dades,new_root())




#Esborrem les dades de la geolocalitazcio individual------------------------

     # output$coordsTable1<- renderTable(NULL)

#Esborrem el planol individual

     # output$map <- renderLeaflet(NULL)
      #df_indiv$dades<-NULL
      #output$temps_fotogeoloc <-NULL
      #output$temps_fotogeoloc_corr <-NULL

  #--------------------------------------------------------------------------


    output$text10 <-renderText("Les dades i posicio de la fotografia son:")

    output$map3 <- renderLeaflet({

                    leaflet()%>%

                      addPolylines(data = shape_track (),color = "red",weight = 2) %>%
                      addProviderTiles(providers$Esri.WorldStreetMap,group = "WSMWorld Street Map") %>%
                      addProviderTiles(providers$Esri.WorldImagery,group = "Satel.lit") %>%
                      addWMSTiles(
                        "https://geoserveis.icgc.cat/servei/catalunya/quadricules-utm/wms?",
                      layers = "quadricula_utm_1km",
                      group ="1x1",
                      options = WMSTileOptions(format = "image/png",
                                               transparent = TRUE,
                                               #opacity = 0.5,
                                               crs="EPSG:4326"),
                      attribution = " Institut Cartogràfic i Geològic de Catalunya"
                    )%>%
                      addWMSTiles(
                        "https://geoserveis.icgc.cat/servei/catalunya/quadricules-utm/wms?",
                        layers = "quadricula_utm_10km",
                        group ="10x10",
                        options = WMSTileOptions(format = "image/png",
                                                 transparent = TRUE,
                                                 #opacity = 0.5,
                                                 crs="EPSG:4326"),
                        attribution = " Institut Cartogràfic i Geològic de Catalunya"
                      )%>%


                    addWMSTiles(
                      "https://geoserveis.icgc.cat/servei/catalunya/linia-costa/wms?",
                      layers = "isobates_clar_2500000",
                      group ="batimetria",
                      options = WMSTileOptions(format = "image/png",
                                               transparent = TRUE,
                                               #opacity = 0.5,
                                               crs="EPSG:25831"),
                      attribution = " Institut Cartogràfic i Geològic de Catalunya"
                    )%>%



                      addLayersControl(baseGroups = c("World Street Map", "Satel.lit"),
                                       overlayGroups = c("batimetria","1x1","10x10"),
                                       options = layersControlOptions(collapsed = TRUE))%>%
                      addMarkers(data = taula_total$dades,
                                  group = "fotos",
                                  lng = taula_total$dades[,2],
                                  lat = taula_total$dades[,3])%>%
                                  #options = markerOptions(draggable = FALSE)) %>%
                                  #popup = ( paste0('<img src="base64enc::dataURI(file =', taula_total$dades[,5], 'mime = "image/png")','" height="','200"></img>') ) )

                     addPopupImages( taula_total$dades[,5], group = "fotos", width = 100)

                  })


    output$coordsTable2<- renderTable({  taula_total$dades

      #arrange(df_total$dades,df_total$dades$id)
    },align ="c" ,digits = 6)


})
#--------------------------------------------------------
}

shinyApp(ui = ui, server = server)
