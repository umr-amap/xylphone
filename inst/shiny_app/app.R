
# library -----------------------------------------------------------------

library(BIOMASS)
library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(shiny)
library(stats)
library(stringr)
library(DT)
library(sf)
library(mapview)
library(leaflet)
library(leafem)
library(writexl)
library(htmlwidgets)
library(htmltools)
library(grDevices)
library(proj4)

col = xyIphone::col
all_sbplot = xyIphone::all_sbplot


# ui ----------------------------------------------------------------------


ui <- fluidPage(
   titlePanel("Pipeline LIDAR → Coordonnées d’arbres"),

   sidebarLayout(
      sidebarPanel(

         verbatimTextOutput("logs"),
         hr(),
         textInput("root_in", "Répertoire des scans", value = "D:/VIA/scan_IPhone/mbalmayo004"),
         textInput("directory", "Répertoire de sortie", value = "D:/VIA/scan_IPhone/outputs"),
         fluidRow(
            column(6, textInput("plot_name", "Nom de la parcelle", value = "mbalmayo004")),
            column(6, textInput("crs", "CRS (ex. 'EPSG:32632')", value = "EPSG:32632"))
         ),
         fileInput("inventory", "Importer les données d'inventaire (.rds ou .RData)", accept = c(".rds", ".RData")),
         hr(),
         actionButton("btn_files", "1 – Vérifier noms de fichiers"),br(),
         actionButton("btn_expRawdata", "2 – Exporter les données brutes"),br(),
         actionButton("btn_extract", "2 – Extraire jalons"),br(),
         actionButton("btn_xy", "3 – Calculs XY"),br(),
         actionButton("btn_report", "5 – Rapport scans"),br(),
         actionButton("btn_export", "7 – Exporter GPkg & Excel")
      ),

      mainPanel(
         tabsetPanel(id = "tabs",
                     tabPanel("Résumé fichiers",  hr(),
                              textOutput("check_file_name"), hr(),
                              textOutput("missing_scan"), hr(),
                              DT::dataTableOutput("tbl_files")),

                     tabPanel('Fichiers bruts', hr(),

                              "Vérifier pour chaque quadrat l'endodage des données. Les arbres qui apparaissent en rouge ne sont pas enregistrés dans ce quadrat dans les données d'inventaire, il peut s'agir de recrus ou d'erreurs d'encodage. La correction des erreurs peut se faire directement dans le tableau ci-dessous. NE PAS OUBLIER DE CLIQUER SUR 'ENREGISTRER LES MODIFICATIONS'",
                              hr(),

                              fluidRow(
                                 column(4, offset = 4,selectInput("subplot_file", "Choix du fichier",choices = NULL, selected = NULL))),
                              br(),
                              plotOutput('rawplot', height = 500),

                              fluidRow(
                                 column(4,actionButton("prev_image", "← Image précédente"), offset = 3),
                                 column(4,actionButton("next_image", "Image suivante →"))
                              ),

                              actionButton("clean_out", "In"),

                              actionButton("save", "Enregistrer les modifications"),


                              DTOutput("table")),

                     tabPanel("Carte jalons",hr(),

                              fluidRow(
                                 column(4,
                                        actionButton("btn_type1", "Type 1")
                                 ),
                                 column(4,
                                        actionButton("btn_type2", "Type 2")
                                 ),
                              ),

                              hr(),

                              textOutput("check_jalon"),

                              hr(),

                              fluidRow(
                                 column(6,
                                        "Données brutes",
                                        plotOutput("map_jalons_brut", height = 300)
                                 ),
                                 column(6,
                                        "Données extraites",
                                        plotOutput("map_jalons_all", height = 300)
                                 )
                              ),

                     ),
                     tabPanel('XY reprojetés', hr(),
                              DT::dataTableOutput("table1"),
                              DT::dataTableOutput("table2"),

                              "✅  Les IDs dupliqués proches les uns des autres",  br(),
                              "✅  Les arbres OUT qui sont des recrus se trouvant effectivement dans le bon quadrat",  br(),
                              "✅  Les arbres ADJ qui sont proches du layon les séparant de leur quadrat",  br(),
                              "❌  Les arbres OUT qui ne sont pas des recrus" , br(),
                              "❌  IDs dupliqués trop éloignés", br(),
                              "❌   Les arbres ADJ loin de leur layon les séparant  de leur quadrat", br(),
                              hr(),
                              "Réaliser les modificattions nécessaires dans l'onget 'Fichiers bruts'",

                              leafletOutput("leaflet", height = 600), # Pour debug (optionnel))
                              hr(),
                              DT::dataTableOutput("tbl_alltrees")),

                     tabPanel("Rapport texte",        verbatimTextOutput("txt_report")),
                     tabPanel("Distance reprojection", plotOutput("hist_dist", height = 300))
         )
      )
   )
)


# server ------------------------------------------------------------------


server <- function(input, output, session) {


   # stocker les données -----------------------------------------------------

   inventory <- reactiveVal()
   data <- reactiveVal()
   file_path <- reactiveVal()
   files_sum =  reactiveVal()
   subplot <- reactiveVal()
   all_xy <- reactiveVal()
   all_trees <- reactiveVal()
   all_report <- reactiveVal()
   rawdataPath <- reactiveVal()
   rawfiles_choices <- reactiveVal()
   export <- reactiveVal()


   # Importer les données d'inventaire ---------------------------------------

   observeEvent(input$inventory, {
      req(input$inventory)
      ext <- tools::file_ext(input$inventory$datapath)
      if(ext == "rds") {
         inventory(readRDS(input$inventory$datapath))
      } else {
         # si .RData contient un objet Fieldplot_BDD_full
         e <- new.env()
         load(input$inventory$datapath, envir = e)
         inventory(e$Fieldplot_BDD_full)
      }
      showNotification("Inventaire chargé", type="message")

   })


   # 1 – files_summary -------------------------------------------------------


   observeEvent(input$btn_files, {

      dir.create(paste0(input$directory,'/',input$plot_name))

      output$check_file_name <- renderText({paste('Dans le tableau ci-dessous, pour chaque fichier regarder si les différentes valeurs peuvent être proprement extraites. Le cas échéant, modifier le nom des fichier ici',"'",input$root_in,"'")})
      req(inventory())

      fs <- files_summary(
         root_in   = input$root_in,
         export    = FALSE,
         directory = input$directory,
         plot_name = input$plot_name
      )

      files_sum(fs)

      lidar_subplot = basename(list.files(input$root_in)) %>%
         str_split(.,'_', simplify = T) %>%
         .[,c(3,4)] %>%
         as.data.frame() %>%
         mutate(V2 = str_remove(V2, '.csv'))

      check_subplot = all_sbplot[!all_sbplot %in% paste(lidar_subplot$V1,lidar_subplot$V2, sep = '_')]

      output$missing_scan <- renderText({
         paste(
            if(length(check_subplot) == 0) "✅ Tous les  quadrats ont été scannés au moins une fois" else  paste("❌ Il manque le scan du (des) quadrat(s)",paste(check_subplot, collapse = ', ')) )
      })



      output$tbl_files <- DT::renderDataTable({
         req(files_sum())
         DT::datatable(files_sum(),
                       options = list(pageLength = 50))
      })


      showNotification(paste('A report has been saved : ', file.path(input$directory,input$plot_name,"1_check_files_basenames.txt") ), type="message")

   })


   # 2 - Exporte raw data ----------------------------------------------------

   observeEvent(input$btn_expRawdata,{

      dir.create(paste0(input$directory,'/',input$plot_name,'/1_rawData'))
      rawdataPath(dir(input$root_in, full.names = T))

      for (i in 1:length(rawdataPath())){

         tmp <- readLines(rawdataPath()[i]) %>% stringr::str_replace_all(',',';')
         sousplot = basename(files_sum()$subplot)[i]
         data_type <- if_else('Notes2' %in% tmp[1], 'type2','type1')

         if( data_type == 'type2'){

            tmp <- stringr::str_split(tmp,';', simplify = T) %>%
               dplyr::as_tibble() %>%
               dplyr::slice(-1) %>%
               dplyr::rename(id = V9,
                             X_lidar = V3,
                             Y_lidar = V2) %>%
               dplyr::select(id, X_lidar, Y_lidar) %>%
               dplyr::mutate(X_lidar = as.numeric(X_lidar),
                             Y_lidar = as.numeric(Y_lidar)) %>%
               dplyr::filter(id != '')
         } else {

            tmp <- stringr::str_split(tmp,';', simplify = T) %>%
               dplyr::as_tibble() %>%
               dplyr::slice(-1) %>%
               dplyr::rename(id = V8,
                             X_lidar = V3,
                             Y_lidar = V2) %>%
               dplyr::select(id, X_lidar, Y_lidar) %>%
               dplyr::mutate(X_lidar = as.numeric(X_lidar),
                             Y_lidar = as.numeric(Y_lidar)) %>%
               dplyr::filter(id != '')
         }


         # Extract jalons and subplots informations --------------------------------

         x_min <- as.numeric(str_split(sousplot, '_') [[1]] [1])
         x_max <- as.numeric(str_split(sousplot, '_') [[1]] [1]) + 20
         y_min <- as.numeric(str_split(sousplot, '_') [[1]] [2])
         y_max <- as.numeric(str_split(sousplot, '_') [[1]] [2]) + 20


         jalon_theo <-
            expand.grid(seq(x_min, x_max, 20), seq(y_min, y_max, 20)) %>%
            mutate(jalon = paste(Var1, Var2, sep = '_')) %>%
            .[['jalon']]


         adjacent_sousplot <-
            expand.grid(seq(x_min - 20, x_max, 20), seq(y_min - 20, y_max, 20)) %>%
            filter(Var1 >= 0 &
                      Var1 < 100 & Var2 >= 0 & Var2 < 100) %>%
            mutate(subplot = paste(Var1, Var2, sep = '_')) %>%
            filter(subplot != sousplot) %>% .[['subplot']]

         # Extract data for the subplot --------------------------------------------

         trees_inventory <-
            inventory()$extract %>%
            dplyr::filter(sous_plot_name %in% sousplot)


         id_sousplot <-
            trees_inventory %>%
            dplyr::filter (sous_plot_name == sousplot) %>%
            .[['ind_num_sous_plot']]

         id_adj_sousplot <-
            trees_inventory %>%
            dplyr::filter (sous_plot_name %in% adjacent_sousplot) %>%
            .[['ind_num_sous_plot']]
         dup.id <-
            tmp %>%
            dplyr::filter(duplicated(id)) %>% .[['id']]


         id = tmp %>% .[['id']]

         inv.id = inventory()$extract %>%
            filter(ind_num_sous_plot %in% id) %>%
            select(ind_num_sous_plot, sous_plot_name) %>%
            rename(id = ind_num_sous_plot, where = sous_plot_name) %>%
            mutate(id = as.character(id))

         tmp = tmp %>%
            mutate(
               what = case_when(str_detect(id, 'jalon_') ~ 'jalon', TRUE ~ 'tree'),
               id = str_remove(id, 'jalon_'),
               duplicated_id = dplyr::case_when(id %in% dup.id ~ 'yes', TRUE ~ 'no'),
               where = dplyr::case_when(
                  (what == 'tree' & id %in% id_sousplot) ~ 'in',
                  (what == 'tree' & id %in% id_adj_sousplot) ~ 'adjacent',
                  TRUE ~ 'out'
               ),

               where = dplyr::case_when(
                  (what == 'jalon' & stringr::str_detect(id, paste(jalon_theo, collapse = '|'))) ~ 'in',
                  TRUE ~ where
               ),
            ) %>%
            select(what,id,everything())

         n_jal = tmp %>% filter(what == 'jalon') %>% .[['id']] %>% unique() %>% length()

         tmp$n_jal = n_jal


         write.csv(tmp, file = paste0(input$directory,'/',input$plot_name,'/1_rawData/',basename(rawdataPath()[i])), row.names = FALSE)
      }

      rawfiles_choices(files_sum() %>% arrange(match(subplot,all_sbplot)) %>% pull(path) %>% basename())

      updateSelectInput(session,
                        "subplot_file",
                        choices = rawfiles_choices(),
                        selected = rawfiles_choices()[1]
      )

   })

   # Lecture du fichier
   observeEvent(input$subplot_file, {

      req(rawfiles_choices())

      path <- paste0(input$directory,'/',input$plot_name,'/1_rawData/',input$subplot_file)
      tmp = read.csv(path)
      data(tmp)

      # Affichage de la table modifiable
      output$table <- renderDT({
         datatable(data(), editable = TRUE, options = list(pageLength = 50), rownames = FALSE)
      }, server = TRUE)


      output$rawplot  <- renderPlot({

         ggplot2::ggplot(data(), ggplot2::aes(x=X_lidar, y=Y_lidar)) +
            ggplot2::geom_label(ggplot2::aes(label = id, fill = what)) +
            ggplot2::geom_point(ggplot2::aes(y=Y_lidar+0.000008,col = where, shape = duplicated_id), size = 3) +
            ggplot2::scale_color_manual(values = c("in" = 'blue', 'out' = 'red', 'adjacent'= 'orange')) +
            ggplot2::theme_classic() +
            ggplot2::theme(
               panel.background = element_rect(fill = "grey")
            )
      })
   })


   # Exemple de liste d'images (à adapter à ton contexte)
   # image_list <- reactiveVal(c())  # Stocke les noms de fichiers

   # Image précédente
   observeEvent(input$prev_image, {
      current <- input$subplot_file
      index <- which( rawfiles_choices() == current)

      if (length(index) > 0 && index > 1) {
         updateSelectInput(inputId = "subplot_file", selected = rawfiles_choices()[index - 1], choices =  rawfiles_choices())
      }


   })

   # Image suivante
   observeEvent(input$next_image, {
      current <- input$subplot_file
      index <- which( rawfiles_choices() == current)
      if (length(index) > 0 && index < length(rawfiles_choices())) {
         updateSelectInput(inputId = "subplot_file", selected = rawfiles_choices()[index + 1], , choices =  rawfiles_choices())
      }

   })



   # Mise à jour des données après édition
   observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      str(info)
      df <- data()
      df[info$row, info$col+1] <- info$value
      data(df)
   })

   # Clean out
   observeEvent(input$clean_out, {
      req(data())
      tmp = data() %>% mutate(where = 'in')
      data(tmp)
      write.csv(data(), paste0(input$directory,'/',input$plot_name,'/1_rawData/',input$subplot_file), row.names = FALSE)
      output$save_status <- renderText("Modifications enregistrées avec succès.")
   })

   # Sauvegarde du fichier
   observeEvent(input$save, {
      req(data())
      write.csv(data(), paste0(input$directory,'/',input$plot_name,'/1_rawData/',input$subplot_file), row.names = FALSE)
      showNotification("Modifications enregistrées avec succès.")
   })


   # 3 – extract_jalons ------------------------------------------------------


   observeEvent(input$btn_extract, {
      req(input$inventory)

      output$check_jalon <- renderText({paste("S'assurer de l'emplacement des jalons dans les données extraites. Si il ne correspond pas aux données brutes, essayer un autre type")})

      subplot(extract_jalons(coordinates = inventory()$coordinates, type = 1))

      output$map_jalons_brut <- renderPlot({
         req(subplot())

         ggplot(inventory()$coordinates, aes(x=typevalue_ddlon,y=typevalue_ddlat, label = paste(coord1,coord2,sep='_'))) +
            geom_label(fill = 'grey') +
            theme_bw()
      })

      output$map_jalons_all <- renderPlot({
         req(subplot())
         subplot() %>% ggplot(.,aes(x=XAbs,y=YAbs, label = jalon))+ geom_point() +
            geom_label(fill = 'black', col = 'white') +
            theme_bw()
      })

      showNotification("Jalons extraits", type="message")
   })

   observeEvent(input$btn_type1, {
      req(input$inventory)

      subplot(extract_jalons(coordinates = inventory()$coordinates, type = 1))

      output$map_jalons <- renderPlot({
         req(subplot())
         subplot() %>% ggplot(.,aes(x=XAbs,y=YAbs, label = jalon))+ geom_point() +geom_label()
      })

      showNotification("Jalons extraits", type="message")
   })

   observeEvent(input$btn_type2, {
      req(input$inventory)

      subplot(extract_jalons(coordinates = inventory()$coordinates, type = 2))

      output$map_jalons <- renderPlot({
         req(subplot())
         subplot() %>% ggplot(.,aes(x=XAbs,y=YAbs, label = jalon))+ geom_point() +geom_label()
      })

      showNotification("Jalons extraits", type="message")
   })


   # 4 – XY computation ------------------------------------------------------


   observeEvent(input$btn_xy, {

      groups = 'ARBRES'

      req(inventory(), files_sum(), subplot())

      XY_computation = XY_computation(
         Fieldplot_BDD_full = inventory(),
         raw_files_path     = list.files(paste0(input$directory,'/',input$plot_name,'/1_rawData')),
         subplot            = subplot(),
         plot_name          = input$plot_name,
         files_Summary      = files_sum(),
         directory          = input$directory
      )

      all_xy(XY_computation)

      # MODIF

      output$table1 <- renderDT({
         req(all_xy())
         datatable(all_xy(), options = list(pageLength = 50), rownames = FALSE)
      }, server = TRUE)

      output$table2 <- renderDT({
         req(subplot())
         datatable(subplot(), options = list(pageLength = 50), rownames = FALSE)
      }, server = TRUE)

      # MODIF

      all_xy(reproj_20_20(all_xy(), subplot(),input$directory, input$plot_name))
      #
      # output$leaflet = renderLeaflet({
      #
      #    test = all_xy() %>% st_as_sf(coords = c("XAbs", "YAbs"), crs = input$crs, agr = "constant")
      #
      #
      #    sfc = st_transform(test, crs = "+proj=longlat +datum=WGS84")
      #
      #
      #    all_jalon = sfc %>% filter(what == 'jalon')
      #    all_trees = sfc %>% filter(what == 'tree')
      #    all_dupli = sfc %>% filter(duplicated_id == 'yes')
      #    all_out = sfc %>% filter(where == 'out')
      #    all_adj = sfc %>% filter(where == 'adjacent')
      #
      #
      #    options = providerTileOptions(minzoom = 0.1, maxzoom = 10)
      #
      #
      #    map = leaflet()  %>%
      #       addProviderTiles("CartoDB.Positron",
      #                        options = providerTileOptions(maxNativeZoom=10,maxZoom=100)) %>%
      #
      #       #  addControl(html="<h1 id='zoom'>Zoom</h1>") %>%
      #       #  htmlwidgets::onRender("function(el,x,data){
      #       # var map=this;
      #       # var evt = function(e){
      #       #   $('#zoom').html(map.getZoom())
      #       # };
      #       # map.on('zoom', evt);
      #       # }") %>%
      #       addCircleMarkers(data = all_jalon, color = 'black', group = "JALONS",
      #                        popup = ~htmlEscape(paste(file,id,sep = ' : '))) %>%
      #       addCircleMarkers(data = all_trees,
      #                        color = 'green',
      #                        label = ~id,
      #                        group = "ARBRES",
      #                        labelOptions = labelOptions(noHide = TRUE,
      #                                                    textsize = "15px",
      #                                                    fill = FALSE,
      #                                                    textOnly = T),
      #                        popup = ~htmlEscape(paste(file,id,sep = ' : ')))  %>%
      #       addLayersControl(
      #          overlayGroups = groups,
      #          options = layersControlOptions(collapsed = FALSE)
      #       )
      #
      #    if(nrow(all_adj) > 0){
      #
      #       groups = c(groups, "ARBRES ADJ")
      #
      #       map = map %>%
      #          addCircleMarkers(data = all_adj,
      #                           color = 'orange',
      #                           label = ~id,
      #                           group = "ARBRES ADJ",
      #                           labelOptions = labelOptions(noHide = TRUE,
      #                                                       textsize = "15px",
      #                                                       fill = FALSE,
      #                                                       textOnly = T),
      #                           popup = ~htmlEscape(paste(file,id,sep = ' : '))) %>%
      #          addLayersControl(
      #             overlayGroups = groups,
      #             options = layersControlOptions(collapsed = FALSE)
      #          )
      #    }
      #
      #    if(nrow(all_dupli) > 0){
      #
      #       groups = c(groups, "ID DUPLIQUES")
      #
      #       map = map %>%
      #          addCircleMarkers(data = all_dupli,
      #                           color = 'red',
      #                           label = ~id,
      #                           group = "ID DUPLIQUES",
      #                           labelOptions = labelOptions(noHide = TRUE,
      #                                                       textsize = "15px",
      #                                                       fill = FALSE,
      #                                                       textOnly = T),
      #                           popup = ~htmlEscape(paste(file,id,sep = ' : '))) %>%
      #          addLayersControl(
      #             overlayGroups = groups,
      #             options = layersControlOptions(collapsed = FALSE)
      #          )
      #    }
      #
      #    if(nrow(all_out) > 0){
      #
      #       groups = c(groups, "ARBRES OUT")
      #
      #       map = map %>%
      #          addCircleMarkers(data = all_out,
      #                           color = 'blue',
      #                           label = ~id,
      #                           group = "ARBRES OUT",
      #                           labelOptions = labelOptions(noHide = TRUE,
      #                                                       textsize = "15px",
      #                                                       fill = FALSE,
      #                                                       textOnly = T),
      #                           popup = ~htmlEscape(paste(file,id,sep = ' : '))) %>%
      #          addLayersControl(
      #             overlayGroups = groups,
      #             options = layersControlOptions(collapsed = FALSE)
      #          )
      #    }
      #
      #    map
      #
      # }
      #
      # )
      #
      #
      # showNotification("XY calculés", type="message")
      #
      #
      # output$tbl_alltrees <- renderDT({
      #    req(all_xy())
      #    DT::datatable(all_xy() %>% select(file,id,where,TRUE_X_20,TRUE_Y_20,quadrat_mesured,quadrat_check)
      #                  , editable = FALSE, options = list(pageLength = 10), rownames = FALSE)
      # }, server = TRUE)
      #
      # all_trees = plot_alltrees(
      #    Fieldplot_BDD_full = inventory(),
      #    all    = all_xy(),
      #    subplot = subplot(),
      #    crs     = input$crs,
      #    col     = data(col),  # ou vecteur de couleurs
      #    plot_name = input$plot_name,
      #    directory = input$directory
      # )
      #
      # all_trees(all_trees)

   })


   # 5 – all_scans_report

   observeEvent(input$btn_report,{

      req(inventory(), all_xy())

      all_scans_report(inventory(), all_xy(), input$directory, input$plot_name)

      output$txt_report <- renderText({
         req(file.exists(paste0(input$directory, '/', input$plot_name,"/5_ALL_PLOT_REPORT.txt")))
         all_report(paste0(input$directory, '/', input$plot_name,"/5_ALL_PLOT_REPORT.txt"))
         paste(readLines(all_report()), collapse = "\n")    })

   })


   # 6 – reprojection 20×20

   # output$hist_dist <- renderPlot({
   #   req(reproj20())
   #   # histogramme déjà sauvegardé, on peut également le recalculer ici
   #   hist(
   #     reproj20()$distance,
   #     breaks = seq(0, max(reproj20()$distance), by=1),
   #     xlab = "Distance (m)", main = "Répartition des distances"
   #   )
   # })

   # 7 – export final
   observeEvent(input$btn_export, {
      req(all_xy(), subplot())

      export(1)

      save_gpkg(all_xy(), input$crs, subplot(), input$directory, plot_name = input$plot_name)

      all = all_xy() %>%
         dplyr::filter(what == 'tree') %>%
         filter(grepl("^[0-9]+(\\.[0-9]+)?$", id)) %>%
         dplyr::mutate(id = as.numeric(id)) %>%
         dplyr::group_by(id) %>%
         arrange(match(method, c("jalon", "trees")), .by_group = TRUE, dplyr::across(dplyr::starts_with("trees"))) %>%
         slice(1) %>%
         ungroup() %>%
         group_by(id) %>%
         slice(1)

      writexl::write_xlsx(all,
                          path = file.path(input$directory, input$plot_name, paste0("FINAL_20_20_PROJECTION",input$plot_name,".xlsx"))
      )

      showNotification("Export GPkg et Excel terminé", type="message")
   })

   # logs
   output$logs <- renderText({
      msg <- paste0(
         "Étapes validées :\n",
         "- Préciser le Répertoire des scans : ",     if(!dir.exists(input$root_in)) "❌" else "✅", "\n",
         "- Préciser le dossier de sortie : ",     if(!dir.exists(input$directory)) "❌" else "✅", "\n",
         "- Plot name : ",     if(is.null(input$plot_name) | input$plot_name == '') "❌" else "✅", "\n",
         "- CRS : ",     if(is.null(input$crs) | input$crs == '') "❌" else "✅", "\n",
         "- Importer données inventaire : ",     if(is.null(inventory())) "❌" else "✅", "\n",
         "- Vérifier noms de fichiers : ",       if(is.null(files_sum()))    "❌" else "✅", "\n",
         "- Exporter données brutes : ",       if(is.null(rawdataPath()))    "❌" else "✅", "\n",
         "- Extraire jalons : ",         if(is.null(subplot()))      "❌" else "✅", "\n",
         "- Calculs XY : ",             if(is.null(all_xy()))       "❌" else "✅", "\n",
         "- Rapport scans : ",             if(is.null(all_report()))       "❌" else "✅", "\n",
         "- Exporter GPkg & Excel : ",             if(is.null(export()))       "❌" else "✅", "\n"
      )
      msg
   })
}

shinyApp(ui, server)

