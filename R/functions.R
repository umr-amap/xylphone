#' all_scans_report
#' @export
#' @import dplyr
all_scans_report <- function (Fieldplot_BDD_full, all, directory, plot_name) {
   sink(file = paste0(directory, '/', plot_name,"/5_ALL_PLOT_REPORT.txt"))

   all_missing_id <- unique(Fieldplot_BDD_full$extract$ind_num_sous_plot)[!(unique(Fieldplot_BDD_full$extract$ind_num_sous_plot) %in% unique(all$id))] %>% as.numeric %>% sort()
   aa <- all %>% dplyr::filter(where == 'in') %>% .[['id']] %>% unique()
   all_missing_id_in <- unique(Fieldplot_BDD_full$extract$ind_num_sous_plot)[!(unique(Fieldplot_BDD_full$extract$ind_num_sous_plot) %in% aa)] %>% as.numeric %>% sort()


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                       MISSING ID IN THE SCAN                       '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))


   print(paste('The following id are not present on the scans :', paste(all_missing_id_in, collapse = ', ')))


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('    MISSING ID IN THE SCAN AND THE SUBPLOT TO BE (RE)SCANNED        '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))


   print(Fieldplot_BDD_full$extract %>% dplyr::filter(ind_num_sous_plot %in% all_missing_id_in) %>% dplyr::group_by(sous_plot_name) %>%  dplyr::summarise(id = paste(ind_num_sous_plot, collapse = ', ')))


   print(paste('                                                                    '))
   print(paste('        MISSING ID CAN BE DUE TO THE FOLLOWING MISTAKE              '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   adjacent_error <- all %>% dplyr::filter(where == 'adjacent')
   out_error <- all %>% dplyr::filter(where == 'out')

   if(nrow(adjacent_error) > 0) {

      for (i in 1:nrow(adjacent_error)) {

         print(paste('In the file :', adjacent_error$file[i], ', the id', adjacent_error$id[i], ' is present but it is suppose to be on the adjacent subplot', adjacent_error$sous_plot_name[i]))

      }
   }

   if(nrow(out_error) > 0) {

      for (i in 1:nrow(out_error)) {

         print(paste('In the file :', out_error$file[i], ', the id', out_error$id[i], ' is present but it is suppose to be on the another subplot which is not adjacent', out_error$sousplot[i]))

      }
   }


   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   duplicate_id_same_file <- all %>%
      dplyr::filter(duplicated_id == 'yes' & what == 'tree') %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(file = unique(file),
                n = n())

   if (nrow(duplicate_id_same_file) > 0) {


      for (i in 1:nrow(duplicate_id_same_file)){
         print(paste('In the file :', duplicate_id_same_file$file[i], ', the id', duplicate_id_same_file$id[i], ' is present', duplicate_id_same_file$n[i], ' times'))

      }


   }

   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                FILES WITH LESS THAN 3 REFERENCES                   '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   less_than_3_ref <- all %>%
      dplyr::group_by(file) %>%
      slice(1) %>%
      dplyr::select(file, n_jalon_ref, n_tree_ref, n_tot_ref) %>%
      dplyr::filter(n_tot_ref < 3)

   if (nrow(less_than_3_ref) > 0) {


      for (i in 1:nrow(less_than_3_ref)){
         print(paste('The file :', less_than_3_ref$file[i], 'has only', less_than_3_ref$n_tot_ref[i], ' reference(s)'))
         print(paste('It has :', less_than_3_ref$n_jalon_ref[i], ' jalon(s)'))
         print(paste('It has :', less_than_3_ref$n_tree_ref[i], ' tree(s)'))
         print(paste('                                                                    '))

      }


   }

   print(paste('                                                                    '))
   print(paste('####################################################################'))
   print(paste('                  FILES WITH LESS THAN 3 JALONS                     '))
   print(paste('####################################################################'))
   print(paste('                                                                    '))

   less_than_3_jalon <- all %>%
      dplyr::group_by(file) %>%
      dplyr::slice(1) %>%
      dplyr::select(file, n_jalon_ref, n_tree_ref, n_tot_ref) %>%
      dplyr::filter(n_jalon_ref < 3)

   if (nrow(less_than_3_jalon) > 0) {


      for (i in 1:nrow(less_than_3_jalon)){
         print(paste('The file :', less_than_3_jalon$file[i], 'has only', less_than_3_jalon$n_jalon_ref[i], ' jalon(s) as reference(s)'))
         print(paste('It has :', less_than_3_jalon$n_jalon_ref[i], ' jalon(s)'))
         print(paste('It has :', less_than_3_jalon$n_tree_ref[i], ' tree(s)'))
         print(paste('                                                                    '))

      }


   }

   sink(file =NULL)

   print(paste('A report has been saved : ', paste0(directory, '/',plot_name,"/5_ALL_PLOT_REPORT.txt")))

}

#' bilinear_interpolation
#' @export
#' @import stats data.table
bilinear_interpolation = function(coord, from_corner_coord, to_corner_coord, ordered_corner = F) {

   # Parameters verification
   if(nrow(from_corner_coord)!=4 | nrow(to_corner_coord)!=4 | nrow(from_corner_coord)!=nrow(from_corner_coord)) {
      stop("from_corner_coord and to_corner_coord must have 4 rows representing the 4 corners of the plot")
   }
   if(!(is.data.frame(coord) | is.matrix(coord) | is.data.table(coord))){
      stop("tree coordinates must be a data.frame, a matrix or a data.table")
   }
   if(is.data.table(coord)) coord <- data.frame(coord)
   if(is.data.table(from_corner_coord) | is.data.table(to_corner_coord)) {
      from_corner_coord <- data.frame(from_corner_coord)
      to_corner_coord <- data.frame(to_corner_coord)
   }

   # to_corner_coord colnames attribution
   if(is.null(colnames(to_corner_coord))) {
      to_corner_coord <- to_corner_coord[,1:2]
      colnames(to_corner_coord) <- c("x_interp","y_interp")
   }

   # Sorting rows if necessary
   centroid <- colMeans(from_corner_coord[,1:2])
   if(!ordered_corner) {
      # Sort from_corner_coord and to_corner_coord rows in a counter-clockwise direction
      angles <- atan2(from_corner_coord[, 2] - centroid[2], from_corner_coord[, 1] - centroid[1])
      from_corner_coord <- from_corner_coord[order(angles), ]
      to_corner_coord <- to_corner_coord[order(angles), ]
   }

   # Verification of a rectangular plot for from_corner_coord
   if(!all(abs(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)] - mean(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)]))<0.01)) {
      stop("The plot in the relative coordinate system is not a rectangle (or a square). You may consider using trustGPScorners = F")
   }

   x_A <- from_corner_coord[1,1] ; x_B <- from_corner_coord[2,1] ; x_C <- from_corner_coord[3,1]  ; x_D <- from_corner_coord[4,1]
   y_A <- from_corner_coord[1,2] ; y_B <- from_corner_coord[2,2] ; y_C <- from_corner_coord[3,2] ; y_D <- from_corner_coord[4,2]
   u_A <- to_corner_coord[1,1] ; u_B <- to_corner_coord[2,1];  u_C <- to_corner_coord[3,1];  u_D <- to_corner_coord[4,1]
   v_A <- to_corner_coord[1,2] ; v_B <- to_corner_coord[2,2] ; v_C <- to_corner_coord[3,2] ; v_D <- to_corner_coord[4,2]

   apply_bilinear_interpolation <- function(x,y,to_corner_coord_colnames) {
      rate_A <- (1-(x-x_A)/(x_C-x_A)) * (1-(y-y_A)/(y_C-y_A))
      rate_B <- (1-(x-x_B)/(x_D-x_B)) * (1-(y-y_B)/(y_D-y_B))
      rate_C <- (1-(x-x_C)/(x_A-x_C)) * (1-(y-y_C)/(y_A-y_C))
      rate_D <- (1-(x-x_D)/(x_B-x_D)) * (1-(y-y_D)/(y_B-y_D))
      interp_df <- data.frame(
         rate_A*u_A + rate_B*u_B + rate_C*u_C + rate_D*u_D,
         rate_A*v_A + rate_B*v_B + rate_C*v_C + rate_D*v_D
      )
      data.table::setnames(interp_df, new = to_corner_coord_colnames)
      interp_df
   }

   return(apply_bilinear_interpolation(x=coord[,1],y=coord[,2],to_corner_coord_colnames=colnames(to_corner_coord)[1:2]))
}

#' extract_jalons
#' @export
#' @import data.table sf dplyr stringr forcats
extract_jalons <- function(coordinates, type = 1){

   longlat = coordinates[, c("typevalue_ddlon", "typevalue_ddlat")]
   coordRel = coordinates[, c("Xrel", "Yrel")]
   rangeX = c(0, 100)
   rangeY = c(0, 100)
   maxDist = 10
   rmOutliers = TRUE
   projCoord = NULL

   if (is.null(longlat)) {
      stop("Give one set of coordinates: coordinates[, c('typevalue_ddlon', 'typevalue_ddlat')]")
   }

   if (!all(data.table::between(coordRel[, 1], lower = rangeX[1], upper = rangeX[2]) &
            data.table::between(coordRel[, 2], lower = rangeY[1], upper = rangeY[2]))) {
      stop("coordinates[, c('Xrel', 'Yrel')] must be inside the 0 and 100 ranges")
   }
   if ((!is.null(longlat) && any(dim(longlat) != dim(coordRel))) ||
       (!is.null(projCoord) && any(dim(projCoord) != dim(coordRel)))) {
      stop("GPS and relative coordinates are not of the same dimension")
   }

   if (!is.null(longlat)) {
      projCoord <- latlong2UTM(longlat)
      codeUTM <- unique(projCoord[, "codeUTM"])
      projCoord <- projCoord[, c("X", "Y")]
   }

   res <- procrust(projCoord, coordRel)
   coordAbs <- as.matrix(coordRel) %*% res$rotation
   coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
   dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[,
                                                               2] - projCoord[, 2])^2)
   outliers <- which(dist > maxDist)
   if (length(outliers) == nrow(projCoord)) {
      stop("All coordinates points are considered as outliers at the first stage.\n\n         This may be because some coordinates have very large error associated.\n\n         Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
   }
   if (rmOutliers & length(outliers) > 0) {
      refineCoord <- TRUE
      while (refineCoord) {
         res <- procrust(projCoord[-outliers, ], coordRel[-outliers,
         ])
         coordAbs <- as.matrix(coordRel) %*% res$rotation
         coordAbs <- sweep(coordAbs, 2, res$translation,
                           FUN = "+")
         newdist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 +
                            (coordAbs[, 2] - projCoord[, 2])^2)
         if (all(which(newdist > maxDist) == outliers))
            refineCoord <- FALSE
         outliers <- which(newdist > maxDist)
      }
   }
   cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
   cornerCoord <- cornerCoord[c(1, 2, 4, 3), ]
   cornerCoord <- as.matrix(cornerCoord) %*% res$rotation
   cornerCoord <- sweep(cornerCoord, 2, res$translation, FUN = "+")
   p <- sf::st_multipoint(rbind(cornerCoord, cornerCoord[1, ]))
   ps <- sf::st_polygon(list(p), 1)
   sps <- sf::st_sfc(list(ps))
   if (length(outliers) != 0 & !rmOutliers) {
      warning("Be carefull, you may have GNSS measurement outliers. \n",
              "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument).")
   }
   correct_plot <- list(cornerCoords = data.frame(X = cornerCoord[,
                                                                  1], Y = cornerCoord[, 2]), correctedCoord = data.frame(X = coordAbs[,
                                                                                                                                      1], Y = coordAbs[, 2]), polygon = sps, outliers = outliers)
   if (!is.null(longlat)) {
      correct_plot$codeUTM <- codeUTM
   }

   projCoord = correct_plot$cornerCoords
   plot = rep("plot", 4)
   cornerNum = c(1, 2, 3, 4)
   gridsize = 20
   dimX = 100
   dimY = 100

   if (!(length(dimY) %in% c(1, length(unique(plot))))) {
      stop("Your dimY vector must be of length 1 or of length equal to length(unique(plot))")
   }
   if (any(gridsize > dimX) || any(gridsize > dimY)) {
      stop("Your gridsize is larger than the X or Y dimensions")
   }
   cornerCoord <- data.table::data.table(plot = plot, X = projCoord[, 1],
                             Y = projCoord[, 2], cornerNum = cornerNum)
   data.table::setnames(cornerCoord, colnames(cornerCoord), c("plot", "X",
                                                  "Y", "cornerNum"))
   cornerCoord <- cornerCoord[order(cornerNum), .SD, by = plot]
   dimRel <- data.table::data.table(plot = unique(plot), dimX = dimX, dimY = dimY)
   gridFunction <- function(data, gridsize) {
      absCoordMat <- as.matrix(data[, .(X, Y)])
      plotDimX <- as.numeric(unique(data[, "dimX"]))
      plotDimY <- as.numeric(unique(data[, "dimY"]))
      relCoordMat <- matrix(c(0, 0, 0, plotDimY, plotDimX,
                              plotDimY, plotDimX, 0), byrow = T, ncol = 2)
      gridMat <- as.matrix(expand.grid(X = seq(0, max(relCoordMat[,
                                                                  1]), by = gridsize), Y = seq(0, max(relCoordMat[,
                                                                                                                  2]), by = gridsize)))
      absCoord <- bilinear_interpolation(coord = gridMat,
                                         from_corner_coord = relCoordMat, to_corner_coord = absCoordMat)
      return(data.table::data.table(XRel = gridMat[, 1], YRel = gridMat[,
                                                            2], XAbs = absCoord[, 1], YAbs = absCoord[, 2]))
   }
   cornerCoord <- cornerCoord[dimRel, on = "plot"][, gridFunction(.SD,
                                                                  gridsize), by = plot]
   numberingCorner <- function(data) {
      rbindlist(apply(data[XRel < max(XRel) & YRel < max(YRel),
                           -"plot"], 1, function(x) {
                              X <- x["XRel"]
                              Y <- x["YRel"]
                              data[(XRel == X & YRel == Y) | (XRel == X + gridsize &
                                                                 YRel == Y) | (XRel == X + gridsize & YRel ==
                                                                                  Y + gridsize) | (XRel == X & YRel == Y + gridsize),
                                   .(subplot = paste(plot, X/gridsize, Y/gridsize,
                                                     sep = "_"), XRel, YRel, XAbs, YAbs)][, `:=`(cornerNum,
                                                                                                 c(1, 4, 2, 3))]
                           }))
   }
   cornerCoord <- cornerCoord[, numberingCorner(.SD), by = plot,
                              .SDcols = colnames(cornerCoord)]
   subplot <- as.data.frame(cornerCoord) %>%
      dplyr::mutate(sousplot = rep(stringr::str_remove(unique(subplot), 'plot_'), each = 4),
                    jalon = paste(XRel,YRel,sep='_')) %>%
      dplyr::select(sousplot, jalon, XRel, YRel, XAbs, YAbs, cornerNum)

   if(type == 1 ){
      subplot$sousplot <- forcats::fct_recode(subplot$sousplot,
                                              "0_0" = '0_0',
                                              "20_0" = '0_1',
                                              "40_0" = '0_2',
                                              "60_0" = '0_3',
                                              "80_0" = '0_4',
                                              "0_20" = '1_0',
                                              "20_20" = '1_1',
                                              "40_20" = '1_2',
                                              "60_20" = '1_3',
                                              "80_20" = '1_4',
                                              "0_40" = '2_0',
                                              "20_40" = '2_1',
                                              "40_40" = '2_2',
                                              "60_40" = '2_3',
                                              "80_40" = '2_4',
                                              "0_60" = '3_0',
                                              "20_60" = '3_1',
                                              "40_60" = '3_2',
                                              "60_60" = '3_3',
                                              "80_60" = '3_4',
                                              "0_80" = '4_0',
                                              "20_80" = '4_1',
                                              "40_80" = '4_2',
                                              "60_80" = '4_3',
                                              "80_80" = '4_4'
      )

      subplot$jalon <- forcats::fct_recode(subplot$jalon,
                                           "20_0" = '0_20',
                                           "40_0" = '0_40',
                                           "60_0" = '0_60',
                                           "80_0" = '0_80',
                                           "100_0" = '0_100',
                                           "0_20" = '20_0',
                                           "20_20" = '20_20',
                                           "40_20" = '20_40',
                                           "60_20" = '20_60',
                                           "80_20" = '20_80',
                                           "100_20" = '20_100',
                                           "0_40" = '40_0',
                                           "20_40" = '40_20',
                                           "40_40" = '40_40',
                                           "60_40" = '40_60',
                                           "80_40" = '40_80',
                                           "100_40" = '40_100',
                                           "0_60" = '60_0',
                                           "20_60" = '60_20',
                                           "40_60" = '60_40',
                                           "60_60" = '60_60',
                                           "80_60" = '60_80',
                                           "100_60" = '60_100',
                                           "0_80" = '80_0',
                                           "20_80" = '80_20',
                                           "40_80" = '80_40',
                                           "60_80" = '80_60',
                                           "80_80" = '80_80',
                                           "100_80" = '80_100',
                                           "0_100" = '100_0',
                                           "20_100" = '100_20',
                                           "40_100" = '100_40',
                                           "60_100" = '100_60',
                                           "80_100" = '100_80',
                                           "100_100" = '100_100'
      )

      subplot$XRel = as.numeric(stringr::str_split(as.character(subplot$jalon), pattern="_", simplify = T)[,1])
      subplot$YRel = as.numeric(stringr::str_split(as.character(subplot$jalon), pattern="_", simplify = T)[,2])

   }

   if(type == 2){

      subplot$sousplot <- forcats::fct_recode(subplot$sousplot,
                                              "0_0" = '0_0',
                                              "0_20" = '0_1',
                                              "0_40" = '0_2',
                                              "0_60" = '0_3',
                                              "0_80" = '0_4',
                                              "20_0" = '1_0',
                                              "20_20" = '1_1',
                                              "20_40" = '1_2',
                                              "20_60" = '1_3',
                                              "20_80" = '1_4',
                                              "40_0" = '2_0',
                                              "40_20" = '2_1',
                                              "40_40" = '2_2',
                                              "40_60" = '2_3',
                                              "40_80" = '2_4',
                                              "60_0" = '3_0',
                                              "60_20" = '3_1',
                                              "60_40" = '3_2',
                                              "60_60" = '3_3',
                                              "60_80" = '3_4',
                                              "80_0" = '4_0',
                                              "80_20" = '4_1',
                                              "80_40" = '4_2',
                                              "80_60" = '4_3',
                                              "80_80" = '4_4'
      )
   }



   return(subplot)
}

#' Field_comparison
#' @export
#' @import dplyr ggplot2 ggrepel grDevices sf
Field_comparison <- function (Fieldplot_BDD_full, all, directory, plot_name) {


   if(!file.exists(paste0(directory,'/',plot_name,"/Field_scan_comparison"))){
      dir.create(paste0(directory,'/',plot_name,"/Field_scan_comparison"))
   }

   if('position_x' %in% names(Fieldplot_BDD_full) | 'position_y' %in% names(Fieldplot_BDD_full)) {

      field_xy <- Fieldplot_BDD_full$extract %>%
         dplyr::filter((!(is.na(position_x)) & !(is.na(position_y)))) %>%
         dplyr::select(ind_num_sous_plot, position_x, position_y) %>%
         dplyr::rename(id = ind_num_sous_plot) %>%
         dplyr::mutate(id = as.character(id))

      all <- all %>% dplyr::left_join(field_xy)

      all_comparison <- all %>% dplyr::filter((!(is.na(position_x)) & !(is.na(position_y))) | what == 'jalon')

      for (i in 1:length(unique(all_comparison$sousplot))) {

         tmp <- all_comparison %>%
            dplyr::group_by(id) %>%
            dplyr::arrange(match(method, c("method1", "method2")), .by_group = TRUE, across(starts_with("method2"))) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::filter (sousplot == unique(all_comparison$sousplot)[i])

         tmp_tree <- tmp %>% dplyr::filter(what == 'tree')
         tmp_jalon <- tmp %>% dplyr::filter(what == 'jalon')

         my_plot <- ggplot2::ggplot() +
            ggrepel::geom_label_repel(data = tmp_tree, aes(label = id,x=TRUE_X, y=TRUE_Y), size = 5, fill = 'blue', col = 'black') +
            ggrepel::geom_label_repel(data = tmp_tree, aes(label = id,x=position_x, y=position_y), size = 5, fill = 'red', col = 'black') +
            ggrepel::geom_label_repel(data = tmp_jalon, aes(label = id,x=TRUE_X, y=TRUE_Y), size = 5, fill = 'green', col = 'black') +
            ggplot2::theme_classic() +
            ggplot2::theme(
               panel.grid.major = element_line(colour = "black"),
               panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
               panel.background = element_rect(fill = "black")) +
            ggplot2::ggtitle(
               paste0('COMPARISON FIELD AND SCAN :   ', unique(tmp$sousplot)),
               subtitle = 'green : jalon,  blue : LIDAR,   red : manual')

         grDevices::png(paste0(directory,'/',plot_name,"/Field_scan_comparison/",'Field_scan_comparison_', str_remove(unique(tmp$file),'.csv'),'.png'), width = 600)
         print(my_plot)
         grDevices::dev.off()
      }

      tmp <- all %>%
         dplyr::filter((!(is.na(position_x)) & !(is.na(position_y)))) %>%
         dplyr::filter(!is.nan(TRUE_X))
      x_field <- tmp %>% .[['position_x']] %>% as.numeric()
      y_field <- tmp %>% .[['position_y']] %>% as.numeric()
      x_LIDAR <- tmp %>% .[['TRUE_X']] %>% as.numeric()
      y_LIDAR <- tmp %>% .[['TRUE_Y']] %>% as.numeric()

      coord_field <- as.matrix(cbind(x_field,y_field))
      coord_LIDAR <- as.matrix(cbind(x_LIDAR,y_LIDAR))

      pt_field <- sf::st_cast(sf::st_sfc(sf::st_multipoint(coord_field)), "POINT")
      pt_LIDAR <- sf::st_cast(sf::st_sfc(sf::st_multipoint(coord_LIDAR)), "POINT")

      distance <- sf::st_distance(pt_field,pt_LIDAR, by_element = TRUE)

      # hist(distance, xlab = 'Distance (m)', main = plot_name)

      png(paste0(directory,'/',plot_name,"/distance_", plot_name,'.png'), width = 600)
      hist(distance, breaks = c(0:45), xlab = 'Distance (m)', main = paste(plot_name, '  |  n = ', length(distance)))
      dev.off()

      tmp_dist <- tmp %>% dplyr::mutate(distance = distance) %>% dplyr::select(file, id, distance)

      write.xlsx(tmp_dist, file = paste0(directory,'/',plot_name,"/distance.xlsx"), append = FALSE)

   }else{

      warning("X and Y coordinates should be stored in the position_x & position_y variables")
   }



}

#' files_summary
#' @export
#' @import stringr dplyr
files_summary <- function(root_in, export = FALSE, directory = NULL, plot_name = NULL){


   List_file = dir(root_in)
   List_path = dir(root_in, full.names = T)
   list_of_Lidar <- list()


   for (i in 1 : length(List_path)) {

      tmp <- readLines(List_path[i]) %>% stringr::str_replace_all(',',';')

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

      list_of_Lidar[[i]] <- tmp

      if(length(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]]) == 5){
         names(list_of_Lidar)[i] <- paste(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]][3:5], collapse = '_')
      }

      if(length(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]]) == 4){
         names(list_of_Lidar)[i] <- paste(stringr::str_split(stringr::str_remove(basename(List_path[i]), '.csv'), '_')[[1]][3:4], collapse = '_')
      }

   }

   for (i in 1:length(list_of_Lidar)) {

      tmp_tree <- list_of_Lidar[[i]] %>% dplyr::filter(stringr::str_detect(id, '_', negate = TRUE)) %>% .[['id']] %>% unique() %>% length()
      tmp_jalon <- list_of_Lidar[[i]] %>% dplyr::filter(stringr::str_detect(id, '_', negate = FALSE)) %>% .[['id']] %>% unique() %>% length()

      if(i == 1){nb_tree <- tmp_tree ; nb_jalon <- tmp_jalon } else{nb_tree <- c(nb_tree,tmp_tree) ; nb_jalon <- c(nb_jalon,tmp_jalon)}
   }

   file_summary <-

      List_file %>%
      stringr::str_remove('.csv') %>%
      stringr::str_split('_', simplify = TRUE) %>%
      {if (ncol(.) == 4) `colnames<-`(.,c('site','plot_number','x','y')) else `colnames<-`(.,c('site','plot_number','x','y','scan_number'))} %>%
      dplyr::as_tibble() %>%
      {if (ncol(.) == 4) dplyr::mutate(.,scan_number = NA_character_) else .} %>%
      dplyr::mutate(subplot = paste(x,y,sep='_'),
                    nb_tree = nb_tree,
                    nb_jalon = nb_jalon,
                    path = List_path)



   if(TRUE){

      sink(file = file.path(directory,plot_name,"1_check_files_basenames.txt"))

      print(as.data.frame(file_summary) )

      sink(file= NULL)

      print(paste('A report has been saved : ', file.path(directory,plot_name,"1_check_files_basenames.txt") ))

   }

   return(file_summary = file_summary)


}

#' procrust
#' @export
procrust <- function(X, Y) {
   xmean <- colMeans(X)
   ymean <- colMeans(Y)

   X <- scale(X, scale = FALSE)
   Y <- scale(Y, scale = FALSE)

   XY <- crossprod(X, Y)
   sol <- svd(XY)
   A <- sol$v %*% t(sol$u)

   b <- xmean - ymean %*% A

   return(list(rotation = A, translation = b))
}

#' latlong2UTM
#' @export
#' @import data.table proj4
latlong2UTM <- function (coord)
{
   coord <- data.table::data.table(coord, check.names = TRUE)
   data.table::setnames(coord, colnames(coord), c("long", "lat"))
   if (!requireNamespace("proj4")) {
      stop("Please install the package 'proj4'\n\n         \t\tinstall.packages('proj4').")
   }
   codelatlong2UTM <- function(long, lat) {
      Nzone <- (floor((long + 180)/6)%%60) + 1
      Nzone <- paste0(Nzone, ifelse(lat >= 0, " +north ",
                                    " +south "))
      Nzone <- paste0("+proj=utm +zone=", Nzone, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      return(Nzone)
   }
   coord[, `:=`(codeUTM, codelatlong2UTM(long, lat))]
   coord[, `:=`(c("X", "Y"), proj4::project(.(long, lat), proj = unique(.BY))),
         by = codeUTM]
   data.table::setDF(coord)
   return(coord)
}

#' import_plotdata
#' @export
import_plotdata <- function(method,plot_name){

   if(method == 'plotsdatabase') {
      Fieldplot_BDD_full = plotsdatabase::query_plots(plot_name = plot_name, show_all_coordinates = TRUE, map = F, extract_individuals = T)
      Tree_list <- Fieldplot_BDD_full$extract %>% dplyr::group_by(sous_plot_name) %>% dplyr::summarise (id = list(ind_num_sous_plot))

      return(Fieldplot_BDD_full = Fieldplot_BDD_full)

   }

   if(method == 'local_file') {
      a = "PLEASE IMPORT INVENTORY DATA BY OUR OWN, THE DATA MUST BE NAMED 'Tree_list' "
      return(a)
   }

}

#' plot_alltrees
#' @export
#' @import grDevices ggplot2 dplyr sf
plot_alltrees <- function(
      Fieldplot_BDD_full,
      all,
      subplot,
      crs,
      col,
      plot_name,
      directory)

   {

      id_ssplot <-
         Fieldplot_BDD_full$extract %>%
         dplyr::select(ind_num_sous_plot,sous_plot_name) %>%
         dplyr::rename(id = ind_num_sous_plot)


      id_ssplot$id <- as.character(id_ssplot$id)


      all <-
         all %>%
         dplyr::left_join(id_ssplot)


      all <-
         all %>%
         dplyr::mutate(sous_plot_name = dplyr::case_when(
            is.na(sous_plot_name) & what == 'tree' ~ 'tree not in inventory data',
            is.na(sous_plot_name) & what == 'jalon' ~ 'jalon',
            TRUE ~ sous_plot_name
         ))


      subplot_sf <-
         subplot %>%
         dplyr::group_by(sousplot) %>%
         dplyr::summarise(sousplot = unique(sousplot),
                          X1 = XAbs[1],
                          X2 = XAbs[2],
                          X3 = XAbs[4],
                          X4 = XAbs[3],
                          X5 = XAbs[1],
                          Y1 = YAbs[1],
                          Y2 = YAbs[2],
                          Y3 = YAbs[4],
                          Y4 = YAbs[3],
                          Y5 = YAbs[1])

      for (i in 1:25) {

         tmp  <-
            sf::st_polygon(
               list(
                  rbind(
                     c(X = subplot_sf$X1[i],Y = subplot_sf$Y1[i]),
                     c(X = subplot_sf$X2[i],Y = subplot_sf$Y2[i]),
                     c(X = subplot_sf$X3[i],Y = subplot_sf$Y3[i]),
                     c(X = subplot_sf$X4[i],Y = subplot_sf$Y4[i]),
                     c(X = subplot_sf$X1[i],Y = subplot_sf$Y1[i])
                  )
               )
            )

         assign (paste('tttttt',i,sep = "_"), tmp)

      }

      nrows <- 25

      sub_plot <- sf::st_sf(crs = crs,
                            sous_plot_name = 1:nrows,
                            geometry = sf::st_sfc(lapply(1:nrows,
                                                         function(x) sf::st_geometrycollection())
                            )
      ) # Create a fake multipolygons of 25 object with the good crs

      # Add the right 25 polygon geometry
      for (j in 1:25) {sub_plot$geometry[j] <- mget(ls(pattern = "tttttt"))[[j]]}

      myplot <- ggplot2::ggplot(all ) +
         ggplot2::geom_sf(data = sub_plot, fill = 'black') +
         ggplot2::geom_point(aes(x=XAbs, y=YAbs, col = sous_plot_name, size = what)) +
         ggplot2::scale_size_manual(values = c('jalon' = 5,
                                               'tree' = 3)) +
         ggplot2::scale_color_manual(values = col) +
         ggplot2::facet_wrap(~method) +
         ggplot2::ggtitle(plot_name) +
         ggplot2::theme_bw() +
         ggplot2::theme_classic()


      grDevices::png(paste0(directory,'/',plot_name,'/all_projected_trees.png'), width = 600)

      print(myplot)

      grDevices::dev.off()

      print(paste0('PLOT SAVE IN :', directory,'/',plot_name,"/all_projected_trees.jpg"))


      return(list(sub_plot = sub_plot, all_tress_plot = myplot))

   }

#' reproj_20_20
#' @export
#' @import dplyr stringr ggplot2 ggrepel grDevices
reproj_20_20 <- function (all, subplot, directory, plot_name) {

   dir.create(paste0(directory,'/',plot_name,"/20_20_PROJECTION"))


   for (i in 1:length(unique(all$file))) {

      tmp <- all %>% dplyr::filter(file == unique(all$file)[i])
      jalon_ref <- subplot %>% dplyr::filter(sousplot == unique(tmp$sousplot))
      jalon_ref <- jalon_ref[as.character(jalon_ref$jalon) %in% tmp$id,]

      jalon_ref <-jalon_ref %>% dplyr::left_join(tmp %>% dplyr::filter(what == 'jalon' & where == 'in' ) %>% dplyr::select(-c("XAbs", "YAbs")), by = dplyr::join_by(jalon == id))

      res <-procrust((jalon_ref[,c("XRel", "YRel")]), jalon_ref[,c("XAbs", "YAbs")])


      coordAbs_allTrees <- as.matrix(tmp[,c("XAbs", "YAbs")]) %*% res$rotation
      coordAbs_allTrees <- sweep(coordAbs_allTrees, 2, res$translation, FUN = "+")
      tmp$TRUE_X <- coordAbs_allTrees[,1]
      tmp$TRUE_Y <- coordAbs_allTrees[,2]

      TRUE_XY <- tmp
      assign(  paste("TRUE_XY_20_20", unique(TRUE_XY$file), sep = "_"), TRUE_XY )

   }

   list_of_objects <- mget(ls(pattern="TRUE_XY_20_20"))

   # bind the elements together into a data frame

   all <- do.call("rbind", list_of_objects)

   all$quadrat_x <- as.numeric(stringr::str_split(all$sousplot, '_', simplify = TRUE)[,1])
   all$quadrat_y <- as.numeric(stringr::str_split(all$sousplot, '_', simplify = TRUE)[,2])

   all <- all %>%

      dplyr::mutate(
         TRUE_X_20 = TRUE_X - quadrat_x,
         TRUE_Y_20 = TRUE_Y - quadrat_y) %>%

      dplyr::mutate(
         quadrat_x_mesured = dplyr::case_when(
            TRUE_X < 20 ~ 0,
            TRUE_X > 20 & TRUE_X < 40 ~ 20,
            TRUE_X > 40 & TRUE_X < 60 ~ 40,
            TRUE_X > 60 & TRUE_X < 80 ~ 60,
            TRUE_X > 80 ~ 80

         ),

         quadrat_y_mesured = dplyr::case_when(
            TRUE_Y < 20 ~ 0,
            TRUE_Y > 20 & TRUE_Y < 40 ~ 20,
            TRUE_Y > 40 & TRUE_Y < 60 ~ 40,
            TRUE_Y > 60 & TRUE_Y < 80 ~ 60,
            TRUE_Y > 80 ~ 80

         ),

         quadrat_mesured = paste(quadrat_x_mesured,quadrat_y_mesured,sep = '_'),

         quadrat_check = dplyr::case_when(
            quadrat_mesured == sousplot & what != 'jalon' ~ TRUE,
            quadrat_mesured != sousplot & what != 'jalon' ~ FALSE,
            TRUE ~ NA
         )) %>%
      dplyr::select(-c(quadrat_x_mesured,quadrat_y_mesured))

   for (i in 1:length(unique(all$sousplot))){


      tmp <- all %>% dplyr::filter(sousplot == unique(all$sousplot)[i])

      my_plot <- ggplot2::ggplot(tmp) +
         ggrepel::geom_label_repel(ggplot2::aes(label = id,x=TRUE_X_20, y=TRUE_Y_20, col = where, fill = what), size = 5) +
         ggplot2::scale_color_manual(values = c('in' = 'black', 'out' = 'red', 'adjacent' = 'orange')) +
         ggplot2::scale_fill_manual(values = c('tree' = 'lightgreen', 'jalon' = 'white')) +
         ggplot2::ggtitle(unique(tmp$sousplot)) +
         ggplot2::theme_classic() +
         ggplot2::theme(
            panel.grid.major = element_line(colour = "black"),
            panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
            panel.background = element_rect(fill = "black")) +
         ggplot2::ggtitle(paste0('REPROJECTION FOR THE SUBPLOT :   ', unique(tmp$sousplot)))

      grDevices::png(paste0(directory,'/',plot_name,"/20_20_PROJECTION/",'20_20_PROJECTION_', str_remove(unique(tmp$file),'.csv'),'.png'), width = 600)
      print(my_plot)
      grDevices::dev.off()
   }

   return(all)
}

#' save_gpkg
#' @export
#' @import dplyr sf
save_gpkg <-

   function(all, crs, sub_plot, directory, plot_name){

      unique_id <- all %>%
         dplyr::filter(what == 'tree') %>%
         dplyr::mutate(id = as.numeric(id)) %>%
         dplyr::group_by(id) %>%
         dplyr::arrange(match(method, c("jalon", "trees")), .by_group = TRUE, dplyr::across(dplyr::starts_with("trees"))) %>%
         dplyr::slice(1)

      all_sf = sf::st_as_sf(as.data.frame(all), coords = c("XAbs", "YAbs"), crs = crs, agr = "constant")
      unique_id_sf = sf::st_as_sf(as.data.frame(unique_id), coords = c("XAbs", "YAbs"), crs = crs, agr = "constant")

      sf::st_write(all_sf, paste0(directory,'/',plot_name,'/',plot_name,"_all_project_trees",'.gpkg'),append=FALSE)
      sf::st_write(unique_id_sf, paste0(directory,'/',plot_name,'/',plot_name,"_unique_id_sf",'.gpkg'),append=FALSE)
      sf::st_write(sub_plot, paste0(directory,'/',plot_name,'/',plot_name,"_subplot",'.gpkg'),append=FALSE)

   }

#' XY_computation
#' @export
#' @import dplyr stringr ggplot2 grDevices BIOMASS writexl
XY_computation <-

   function(
      Fieldplot_BDD_full,
      raw_files_path,
      subplot,
      plot_name,
      files_Summary,
      directory){


      files_Summary_jalon <- files_Summary %>% dplyr::filter(nb_jalon > 2)
      # files_Summary_trees <- files_Summary %>% dplyr::filter(nb_jalon < 3)

      if(nrow(files_Summary_jalon) > 0 ){
         dir.create(file.path(directory,plot_name,"xy_rawdata_method1"))
         dir.create(file.path(directory,plot_name,"xy_reproject_method1"))

         for(i in 1:nrow(files_Summary_jalon) ){

            file_info <- files_Summary_jalon[i,]

            sousplot <- file_info$subplot
            filepath <- file_info$path

            if(file_info$scan_number == '' | is.na(file_info$scan_number)){

               pattern = paste0('_',sousplot,'$')
               file = raw_files_path[grep(pattern, stringr::str_remove(raw_files_path,'.csv'))]

               tmp <- read.csv(paste0(directory,'/',plot_name,'/1_rawData/',file))

            }else{

               pattern = paste0('_',sousplot,'_',file_info$scan_number,'$')
               file = raw_files_path[grep(pattern, stringr::str_remove(raw_files_path,'.csv'))]

               tmp <- read.csv(paste0(directory,'/',plot_name,'/1_rawData/',file))
            }


            LIDAR_sousplot <-
               tmp %>%

               dplyr::mutate(

                  file = basename(filepath),

                  sousplot = sousplot,

               )%>%

               dplyr::select (file, sousplot, what, id, where, X_lidar, Y_lidar, duplicated_id)

            check_jalon <- tmp %>%
               dplyr::filter(what == 'jalon') %>% .[['where']]
            check_jalon = (length(check_jalon[check_jalon == 'in']) > 2)

            if(check_jalon){

               jalon_ref <-
                  dplyr::as_tibble(subplot) %>%
                  dplyr::filter(jalon %in% (LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ) %>%
                                               .[['id']]) ) %>%
                  dplyr::select(jalon, XAbs,YAbs) %>%
                  dplyr::group_by(jalon) %>%
                  dplyr::slice(1) %>%
                  dplyr::ungroup()


               jalon_ref <-
                  jalon_ref %>%
                  dplyr::left_join(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ), by = join_by(jalon == id))


               # Plot title definitiuon --------------------------------------------------


               if(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'out' ) %>% nrow() != 0){

                  title = 'PROBLEME WITH JALON'

               } else{title = ''}


               if(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where %in% c('out','adjacent') ) %>% nrow() != 0 ){

                  title = paste(title, '| SOME TREES ARE OUT OF THIS SUBPLOT')} else{title = paste(title, '')

                  }


               if(TRUE %in% stringr::str_detect(LIDAR_sousplot$duplicated_id, 'yes')){

                  title = paste(title, '| SOME ID ARE DUPLICATED')

               } else{title = paste(title, '')}


               if(title == '  '){title = 'NO PROBLEM DETECTED'}


               # Plot raw data -----------------------------------------------------------

               my_plot <-
                  ggplot2::ggplot(LIDAR_sousplot, ggplot2::aes(x=X_lidar, y=Y_lidar)) +
                  ggplot2::geom_label(ggplot2::aes(label = id, fill = what)) +
                  ggplot2::geom_point(ggplot2::aes(y=Y_lidar+0.000008,shape = where, col = duplicated_id), size = 3) +
                  ggplot2::scale_color_manual(values = c('yes' = 'red', 'no' = 'white')) +
                  ggplot2::theme_classic() +
                  ggplot2::theme(
                     panel.grid.major = element_line(colour = "black"),
                     panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
                     panel.background = element_rect(fill = "black")) +
                  ggplot2::ggtitle(title)


               grDevices::png(paste0(directory,'/',plot_name,"/xy_rawdata_method1", '/RAWDATA_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
               print(my_plot)
               grDevices::dev.off()

               # Reproject data ----------------------------------------------------------

               projCoord <- BIOMASS::latlong2UTM(LIDAR_sousplot[,c('X_lidar','Y_lidar')])
               codeUTM <- unique(projCoord[, "codeUTM"])
               projCoord <- projCoord[, c("X", "Y")]
               LIDAR_sousplot$X_iPhone = projCoord$X
               LIDAR_sousplot$Y_iPhone = projCoord$Y

               jalon_ref$LATiPhone = jalon_ref$LONGiPhone = jalon_ref$X_iPhone = jalon_ref$Y_iPhone = NA

               for(j in 1:nrow(jalon_ref)){

                  bla <- jalon_ref[j,'jalon'] %>% dplyr::pull()
                  jalon_ref$X_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['X_iPhone']]
                  jalon_ref$Y_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['Y_iPhone']]

               }

               res <-procrust(jalon_ref[,c("XAbs", "YAbs")], jalon_ref[,c("X_iPhone", "Y_iPhone")])

               coordAbs_allTrees <- as.matrix(LIDAR_sousplot[,c("X_iPhone", "Y_iPhone")]) %*% res$rotation
               coordAbs_allTrees <- sweep(coordAbs_allTrees, 2, res$translation, FUN = "+")
               LIDAR_sousplot$XAbs <- coordAbs_allTrees[,1]
               LIDAR_sousplot$YAbs <- coordAbs_allTrees[,2]



               # Plot reprojected data ---------------------------------------------------

               my_plot <-
                  ggplot2::ggplot(LIDAR_sousplot) +
                  ggrepel::geom_label_repel(ggplot2::aes(label = id,x=XAbs, y=YAbs, col = where, fill = what), size = 5) +
                  ggplot2::scale_color_manual(values = c('in' = 'black', 'out' = 'red', 'adjacent' = 'orange')) +
                  ggplot2::scale_fill_manual(values = c('tree' = 'lightgreen', 'jalon' = 'white')) +
                  ggplot2::ggtitle(sousplot) +
                  ggplot2::theme_classic() +
                  ggplot2::theme(
                     panel.grid.major = element_line(colour = "black"),
                     panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
                     panel.background = element_rect(fill = "black")) +
                  ggplot2::ggtitle(paste0('REPROJECTION FOR THE FILE :   ', unique(LIDAR_sousplot$file)))


               grDevices::png(paste0(directory,'/',plot_name,"/xy_reproject_method1", '/REPROJECT_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
               print(my_plot)
               grDevices::dev.off()

               LIDAR_sousplot <-
                  LIDAR_sousplot %>%
                  dplyr::mutate(
                     plot = plot_name,
                     method = 'jalon') %>%
                  dplyr::mutate(., n_jalon_ref = length(unique(jalon_ref$jalon)) ) %>%
                  dplyr::mutate(., n_tree_ref = 0 )  %>%
                  dplyr::mutate(n_tot_ref = n_jalon_ref + n_tree_ref )


               assign(  paste("Reproj_XY_jalon", unique(LIDAR_sousplot$file), sep = "_"), LIDAR_sousplot )
               }



         }

         list_of_objects <- mget(ls(pattern="Reproj_XY_jalon"))
         all_jalon_method <- do.call("rbind", list_of_objects)

      }

      # if(nrow(files_Summary_trees) > 0 ){
      #    dir.create(file.path(directory,plot_name,"xy_rawdata_method2"))
      #    dir.create(file.path(directory,plot_name,"xy_reproject_method2"))
      #
      #    for(i in 1:nrow(files_Summary_jalon) ){
      #
      #       file_info <- files_Summary_trees[i,]
      #
      #       sousplot <- file_info$subplot
      #       filepath <- file_info$path
      #
      #       # Extract jalons and subplots informations --------------------------------
      #
      #       x_min <- as.numeric(stringr::str_split(sousplot, '_') [[1]] [1])
      #       x_max <- as.numeric(stringr::str_split(sousplot, '_') [[1]] [1]) + 20
      #       y_min <- as.numeric(stringr::str_split(sousplot, '_') [[1]] [2])
      #       y_max <- as.numeric(stringr::str_split(sousplot, '_') [[1]] [2]) + 20
      #
      #
      #       jalon_theo <-
      #          expand.grid(seq(x_min, x_max, 20), seq(y_min, y_max, 20)) %>%
      #          dplyr::mutate(jalon = paste(Var1, Var2, sep = '_')) %>%
      #          .[['jalon']]
      #
      #
      #       adjacent_sousplot <-
      #          expand.grid(seq(x_min - 20, x_max, 20), seq(y_min - 20, y_max, 20)) %>%
      #          dplyr::filter(Var1 >= 0 &
      #                    Var1 < 100 & Var2 >= 0 & Var2 < 100) %>%
      #          dplyr::mutate(subplot = paste(Var1, Var2, sep = '_')) %>%
      #          dplyr::filter(subplot != sousplot) %>% .[['subplot']]
      #
      #
      #
      #       # Extract data for the subplot --------------------------------------------
      #
      #       trees_inventory <-
      #          Fieldplot_BDD_full$extract %>%
      #          dplyr::filter(sous_plot_name %in% sousplot)
      #
      #
      #       id_sousplot <-
      #          trees_inventory %>%
      #          dplyr::filter (sous_plot_name == sousplot) %>%
      #          .[['ind_num_sous_plot']]
      #
      #       id_adj_sousplot <-
      #          trees_inventory %>%
      #          dplyr::filter (sous_plot_name %in% adjacent_sousplot) %>%
      #          .[['ind_num_sous_plot']]
      #
      #       if(file_info$scan_number == '' | is.na(file_info$scan_number)){
      #
      #          pattern = paste0('_',sousplot,'$')
      #          file = raw_files_path[grep(pattern, stringr::str_remove(raw_files_path,'.csv'))]
      #
      #          tmp <- read.csv(paste0(directory,'/',plot_name,'/1_rawData/',file))
      #
      #       }else{
      #
      #          pattern = paste0('_',sousplot,'_',file_info$scan_number,'$')
      #          file = raw_files_path[grep(pattern, stringr::str_remove(raw_files_path,'.csv'))]
      #
      #          tmp <- read.csv(paste0(directory,'/',plot_name,'/1_rawData/',file))
      #       }
      #
      #
      #       LIDAR_sousplot <-
      #          tmp %>%
      #
      #          dplyr::mutate(
      #
      #             file = basename(filepath),
      #
      #             sousplot = sousplot,
      #          )%>%
      #
      #          dplyr::select (file, sousplot, what, id, where, X_lidar, Y_lidar, duplicated_id)
      #
      #
      #       dup.id <-
      #          LIDAR_sousplot %>%
      #          dplyr::filter(duplicated(id)) %>% .[['id']]
      #
      #       jalon_ref <-
      #          dplyr::as_tibble(subplot) %>%
      #          dplyr::filter(jalon %in% (LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ) %>%
      #                                       .[['id']]) ) %>%
      #          dplyr::select(jalon, XAbs,YAbs) %>%
      #          dplyr::group_by(jalon) %>%
      #          dplyr::slice(1) %>%
      #          dplyr::ungroup()
      #
      #
      #       jalon_ref <-
      #          jalon_ref %>%
      #          dplyr::left_join(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'in' ), by = join_by(jalon == id))
      #
      #
      #       jalon_ref <-
      #          jalon_ref %>%
      #          dplyr::rename(id = jalon) %>%
      #          dplyr::select(-sousplot) %>%
      #          dplyr::select(id, XAbs, YAbs, file, what, where, X_lidar, Y_lidar)
      #
      #       tree_ref <- all_jalon_method[(all_jalon_method$id) %in% (LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where %in% c('in','adjacent')) %>% .[['id']]),] %>%
      #          dplyr::select(id, XAbs, YAbs, file, what, where, X_lidar, Y_lidar)
      #
      #       all_ref <- rbind(jalon_ref, tree_ref)
      #
      #       LIDAR_sousplot <- LIDAR_sousplot %>%
      #          dplyr::mutate(
      #             ref =
      #                dplyr::case_when(
      #                   id %in% all_ref$id ~ 'yes',
      #                   TRUE ~ 'no'),
      #
      #             duplicated_id =
      #                dplyr::case_when(
      #                   id %in% dup.id ~ 'yes',
      #                   TRUE ~ 'no'
      #                )
      #          )
      #
      #
      #       # Plot title definitiuon --------------------------------------------------
      #
      #
      #       if(LIDAR_sousplot %>% dplyr::filter(what == 'jalon' & where == 'out' ) %>% nrow() != 0){
      #
      #          title = 'PROBLEME WITH JALON'
      #
      #       } else{title = ''}
      #
      #
      #       if(LIDAR_sousplot %>% dplyr::filter(what == 'tree' & where %in% c('out','adjacent') ) %>% nrow() != 0 ){
      #
      #          title = paste(title, '| SOME TREES ARE OUT OF THIS SUBPLOT')} else{title = paste(title, '')
      #
      #          }
      #
      #
      #       if(TRUE %in% stringr::str_detect(LIDAR_sousplot$duplicated_id, 'yes')){
      #
      #          title = paste(title, '| SOME ID ARE DUPLICATED')
      #
      #       } else{title = paste(title, '')}
      #
      #
      #       if(title == '  '){title = 'NO PROBLEM DETECTED'}
      #
      #
      #       # Plot raw data -----------------------------------------------------------
      #
      #       my_plot <-
      #          ggplot2::ggplot(LIDAR_sousplot, ggplot2::aes(x=X_lidar, y=Y_lidar)) +
      #          ggplot2::geom_label(ggplot2::aes(label = id, fill = ref))  +
      #          ggplot2::geom_point(ggplot2::aes(y=Y_lidar+0.000008,shape = where, col = duplicated_id), size = 3) +
      #          ggplot2::scale_color_manual(values = c('yes' = 'red', 'no' = 'white')) +
      #          ggplot2::scale_fill_manual(values = c('yes' = 'red', 'no' = 'white'))  +
      #          ggplot2::theme_classic() +
      #          ggplot2::theme(
      #             panel.grid.major = element_line(colour = "black"),
      #             panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
      #             panel.background = element_rect(fill = "black")) +
      #          ggtitle(paste0(unique(LIDAR_sousplot$file)))
      #
      #
      #
      #
      #       grDevices::png(paste0(directory,'/',plot_name,"/xy_rawdata_method1", '/RAWDATA_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      #       print(my_plot)
      #       grDevices::dev.off()
      #
      #
      #
      #
      #       # Reproject data ----------------------------------------------------------
      #
      #       projCoord <- BIOMASS::latlong2UTM(LIDAR_sousplot[,c('X_lidar','Y_lidar')])
      #       codeUTM <- unique(projCoord[, "codeUTM"])
      #       projCoord <- projCoord[, c("X", "Y")]
      #       LIDAR_sousplot$X_iPhone = projCoord$X
      #       LIDAR_sousplot$Y_iPhone = projCoord$Y
      #
      #       all_ref$LATiPhone = all_ref$LONGiPhone = all_ref$X_iPhone = all_ref$Y_iPhone = NA
      #
      #       for(j in 1:nrow(all_ref)){
      #
      #          bla <- all_ref[j,'id'] %>% dplyr::pull()
      #          all_ref$X_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['X_iPhone']]
      #          all_ref$Y_iPhone[j] = LIDAR_sousplot %>% dplyr::filter(id == bla) %>% .[['Y_iPhone']]
      #
      #       }
      #
      #       res <-procrust(all_ref[,c("XAbs", "YAbs")], all_ref[,c("X_iPhone", "Y_iPhone")])
      #
      #       coordAbs_allTrees <- as.matrix(LIDAR_sousplot[,c("X_iPhone", "Y_iPhone")]) %*% res$rotation
      #       coordAbs_allTrees <- sweep(coordAbs_allTrees, 2, res$translation, FUN = "+")
      #       LIDAR_sousplot$XAbs <- coordAbs_allTrees[,1]
      #       LIDAR_sousplot$YAbs <- coordAbs_allTrees[,2]
      #
      #
      #       # Plot reprojected data ---------------------------------------------------
      #
      #       my_plot <-
      #          ggplot2::ggplot(LIDAR_sousplot) +
      #          ggrepel::geom_label_repel(ggplot2::aes(label = id,x=XAbs, y=YAbs, col = where, fill = what), size = 5) +
      #          ggplot2::scale_color_manual(values = c('in' = 'black', 'out' = 'red', 'adjacent' = 'orange')) +
      #          ggplot2::scale_fill_manual(values = c('tree' = 'lightgreen', 'jalon' = 'white')) +
      #          ggplot2::ggtitle(sousplot) +
      #          ggplot2::theme_classic() +
      #          ggplot2::theme(
      #             panel.grid.major = element_line(colour = "black"),
      #             panel.grid.minor  = element_line(colour = "white", linetype = "dotdash"),
      #             panel.background = element_rect(fill = "black")) +
      #          ggplot2::ggtitle(paste0('REPROJECTION FOR THE FILE :   ', unique(LIDAR_sousplot$file)))
      #
      #       grDevices::png(paste0(directory,'/',plot_name,"/xy_reproject_method2", '/REPROJECT_', stringr::str_remove(unique(LIDAR_sousplot$file),'.csv'),'.png'), width = 600)
      #       print(my_plot)
      #       grDevices::dev.off()
      #
      #
      #
      #       LIDAR_sousplot <-
      #          LIDAR_sousplot %>%
      #          dplyr::mutate(
      #             plot = plot_name,
      #             method = 'tree') %>%
      #          dplyr::mutate(., n_jalon_ref = length(unique(jalon_ref$id)) ) %>%
      #          dplyr::mutate(., n_tree_ref = length(unique(tree_ref$id)) ) %>%
      #          dplyr::mutate(n_tot_ref = n_jalon_ref + n_tree_ref ) %>%
      #          dplyr::select(., -ref)
      #
      #
      #       assign(  paste("Reproj_XY_jalon", unique(LIDAR_sousplot$file), sep = "_"), LIDAR_sousplot )
      #
      #    }
      #
      #    list_of_objects <- mget(ls(pattern="Reproj_XY_tree"))
      #    all_tree_method <- do.call("rbind", list_of_objects)
      #
      #    all <- rbind(all_jalon_method, all_tree_method)
      #
      #
      # }
      # else{

         all <- all_jalon_method

      # }

      dup.id <-
         all %>%
         dplyr::filter(duplicated(id)) %>% .[['id']]

      all = all %>% dplyr::mutate(duplicated_id = dplyr::if_else(id %in% dup.id & what == 'tree', 'yes','no'))

      writexl::write_xlsx(all,path = paste0(directory,'/',plot_name,'/','all_reprojection.xlsx'))
      return(all)

   }

#' xy_20_100
#' @export
#' @import stringr
xy_20_100 = function(id = NULL,x = NULL, y = NULL, subplot = NULL, reverse = FALSE){

   if(reverse){

      subplot_x = stringr::str_split(subplot, '_', simplify = T)[,1] %>% as.numeric()
      subplot_y = stringr::str_split(subplot, '_', simplify = T)[,2] %>% as.numeric()

      x = x - subplot_x
      y = y - subplot_y

      data = data.frame(
         id = id,
         x = x,
         y =y,
         subplot = subplot
      )

   }

   if(!reverse){

      subplot_x = stringr::str_split(subplot, '_', simplify = T)[,1] %>% as.numeric()
      subplot_y = stringr::str_split(subplot, '_', simplify = T)[,2] %>% as.numeric()

      x = x + subplot_x
      y = y + subplot_y

      data = data.frame(
         id = id,
         x = x,
         y =y,
         subplot = subplot
      )


   }
   return(data)

}
