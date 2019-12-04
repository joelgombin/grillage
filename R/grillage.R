#' Create shape
#'
#' @param points an `sf` object with points
#' @param id the name of the column holding the IDs of the shapes
#' @param n a vector, number of cells used (rows and columns)
#'
#' @return an `sf` object with polygons
#' @export
#'
#' @import rlang
#' @import dplyr
#' @import sf
gr_create_shape <- function(points, id, n = c(150, 150), code_insee = NULL) {

  # get the convex hull of the points
  if (is.null(code_insee)) {
    enveloppe <- sf::st_sf(sf::st_convex_hull(sf::st_union(points)))
  } else {
    enveloppe <- gr_get_enveloppe(code_insee)
  }

  # make a grid in this enveloppe
  grid <- sf::st_make_grid(enveloppe, n = n)
  grid <- sf::st_intersection(enveloppe, grid)
  content <- sf::st_contains(grid, points)

  grid$polygon <- purrr::map_chr(content,
                            ~ dplyr::if_else(rlang::is_empty(.x),
                                      NA_character_,
                                      points %>%
                                        slice(.x) %>%
                                        pull({{id}}) %>%
                                        table %>%
                                        sort(decreasing = TRUE) %>%
                                        names %>%
                                        .[1])
  )

  # now converting neighbours

  grid_iteration1 <- grid
  grid_iteration1$intersections <- unclass(sf::st_intersects(grid))
  grid_iteration1 <- grid_iteration1 %>%
    mutate(polygon = dplyr::if_else(!is.na(polygon),
                            polygon,
                            purrr::map_chr(intersections,
                                    function(x) {
                                      df <- grid %>%
                                        slice(x) %>%
                                        filter(!is.na(polygon))
                                      if (nrow(df) %in% 0) {
                                        return(NA_character_)
                                      }
                                      df %>%
                                        pull(polygon) %>%
                                        table %>%
                                        sort(decreasing = TRUE) %>%
                                        names %>%
                                        .[1]
                                    }
                            )
    ))

  grid_valid <- grid_iteration1 %>%
    filter(!is.na(polygon)) %>%
    group_by(polygon) %>%
    summarise()

  return(grid_valid)
}

#' Utilitary function allowing to get the shape of a commune given its code Insee
gr_get_enveloppe <- function(code_insee) {
  correspondance_table <- read.csv("https://gist.githubusercontent.com/Marc-marc-marc/07b0d74e63be682505b49488221b1bb9/raw/d1ec095657796a27bd37efc8e378e1836acdd0a5/gistfile1.txt", sep = "\t", col.names = c("id", "insee"), stringsAsFactors = FALSE)
  id <- correspondance_table[correspondance_table$insee %in% code_insee, "id"]
  shape <- sf::st_read(glue::glue("http://polygons.openstreetmap.fr/get_geojson.py?id={id}&params=0"))
  return(shape)
}
