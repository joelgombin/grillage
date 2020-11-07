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
  shape <- sf::st_collection_extract(shape, "POLYGON")
  return(shape)
}

#' Get polling stations shapes from the voters register
#'
#' @param path path to the voters register (preferably in the REU format)
#' @param code_insee code INSEE of the commune
#' @param n a vector, number of cells used (rows and columns)
#' @param confidence_level threshold under which geolocalisation is not to be trusted
#' @param numero_bv column containing polling station code/number
#' @param numero_voie column containing the number in the street
#' @param libelle_voie column containing the name of the street
#' @param code_postal column containing the zip code
#' @param ville column containing the name of the city
#'
#' @return an sf object containing the (estimated) shape of the polling stations
#' @export
#'

gr_list_to_bv <- function(path, code_insee, n = c(50, 50), confidence_level = 0.6,
                        numero_bv = `numéro du bureau de vote`,
                        numero_voie = NumeroVoie,
                        libelle_voie = LibelleVoie,
                        code_postal = `Code postal`,
                        ville = VilleLocalite) {
  if (stringr::str_ends(path, coll(".csv"))) {
    listes <- readr::read_csv2(path)
  } else if (str_ends(path, coll(".xls"))) {
    listes <- readxl::read_xls(path)
  } else if (str_ends(path, coll(".xlsx"))) {
    listes <- readxl::read_xlsx(path)
  }

  listes <- listes %>%
    select({{numero_bv}}, {{numero_voie}}, {{libelle_voie}}, {{code_postal}}, {{ville}}) %>%
    mutate({{code_postal}} := str_pad({{code_postal}}, 5, "left", "0")) %>%
    distinct()

  listes <- listes %>%
    mutate({{libelle_voie}} := str_replace({{libelle_voie}}, "BD ", "Boulevard "),
           {{libelle_voie}} := str_replace({{libelle_voie}}, "R ", "Rue "),
           {{libelle_voie}} := str_replace({{libelle_voie}}, "AV ", "Avenue "),
           {{libelle_voie}} := str_replace({{libelle_voie}}, "CHE ", "Chemin "),
           {{libelle_voie}} := str_replace({{libelle_voie}}, "IMP ", "Impasse ")) %>%
    mutate(adresse = paste0({{numero_voie}}, " ", {{libelle_voie}}),
           code_insee = code_insee) %>%
    banR::geocode_tbl(adresse, code_insee) %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(4326)

  adresses <- listes %>%
    filter(result_score >= confidence_level) %>%
    distinct(result_id, .keep_all = TRUE)

  bv <- adresses %>%
    grillage::gr_create_shape({{numero_bv}}, n = n, code_insee = code_insee) %>%
    mutate(bureau_vote_id = paste0(code_insee, stringr::str_pad(polygon, width = 4, pad = "0", side = "left")))

  return(bv)
}
