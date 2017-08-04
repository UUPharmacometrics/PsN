read_nm_tables <- function(files         = NULL,
                           combined      = TRUE,
                           rm_duplicates = TRUE,
                           quiet         = TRUE,
                           simtab        = NULL,
                           ...) {
  
  if (!is.null(files) && !is.nm.table.list(files)) {
    files <- dplyr::tibble(problem   = 1, 
                           file      = files,
                           firstonly = FALSE,
                           simtab    = FALSE)
  }
  
  user_mode <- !is.nm.table.list(files)
  
  # Filter tables if needed
  if (!is.null(simtab)) files <- files[files$simtab == simtab, ]
  msg(c('\nLooking for nonmem', dplyr::if_else(!is.null(simtab) && simtab, 
                                               ' simulation', ' output'), ' tables'), quiet)
  
  # Check that file exists
  if (is.null(files) || !any(file.exists(files$file))) {
    msg('No table file could be found.', quiet)
    return()
  }
  
  if (any(duplicated(files$file))) {
    msg('No tables imported due to duplicated names', quiet)
    return()
  }
  
  tables <- files[file.exists(files$file), ]
  
  # Print reading messages
  tables %>% 
    dplyr::mutate(grouping = 1:n(),
                  name = stringr::str_c(basename(.$file), dplyr::if_else(.$firstonly, ' (firstonly)', ''))) %>% 
    dplyr::group_by_(.dots = 'problem') %>% 
    tidyr::nest() %>% 
    dplyr::mutate(string = purrr::map_chr(.$data, ~stringr::str_c(.$name, collapse = ', '))) %>% 
    {stringr::str_c(.$string, ' [$prob no.', .$problem, ']', collapse = '\n         ')} %>% 
    {msg(c('Reading: ', .), quiet)}
  
  # Collect options for table import
  tables <- tables %>% 
    dplyr::mutate(top = purrr::map(.$file, ~readr::read_lines(file = ., n_max = 3)),
                  grouping = 1:n()) %>% 
    dplyr::group_by_(.dots = 'grouping') %>% 
    tidyr::nest() %>% 
    dplyr::mutate(args = purrr::map(.$data, ~read_args(x = . , quiet, ...))) %>% 
    tidyr::unnest_(unnest_cols = 'data') %>% 
    tidyr::unnest_(unnest_cols = 'args') %>% 
    dplyr::mutate(name = basename(.$file)) %>% 
    dplyr::select(dplyr::one_of('problem', 'name', 'simtab', 'firstonly', 'fun', 'params'))
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
  }
  
  # Read in data
  tables <- tables %>% 
    dplyr::bind_cols(tables %>% 
                       dplyr::select(dplyr::one_of(c('fun', 'params'))) %>% 
                       {purrr::invoke_map(.$fun, .$params)} %>%
                       dplyr::tibble(data = .))
  
  if (!combined) {
    return(purrr::set_names(x = purrr::map(tables$data, ~tidyr::drop_na(., dplyr::one_of('ID'))),
                            nm = tables$name))
  }
  
  # Index datasets
  tables <- dplyr::bind_cols(tables,
                             dplyr::tibble(index = purrr::map(tables$data, colnames) %>% 
                                             purrr::set_names(tables$name),
                                           nrow  = purrr::map_dbl(tables$data, nrow)))
  
  # Combine tables with same number of rows
  tables <- tables %>% 
    dplyr::group_by_(.dots = c('problem', 'simtab', 'firstonly')) %>% 
    tidyr::nest(.key = 'tmp') %>% 
    dplyr::mutate(out = purrr::map(.$tmp, combine_tables, quiet)) %>% 
    tidyr::unnest_(unnest_cols = 'out') %>% 
    dplyr::select(dplyr::one_of('problem', 'simtab', 'firstonly', 'data', 'index'))
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
  }
  
  # Merge firsonly tables with main tables
  if (any(tables$firstonly)) {
    msg('Consolidating tables with `firstonly`', quiet)
    tables <- tables %>%
      dplyr::group_by_(.dots = c('problem', 'simtab')) %>%
      tidyr::nest(.key = 'tmp') %>% 
      dplyr::mutate(out = purrr::map(.$tmp, merge_firstonly, quiet)) %>% 
      tidyr::unnest_(unnest_cols = 'out') %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'data', 'index'))
  }
  
  if (nrow(tables) == 0) {
    msg('\nNo table imported.', quiet)
    return() 
  }
  
  # Remove duplicated columns to decrease xpdb size
  if (rm_duplicates) {
    tables <- tables %>% 
      dplyr::mutate(grouping = 1:n()) %>% 
      dplyr::group_by_(.dots = 'grouping') %>% 
      tidyr::nest(.key = 'tmp') %>% 
      dplyr::mutate(out = purrr::map(.$tmp, ~dplyr::select(.$data[[1]], 
                                                           dplyr::one_of(unique(unlist(.$index[[1]]$colnames)))))) %>% 
      tidyr::unnest_(unnest_cols = 'tmp') %>% 
      dplyr::select(dplyr::one_of('problem', 'simtab', 'index', 'out')) %>% 
      dplyr::rename_(.dots = c('data' = 'out'))
  }
  
  # If user mode return simple tibble as only 1 problem should be used
  if (user_mode) return(tables$data[[1]])
  
  tables
}


read_funs <- function(fun) {
  c(csv = readr::read_csv,
    csv2 = readr::read_csv2,
    table = readr::read_table,
    table2 = readr::read_table2)[fun]
}

read_args <- function(x, quiet, col_types, ...) {
  if (missing(col_types)) col_types <- readr::cols(.default = 'd')
  
  top <- x$top[[1]]
  
  if (is.na(top[3]) || !stringr::str_detect(top[3], '\\d+E[+-]\\d+\\s*')) {
    msg(c('Dropped: ', basename(x$file), ' due to unexpected data format'), quiet)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  fun <- dplyr::case_when(stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s*;') ~ 'csv2',
                          stringr::str_detect(top[3], '\\d.\\d+E[+-]\\d+\\s*,') ~ 'csv', 
                          stringr::str_detect(top[3], '\\d,\\d+E[+-]\\d+\\s+') ~ 'table2',
                          TRUE ~ 'table')
  skip <- dplyr::if_else(stringr::str_detect(top[1], 'TABLE NO\\.\\s+\\d'), 1, 0)
  
  if (!stringr::str_detect(top[1 + skip], '[A-z]{2,}+')) {
    msg(c('Dropped: ', basename(x$file), ' due to missing headers.'), quiet = quiet)
    return(dplyr::tibble(fun = list(), params = list()))
  }
  
  col_names <- top[1 + skip] %>% 
    stringr::str_trim(side = 'both') %>% 
    stringr::str_split(pattern = dplyr::case_when(fun == 'csv' ~ ',', 
                                                  fun == 'csv2' ~ ';',
                                                  fun %in% c('table', 'table2') ~ '\\s+')) %>% 
    purrr::flatten_chr()
  
  dplyr::tibble(fun = read_funs(fun),
                params = list(list(file = x$file, skip = skip + 1,
                                   col_names = col_names, col_types = col_types, ...)))
}

combine_tables <- function(x, quiet) {
  if (length(unique(x$nrow)) > 1) {
    msg(c('Dropped: ', stringr::str_c(x$name, collapse = ', '), 
          ' due to missmatch in row number.'), quiet)
    return(dplyr::tibble(data = list(), index = list()))
    
  }
  dplyr::tibble(data = x$data %>%
                  dplyr::bind_cols() %>%
                  purrr::set_names(make.unique(names(.))) %>%
                  tidyr::drop_na(dplyr::one_of('ID')) %>%
                  list(),
                
                # Get around purrr droping list names
                index = list(dplyr::tibble(tables = x$name, 
                                           colnames = x$index)))
}

merge_firstonly <- function(x, quiet) {
  if (nrow(x) == 1) {
    # No merge needed
    return(dplyr::tibble(data = x$data, index = x$index))
  } else if (nrow(x) != 2) {
    msg(c(' * Something went wrong while consolidating: ', 
          paste0(x[x$firstonly == TRUE, ]$index[[1]]$tables, 
                 collapse = ', ')), quiet) 
    return(dplyr::tibble(data = list(), index = list()))
  }
  xdata   <- x$data[x$firstonly == FALSE][[1]]
  ydata   <- x$data[x$firstonly == TRUE][[1]]
  by_vars <- intersect(colnames(xdata), colnames(ydata))
  msg(c(' * Joining by: ', paste0(by_vars, collapse = ', ')), quiet)
  dplyr::tibble(data = list(dplyr::left_join(x  = xdata, 
                                             y  = ydata,
                                             by = by_vars)),
                index = x$index %>% 
                  dplyr::bind_rows() %>% 
                  list())
}

# Message function with quiet option (from Ron Keizer)
msg <- function(txt, quiet = TRUE) {
  if (!quiet) message(txt)
}

# Reports whether x is a nm.table.list object
is.nm.table.list <- function(x) {
  inherits(x, 'nm_table_list')
}