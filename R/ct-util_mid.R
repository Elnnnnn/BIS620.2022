#' My Dataset
#'
#' A dataset containing information on clinical trials
#'
#' @format A data frame with 471252 rows and 13 columns:
#' @keywords studies
"studies"

#' @importFrom utils data
data("studies")

#' @title Select the clinical trials with keywords
#' @description The function is to select cilinical trials that have a keyword in a certain column
#' @param d the data that choose from
#' @param kwds keywords
#' @param column the column that the keyword is chosen from
#' @param ignore_case where ignore case is true or false
#' @param match_all whether should match all keywords
#' @export
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  # Ensure that 'kwds' is a character vector
  kwds <- as.character(kwds)
  # Remove empty strings
  kwds <- kwds[kwds != ""]

  # Create the regex pattern
  pattern <- if (match_all) {
    paste(kwds, collapse = ".*")
  } else {
    paste(kwds, collapse = "|")
  }

  # Use grepl to filter rows based on the presence of keywords
  matches <- grepl(pattern, d[[column]], ignore.case = ignore_case)

  # Return the rows where the pattern matches
  d = d[matches, ]
}



#' @title Plot a histogram of different phases
#' @description The function is to plot a histogram of phases of selected table with a fixed x-axis.
#' @param x the database table.
#' @export
#' @importFrom dplyr select distinct bind_rows group_by summarise
#' @importFrom ggplot2 ggplot geom_col theme_bw xlab ylab
plot_phase_histogram = function(x) {
  #Find all the phases in studies table
  phase_all = x |>
    select(phase) |>
    distinct()
  x = x |>
    select(phase)
  #Bind all phases to the selected table to make sure every phase will appear at least once.
  x = x |> bind_rows(phase_all)
  x$phase[is.na(x$phase)] = "NA"
  x = x|>
    group_by(phase) |>
    summarise(n = n()-1)
  #Plot the pahse histogram
  ggplot(x, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}

#' @title Plot a histogram of conditions
#' @description The function is to plot a histogram of conditions of selected studies.
#' The number of conditions is the smaller one of 50 and number of conditions with percentage>0.0001.
#' @param x the database table.
#' @return a histogram of conditions of selected studies
#' @export
#' @importFrom dplyr select group_by arrange
#' @importFrom tidyr separate_rows
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col theme_bw theme xlab ylab
plot_conditions_histogram = function(x){
  x = x |>
    select(conditions) |>
    separate_rows(conditions, sep = ", ") |>
    group_by(conditions) |>
    summarize(n = n()) |>
    filter(nchar(conditions) < 30) |>
    arrange(-n)
  #Calculate the sum of all conditions(studies)
  sum = sum(x$n)
  #Calculate the persentage of each condition
  nrows = x |>
    mutate(percentage = n/sum) |>
    filter(percentage > 0.001) |>
    nrow()
  #Let n be the smaller of 50 and number of conditions with percentage>0.0001.
  n = min(50,nrows)
  #Select top n in x
  x = x |>
    head(n)
  #Plot the conditions histgram
  ggplot(x, aes(x = conditions, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Conditions") +
    ylab("Count")
}

#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
#' @export
#' @importFrom purrr map_dbl
#' @importFrom dplyr mutate across where
get_concurrent_trials = function(d) {
  # Get all of the unique dates.

  d <- d %>%
    mutate(across(where(~ inherits(., "Date")), as.character))

  unique_dates <- unique(c(d$start_date, d$completion_date))
  unique_dates <- unique_dates[!is.na(unique_dates)] # Remove NA values
  unique_dates <- sort(unique_dates) # Sort the dates

  # Initialize a data frame to store the counts for each date
  all_dates <- data.frame(date = unique_dates, count = integer(length(unique_dates)))

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}

#' @title Get matrix of terms
#' @description The function is to get the keywords frequency of selected studies.
#' @param x the studies to get the number of keywords for.
#' @return a matrix containing words existed in the keyword column of x and their frequency.
#' @export
#' @importFrom memoise memoise
#' @importFrom tm tm_map TermDocumentMatrix Corpus content_transformer VectorSource stopwords
#' @importFrom dplyr select collect
getTermMatrix <- memoise(function(x) {

  x = x |> collect() |> select(keyword) |> as.character()
  myCorpus = Corpus(VectorSource(x))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but","study","patients"))

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
})

#' @title Plot Countries
#' @description The function is to get the number of studies in different countries
#' and visualize in a world map.
#' @param x the studies to get the world map for.
#' @export
#' @importFrom dplyr select group_by summarise
#' @importFrom rworldmap joinCountryData2Map mapCountryData
plot_countries = function(x){
  x = x |> collect() |>
    select(country) |>
    group_by(country) |>
    summarise(n = n()) |>
    as.data.frame()
  spdf <- joinCountryData2Map(x, joinCode="NAME", nameJoinColumn="country")

  max_value <- max(spdf$n, na.rm = TRUE)

  sep_num = min(10, max_value)

  # Create breaks from 10% up to 100% of the maximum value
  breaks <- seq(max_value/sep_num, max_value, by = max_value/sep_num)

  # Add 0 at the beginning of the breaks and ensure the max value is included
  breaks <- c(0, breaks)

  # Use these breaks in mapCountryData
  mapCountryData(spdf, nameColumnToPlot = "n",  catMethod = breaks,
                 colourPalette = "white2Black", mapTitle = "Number of Studies Worldwide")

#  mapCountryData(spdf, nameColumnToPlot="n", catMethod="fixedWidth",colourPalette = "white2Black", mapTitle = "Number of Studies Worldwide" )
}

#' @title Table with Links
#' @description The function is to get a table of selected studies'
#' nct_id(with link),brief title, start date, and completion date.
#' @param x the studies to get the table.
#' @return table containing nct_id(with link),brief title, start date, and completion date.
#' @export
#' @importFrom dplyr collect mutate case_when select rename
table_with_link = function(x){
  x = x |> collect()
  x$link <- paste0("<a href='", x$url, "' target='_blank'>", x$nct_id, "</a>")
  x = x |> as.data.frame() |>
    mutate(link = case_when(
      is.na(url) ~ nct_id,
      TRUE ~ link
    ))
  x |>
    select(link, brief_title, start_date, completion_date) |>
    rename(`NCT ID` = link, `Brief Title` = brief_title,
           `Start Date` = start_date, `Completion Date` = completion_date)
}

#' @title Plot states
#' @description The function is to get the number of studies in different states
#' and visualize in an US map.
#' @param x the studies to get the US map for.
#' @export
#' @importFrom dplyr collect filter select group_by summarise
#' @importFrom ggplot2 map_data ggplot geom_polygon coord_fixed scale_fill_gradient theme_minimal labs
plot_states_us = function(x){
  x = x |> collect() |>
    filter(country_facility == "United States")|>
    select(state) |>
    group_by(state) |>
    summarise(n = n()) |>
    as.data.frame()

  x$state <- tolower(x$state)
  # Get the map data
  states_map <- map_data("state")
  # Merge your data with the map data
  map_data <- merge(states_map, x, by.x = "region", by.y = "state", all.x = TRUE)

  ggplot() +
    geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = n),
                 color = "white", size = 0.1) +
    coord_fixed(1.3) +
    scale_fill_gradient(low = "white", high = "red",
                        na.value = "grey90", name = "Frequency") +
    theme_minimal() +
    labs(title = "Frequency Distribution Across U.S. States",
         caption = "Each state is color-coded based on the frequency value.")
}


#' @title  Pie Chart
#' @description The function is to get a pie chart of top 10 mesh_terms.
#' @param x the studies to get the top 10 mesh_terms for.
#' @export
#' @importFrom dplyr collect select group_by summarise filter
#' @importFrom grDevices rainbow
#' @importFrom graphics legend pie
#' @importFrom utils head
pie_chart_mesh = function(x){
  x = x |> collect() |>
    select(downcase_mesh_term) |>
    group_by(downcase_mesh_term) |>
    summarise(n = n()) |>
    filter(!is.na(downcase_mesh_term)) |>
    as.data.frame()

  # Ordering the data by the 'n' column in descending order and getting the top 10
  plot1 <- head(x[order(-x$n),], 10)
  # Define colors
  colors <- rainbow(length(plot1$n))
  # Creating the pie chart
  pie_chart <- pie(plot1$n, labels = plot1$downcase_mesh_term,
                   main = "Top 10 investigated diseases",
                   col = colors,
                   clockwise = TRUE)

  # Adding percentages
  pie_labels <- round(100 * plot1$n / sum(plot1$n), 1)
  pie_labels <- paste(pie_labels, "%", sep="")

  # Adding the legend immediately after the pie chart with corresponding colors
  legend("topright", legend = pie_labels, fill = colors, bty = "n", title = "Percentages")
}
