# OSM demonstration application for eddy consensus curation
#
# date: 2024-01-14
#
# author: Roderick Bakker
#

# data formatting and subsets are good now
# tracks highlight responds to main date filter, not a massive issue, but not great
# navpanel could be nice for anomalies?


# 1: libraries ---------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(ggforce)
library(metR)
library(shinydashboard)
library(shinyWidgets)
library(gitlink)

# 2: import data -------------------------------------------------------------

data_names <- list.files( # import file paths
  path = "./data_subset/",
  #path = "./Github/shiny_application/Ed_Edd_n_Eddies_Demo/data_subset/", # only for local checks
  pattern = ".*.csv$",
  full.names = TRUE, 
  recursive = TRUE
)

# 3: format data -------------------------------------------------------------

data_files <- data_names %>% 
  map(.f = read_csv, show_col_types = FALSE) %>% # import all csv files
  set_names(nm = c("profiles", "profiles_mean", "eddy_tracks", "anomalies", "map_coords")) %>% # set names on each file 
  map_if( # if date column is present, format date column as dates
    .p = ~ "date" %in% names(.x),
    .f = ~ mutate(.x, date = as_date(date))
  ) %>% 
  map_if( # if abbreviated month column is present, set as factors
    .p = ~ "month" %in% names(.x), # different from main version
    .f = ~ mutate(.x, month = factor(month, levels = month.abb))
  ) %>% 
  map_if( # if eddy tracks column is present, format eddy tracks as factors
    .p = ~ "track" %in% names(.x),
    .f = ~ mutate(.x, track = as_factor(track))
  )

# 4: set plotting parameters -------------------------------------------------

# set variable levels for selection input to CTD plots
variables <- c("temperature", "salinity", "density", "oxygen")

# set station coordinates for plotting alongside eddies
station_coordinates_bats <- tibble( # BATS coordinates
  longitude = -64.2, latitude = 31.7
)

station_coordinates_hydro <- tibble( # Hydrostation coordinates
  longitude = -64.5, latitude = 32.166
)

# set shapes for months
shapes_months <- c(16, 15, 4, 17, 18, 8, 16, 15, 4, 17, 18, 8) 
names(shapes_months) = month.abb

# set shapes for data types
shapes_types <- c(15, 17, 16, 18)
names(shapes_types) = c("min", "max", "median", "mean")

# set min range for date selection as minimum date of all datasets
min_date <- data_files %>%
  map_if(.p = ~ "date" %in% names(.x), .f = ~ min(.x$date, na.rm = TRUE)) %>% 
  map_df(~ if(is.Date(.x)) .x) %>% 
  pivot_longer(cols = everything()) %>% 
  summarise(min = min(value)) 

# set max range for date selection as maximum date of all datasets
max_date <- data_files %>%
  map_if(.p = ~ "date" %in% names(.x), .f = ~ max(.x$date, na.rm = TRUE)) %>% 
  map_df(~ if(is.Date(.x)) .x) %>% 
  pivot_longer(cols = everything()) %>% 
  summarise(max = max(value))


# 5: define UI V1 ---------------------------------------------------------------

ui <- {page_sidebar( # sidebar and main page
  
  ribbon_css("https://github.com/rjbakker96/historical_eddy_identification"),
  
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  title = "Ed Edd 'N Eddy Demo",
  fillable = FALSE, 
  
  # 5a sidebar -----------------------------------------------------------------
  
  sidebar = sidebar( # set sidebar controls 
    title = "Controls", bg = "gray95", fg = "gray95",
    open = TRUE,
    
    sliderInput( # slider for dates filter
      inputId = "filter_dates",
      label = "Date",
      min = min_date$min, max = max_date$max,
      value = c(ymd("2008-06-01"), ymd("2008-08-31")), 
      timeFormat = "%b",
      step = 29
    ),
    sliderInput( # slider for depth filter
      inputId = "filter_depth",
      label = "Depth",
      min = 0, max = 1500,
      value = c(100, 1000), 
      step = 10,
      post = " (m)"
    ), 
    actionButton( # refresh button to apply new filter settings
      inputId = "refresh_all",
      label = "Refresh"
    )
  ), # end of sidebar
  
  # 5b main page ---------------------------------------------------------------
  
  card( # eddy tracks
    card_header("Eddy Tracks"), 
    full_screen = TRUE, max_height = "800px",
    plotOutput( 
      height = "600px",
      outputId = "eddy_tracks"
    ),
    layout_columns( # set plot controls 
      col_widths = c(2, 6), 
      fill = FALSE,
      switchInput( # legend toggle
        inputId = "toggle_eddy_legends",
        label = "L.",
        value = FALSE,
      ),
      uiOutput(
        "filter_date_tracks"
      )
    ) # end of layout_columns
  ), # end of card 1
  card( # profiles and anomalies
    # allow to swap between profiles and anomalies,
    # render both plots initially, only have the switch change with one is displayed
    card_header("Profiles & Anomalies"),
    full_screen = TRUE,
    plotOutput( 
      height = "600px",
      outputId = "depth_profiles"
    ),
    layout_columns( 
      col_widths = c(2, 3, 7),
      fill = FALSE,
      switchInput( # legend toggle
        inputId = "toggle_profile_legends", 
        label = "L.",
        value = FALSE),
      # turn into a selecter box with preset values
      checkboxGroupButtons(
        inputId = "profile_variables",
        label = NULL,
        choices = variables,
        selected = c("salinity", "temperature")
      ),
      uiOutput( # depth filter
        "depths_profiles"
      )
    )
  ),
  card( # contour plots
    card_header("Contours"),
    full_screen = TRUE,
    plotOutput(
      height = "600px",
      outputId = "depth_contours"
    ),
    layout_columns( 
      col_widths = c(2, 3, 7),
      fill = FALSE,
      switchInput( # legend toggle
        inputId = "toggle_contour_legends", 
        label = "L.",
        value = FALSE),
      # turn into selector box as in previous card
      radioButtons(
        inputId = "contour_variable",
        label = NULL, 
        choices = variables,
        selected = "temperature", 
        inline = TRUE
      ),
      uiOutput( # depth filter
        "depths_contours"
      )
    )
  )
)
} # end of sidebar and page

# 6: function calls ----------------------------------------------------------

eddy_tracks <- function(.tracks, 
                        .highlight = NULL,
                        .maps = NULL, 
                        .coordinates = NULL,
                        .station_bats, .station_hydro,
                        .legend = TRUE){
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  # maybe add filter for only tracks which were around during the highlight?
  
  .tracks <- .tracks %>% # only show days prior to the highlight
    filter(date <= .highlight)
  
  .radius <- .tracks %>% # for plotting highlight radius
    filter(date == .highlight)
  
  .tracks %>% # extract eddy data from list
    arrange(date) %>% # arrange data chronologically for line drawing
    ggplot(aes(x = longitude, y = latitude))+
    # add text label along Bermuda north shore?
    # add fixed point representing BATS and Hydro sites
    geom_text( # BATS site
      data = .station_bats,
      aes(x = longitude, y = latitude),
      label = "BATS",
      size = 6, alpha = 0.8
    )+
    geom_text( # Hydro-S site
      data = .station_hydro,
      aes(x = longitude, y = latitude),
      label = "Hydro-S",
      size = 5, alpha = 0.8
    )+
    # add fixed map representing Bermuda
    geom_polygon(data = .maps, aes(x = longitude, y = latitude),
                 alpha = 0.5, linewidth = 0.5, 
                 colour = "black", fill = "hotpink")+
    # add text label for Bermuda
    geom_text(data = tibble(longitude = -64.78, latitude = 32.42),
              aes(x = longitude, y = latitude),
              label = "Bermuda",
              size = 4, angle = 11)+
    # add eddy data points and draw lines through them
    geom_point(aes(colour = type, shape = type), 
               size = 2, alpha = 0.3)+
    geom_path(aes(linetype = track),
              colour = "black")+
    # add points with radius for highlighted day
    geom_point(data = .radius,
               aes(colour = type, shape = type), 
               size = 3, alpha = 0.9)+
    geom_circle(data = .radius,
                aes(x0 = longitude, y0 = latitude, r = effective_radius, 
                    colour = type, linetype = track), 
                alpha = 0.001)+
    # add cast coordinates
    geom_point(data = .radius,
               aes(x = long_profile, y = lat_profile), 
               shape = 13, size = 5, colour = "hotpink", alpha = 0.8)+
    # add aesthetics 
    scale_colour_manual(values = c("anticyclonic" = "red",
                                   "cyclonic" = "blue"))+ # eddy colours
    scale_shape_manual(values = c("anticyclonic" = 6, 
                                  "cyclonic" = 2))+ # eddy shapes
    ylab("Latitude")+
    xlab("Longitude")+
    labs(linetype = "Eddy ID",
         colour = "Eddy Type",
         shape = "Eddy Type")+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = .legend, # apply legend position
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 13),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
}


# plot faceted variables from profiles against depth together with mean lines

profiles_plot <- function(.profiles = NULL, .means = NULL,
                          .vars, .depths,
                          .legend = TRUE){
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  # filter on secondary depth settings
  .profiles <- .profiles %>% 
    filter(between(depth, .depths[1], .depths[2]))
  
  .means <- .means %>% 
    filter(between(depth, .depths[1], .depths[2]))
  
  # filter mean data on months present in profiles
  # months not showing up properly in plots, needs work!!
  .means <- .means %>% 
    filter(month %in% unique(.profiles$month)) 
  
  # pivot on selected variables according to drop down list
  .profiles <- .profiles %>% 
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values") %>%
    # join cruise and cast for unique daily column
    unite(col = "cast", c(cruise, cast)) %>% 
    # arrange for line drawing
    arrange(cast, date, depth)
  
  .means <- .means %>% 
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values")
  
  # generate facets plot
  .profiles %>% 
    # x and y are flipped downstream, so for naming purposes x = y and y = x
    ggplot(aes(x = depth, y = values))+
    geom_line(data = .means, # add monthly mean lines
              colour = "black", alpha = 0.7, 
              linewidth = 1, linetype = 2)+
    geom_path(aes(group = cast, colour = day), # cast specific lines
              alpha = 0.9)+
    facet_grid(cols = vars(vars), # facet on variables and months
               rows = vars(month),
               scales = "free")+
    scale_x_reverse()+ # remember x & y are flipped, so x will become y later on
    coord_flip()+ # flip x and y axis to facilitate line drawings and calculations
    scale_colour_gradientn(colours = rev(rainbow(7)),
                           limits = c(1, 31))+
    # scale_colour_discrete()+
    #labs(title = paste(as.character(unique(.profiles$year)), "Depth", "Profiles"))+ # use years in filtered data for plot title
    xlab(Depth ~ (m))+ # remember y and x-axis are flipped
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 13),
          legend.position = .legend,
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 13),
          axis.title.x = element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
}

# plot contours for a single variable over time

contours_plot <- function(.profiles, 
                          .depths, .vars, 
                          .legend = TRUE,
                          .bins = 10){
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  # format and filter according to secondary depth settings
  my_data_plot <- .profiles %>% 
    mutate(date = as.POSIXct(date)) %>% # set date as posixct element for plotting
    filter(between(depth, .depths[1], .depths[2]))
  
  # pivot on single variable for formatting (might change this somehow)
  my_data_plot <- my_data_plot %>% 
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values")
  
  # extract cruise ids (casts) where min depth <= depth setting
  my_data_cruises <- my_data_plot %>%
    select(cruise, cast, depth) %>%
    group_by(cruise, cast) %>%
    # depth is formatted as negative value, so using min value = max depth
    summarize(max = max(depth, na.rm = TRUE)) %>%
    # set depth 100 lower as a buffering capacity to not exclude excessive amounts of data.
    filter(max >= (.depths[2] - 100)) %>%
    ungroup() %>% 
    select(cruise, cast)
  
  # filter for casts where max depth >= depth setting
  my_data_plot <- my_data_plot %>%
    semi_join(my_data_cruises,
              by = c("cruise", "cast"))
  
  # depth values are negative, so max = min and min = max
  depth_range <- range(my_data_plot$depth, na.rm = TRUE)
  
  # construct breaks for depth lines every hundred meters, 
  # out of bound values are dropped by the ylim argument in the plot
  .depth_breaks <- seq(round(depth_range[1], -2), 
                       depth_range[2],
                       100)
  
  # extract min max values from selected variable
  legend_range <- range(my_data_plot$values, na.rm = TRUE)
  
  # construct breaks for the legend and round to a single digit
  .breaks <- round(seq(legend_range[1],
                       legend_range[2],
                       length.out = .bins), 1)
  
  # set binwidth for colour scale
  .binwidth <- round(
    (legend_range[2] - legend_range[1]) / (.bins * 2), 1)
  
  # generate plot
  my_data_plot %>% 
    ggplot(aes(x = date, y = depth, z = values))+
    # set lines for when bats sampling occurred
    geom_vline(aes(xintercept = date), 
               linewidth = 0.2)+
    # set colour contours
    geom_contour_fill(aes(fill = after_stat(level_mid)),
                      bins = .bins,
                      breaks = MakeBreaks(binwidth = .binwidth),
                      alpha = 0.85)+
    # set lines to match colour contours
    geom_contour(colour = "black", linetype = 2, alpha = 0.5,
                 bins = .bins, 
                 breaks = MakeBreaks(binwidth = .binwidth))+
    # set text labels to match contour lines
    geom_text_contour(breaks = MakeBreaks(binwidth = .binwidth))+
    # set lines every hundred meters to help identify shifts
    geom_hline(yintercept = .depth_breaks,
               linewidth = 0.2, linetype = 2, alpha = 0.8)+
    # plot min and max lines as plot borders
    geom_hline(yintercept = c(depth_range[1], depth_range[2]))+
    scale_x_datetime(date_breaks = "month", date_labels = "%b",
                     expand = c(0.01, 0.01))+
    scale_fill_gradientn(colours = rev(rainbow(7)))+
    scale_y_reverse(expand = c(0.01, 0.01))+ # reverse y-axis to plot depth
    #labs(title = paste(as.character(unique(my_data_plot$year)), as.character(.vars), "Contours"))+ # paste unique years and selected variable as title
    xlab("Month")+
    ylab(Depth ~ (m))+
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 13),
          legend.position = .legend,
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          strip.text = element_text(size = 13),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
  
}



# 7: define server -----------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent({input$refresh_all | input$dummy_start},{
  
  # 7a: filter data based on dashboard settings ---------------------------------
  
  date_filter <- interval( # set initial time filter values
    start = floor_date(input$filter_dates[1], unit = "month"), # first day of month
    # last day of month (first day of month - 1 day)
    end = (ceiling_date(input$filter_dates[2], unit = "month") - days(x = 1))
    )
  
  depth_filter <- c(input$filter_depth[1], # set initial depth filter values
                    input$filter_depth[2])
  
  data_filt <- data_files %>% 
    map_if( # time filter
      .p = ~ "date" %in% names(.x),
      .f = filter, date %within% date_filter
    ) %>%
    map_if( # depth filter
      .p = ~ "depth" %in% names(.x),
      .f = filter, between(depth,
                           depth_filter[1],
                           depth_filter[2])
    ) # end of filters
  
  # 7b: set server items for UI rendering ----------------------------------------------
  
  ## set items to be rendered in UI for secondary depth filters
  
  # date range for eddy tracks highlighting
  output$filter_date_tracks <- renderUI({
    sliderInput( # slider controls
      inputId = "filter_date_tracks",
      label = NULL,
      width = "100%",
      min = floor_date(input$filter_dates[1], unit = "month"),
      max = (ceiling_date(input$filter_dates[2], unit = "month") - days(x = 1)),
      value = (floor_date(input$filter_dates[1], unit = "month") + days(20)), 
      timeFormat = "%b-%d"
    )
  })
  
  # depth range for CTD depth & density plots
  output$depths_profiles <- renderUI({
    sliderInput( # slider controls
      inputId = "depths_profiles",
      label = NULL,
      width = "100%",
      post = " (m)",
      min = input$filter_depth[1],
      max = input$filter_depth[2],
      value = c(input$filter_depth[1], input$filter_depth[2]),
      step = 10)
  })
  
  # depth range for contour plot
  output$depths_contours <- renderUI({
    sliderInput( # slider controls
      inputId = "depths_contours",
      label = NULL,
      width = "100%",
      post = " (m)",
      min = input$filter_depth[1],
      max = input$filter_depth[2],
      value = c(input$filter_depth[1], input$filter_depth[2]),
      step = 10)
    
  })
  
  # 7c: build figures -------------------------------------------------------
 
  output$eddy_tracks <- renderPlot({ # eddy tracks 
    eddy_tracks(.tracks = data_filt$eddy_tracks,
                .highlight = input$filter_date_tracks,
                .maps = data_filt$map_coords,
                .station_bats = station_coordinates_bats,
                .station_hydro = station_coordinates_hydro,
                .legend = input$toggle_eddy_legends)
    
  })  
  
  output$depth_profiles <- renderPlot({ # profiles
    profiles_plot(.profiles = data_filt$profiles,
                  .means = data_filt$profiles_mean,
                  .vars = input$profile_variables,
                  .depths = input$depths_profiles,
                  .legend = input$toggle_profile_legends)
    
  })
  
  output$depth_contours <- renderPlot({ # contours
    contours_plot(.profiles = data_filt$profiles,
                  .vars = input$contour_variable,
                  .depths = input$depths_contours,
                  .legend = input$toggle_contour_legends,
                  .bins = 10)
    
  })
  
  }
  ) # end of observeEvent
  
   
} # end of server

# 8: run application ---------------------------------------------------------

# run the application 
shinyApp(ui = ui, server = server)
