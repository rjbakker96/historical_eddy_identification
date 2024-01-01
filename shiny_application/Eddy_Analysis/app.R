# shiny application for the exploration of water profiles and eddy data

# Author: Roderick Bakker

# Date: 2023-09-13

# Version: 3.0

# known issues: 
# months aren't ordered chronologically for facets and factors when data spans multiple years
# TS plot gives an error when primary depth filter range is outside of the initial secondary filter settings

# 1: libraries ---------------------------------------------------------------

library(metR)
library(ggforce)
library(oce)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(bslib)


# 2: import data -------------------------------------------------------------

data_names <- list.files( # import file paths
  path = ".",
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
  map_if( # if month column is present, format month as abbreviations and set as factors
    .p = ~ "month" %in% names(.x),
    .f = ~ mutate(.x, month = factor(month.abb[month], levels = month.abb))
  ) %>% 
  map_if( # if eddy tracks column is present, format eddy tracks as factors
    .p = ~ "track" %in% names(.x),
    .f = ~ mutate(.x, track = as_factor(track))
  )

# 4: set plotting parameters -------------------------------------------------

# set variable levels for selection input to CTD plots
variables <- c("temperature", "salinity", "density", "oxygen")

# set station coordinates for plotting alongside eddies
station_coordinates <- tibble( 
  longitude = -64.2, latitude = 31.7
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

# 5: define UI -----------------------------------------------------------------

# Define UI for application
ui <- {dashboardPage(
  skin = "purple",
  
  # 5a: dashboard title and sidebar -----------------------------------------
  
  # Application tittle
  dashboardHeader(title = "Eddy Data Analysis"),
  # define items available in sidebar
  dashboardSidebar(
    sidebarMenu(
      style = "position:fixed; width:17.5%", # set width of dashboardSidebar
      menuItem("Profiles & Eddies", tabName = "data_tab_1", icon = icon("hurricane")
               ), # end of profiles tab menuItem
      menuItem("Settings", icon = icon("gears"),
               dateRangeInput( # set selection for date filter
                 inputId = "filter_dates",
                 label = "Dates",
                 min = min_date$min, max = max_date$max,
                 start = ymd("2008-05-15"), end = ymd("2008-09-15"), 
                 startview = "year"
               ),
               sliderInput( # set selection for depth filter
                 inputId = "filter_depth",
                 label = "Depth",
                 min = 0, max = 1500,
                 value = c(100, 1000)
               ),
               actionButton( # refresh button to apply new filter settings
                 inputId = "refresh_all",
                 label = "Refresh"
                 )
               ), # end of filter settings menuItem
      menuItem("Legends", icon = icon("chart-simple"),
               switchInput(inputId = "toggle_eddy_legends", 
                           label = "Eddies",
                           value = TRUE),
               switchInput(inputId = "toggle_profile_legends", 
                           label = "Profiles",
                           value = TRUE)
               ) # end of legends menuItem
    ) # end of sidebarMenu
  ), # end of dashboardSidebar
  
  # 5b: dashboard tab 1 ------------------------------------------------------
  
  # set body of dashboard 
  dashboardBody(
    # water profiles and eddy activity data analysis window
    tabItems(
      tabItem(tabName = "data_tab_1",
              theme = shinytheme("yeti"),
              # plot eddy data
              box(width = 13, collapsible = TRUE,
                box(
                  plotOutput(outputId = "eddy_tracks")
                ),
                box(
                  plotOutput(outputId = "eddy_radius")
                )
              ), # end of eddy plots
              # select profile and anomaly data for filtering of faceted plots
              box(width = 13, 
                  fluidRow(
                    column(4, align = "center", 
                           selectInput("profile_variables",
                                       "Profile Variables",
                                       choices = variables, multiple = TRUE, # use predefined variables as options
                                       selected = c("salinity", "temperature"))
                    ),
                    column(4, align = "center", # secondary depth filter from renderUI in server
                           uiOutput("depths_profiles")
                    ),
                    column(4, align = "center",
                           selectInput("anomaly_functions",
                                       "Anomaly Functions",
                                       choices = c("min", "max", "mean","median"), multiple = TRUE,
                                       selected = c("median"))
                    )
                  )
              ), # end of profiles and anomlies data selection box
              # plot profiles and anomalies along depth
              fluidRow(
                box( 
                  plotOutput(outputId = "depth_profiles")
                ),
                box(
                  plotOutput(outputId = "depth_anomalies")
                )
                ), # end of profile plots
              # select variable, depths and colour bins for profile contours plot
              box(width = 13,
                  fluidRow(
                    column(3, align = "center", # select variable for the contour plot
                           selectInput("contour_variable",
                                       "Variable",
                                       choices = variables,
                                       selected = "temperature")
                    ),
                    column(5, align = "center", # secondary depth filter from renderUI in server
                           uiOutput("depths_contours")
                    ),
                    column(4, align = "center",
                           sliderInput("colour_bins_contours",
                                       "Colour Bins",
                                       min = 1,
                                       max = 20,
                                       value = 10,
                                       step = 1)
                    )
                  )
              ), # end of profile contours control box
              # plot contours
              box(width = 13,
                  plotOutput(outputId = "depth_contours")
              ), # end of profile contours box
              # set variables for ts_isopycnals plot
              box(width = 13,
                  fluidRow(
                    column(3, align = "center",
                           checkboxInput("show_dots_ts",
                                         "Include Dots",
                                         value = FALSE),
                           
                           checkboxInput("show_lines_ts",
                                         "Include Lines",
                                         value = TRUE)
                    ),
                    column(5, align = "center", # secondary depth filter from renderUI in server
                           uiOutput("depths_ts")
                    ),
                    column(4, align = "center",
                           sliderInput("isopycnal_bins_ts",
                                       "Isopycnals",
                                       min = 2,
                                       max = 20,
                                       value = 10,
                                       step = 1)
                    )
                  ) 
              ), # end of ts_isopycnals plot data selection box
              # plot temperature and salinity along isopycnals
              box(width = 13,
                  plotOutput(outputId = "ts_isopycnals")
              ) # end of ts_isopycnals box
      ) # end of dashboard tab
    ) # end of tab items
  ) # end of dashboard body
) # end of dashboard page
      
  
  
} # end of UI

# 6: function calls ----------------------------------------------------------

# plot eddy tracks 

eddy_tracks <- function(.tracks, 
                        .maps = NULL, .station,
                        .legend = TRUE){
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  .tracks %>% # extract eddy data from list
    arrange(date) %>% # arrange data chronologically for line drawing
    ggplot(aes(x = longitude, y = latitude))+
    # add fixed point representing BATS site
    geom_point(data = .station, aes(x = longitude, y = latitude),
               shape = 13, size = 7, 
               colour = "black",
               alpha = 0.5)+
    # add fixed map representing Bermuda
    {if(!is.null(.maps)){ # only draw map if data is present
    geom_polygon(data = .maps, aes(x = longitude, y = latitude),
                 alpha = 0.5, linewidth = 0.5, 
                 colour = "black", fill = "hotpink")}}+
    # add eddy data points and draw lines through them
    geom_point(aes(colour = day, shape = month), size = 2.5)+
    geom_path(aes(linetype = track),
              colour = "black")+
    # add aesthetics 
    scale_colour_gradientn(colours = rainbow(7), 
                           limits = c(1,31))+
    scale_shape_manual(values = shapes_months)+ # shapes as set in chapter 4
    scale_y_continuous(limits = c(30.7, 32.7))+
    scale_x_continuous(limits = c(-65.2, -63.2))+
    ylab("Latitude")+
    xlab("Longitude")+
    labs(title = paste(c(unique(.tracks$year)), "Eddy", "Tracks"), # paste unique years as title
         linetype = "eddy")+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = .legend, # apply legend position
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 13),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
}

# plot the eddy tracks and their respective radius and BATS/Hydro sampling coordinates per month

eddy_radius <- function(.tracks, 
                        .cast_coords = NULL, 
                        .maps = NULL, .station,
                        .legend = TRUE){
  
  # extract sampling dates overlapping with eddy activity if data is supplies
  if(!is.null(.cast_coords)){
    coords <- .cast_coords %>% 
      select(year, month, day, latitude, longitude) %>% 
      unique() %>% 
      semi_join(.tracks,
                by = c("year", "month", "day"))
  }
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  .tracks %>% 
    #arrange(date) %>% # arrange data chronologically for line drawing
    ggplot(aes(x = longitude, y = latitude))+
    # add data points, lines and circles representing the eddies and their radius
    geom_circle(aes(x0 = longitude, y0 = latitude, r = effective_radius, 
                    colour = day, linetype = track), 
                alpha = 0.001)+
    geom_point(aes(colour = day, shape = track), 
               alpha = 0.5)+
    geom_path(aes(linetype = track),
              colour = "black")+
    # add variable points for profiles if sampling locations and dates if available
    {if(!is.null(.cast_coords)){ # only plot cast coordinates if data is present
      geom_point(data = coords, 
                 aes(x = longitude, y = latitude, colour = day), 
                 shape = 10, size = 5)}}+
    # add points where eddies overlap with profiles
    geom_point(aes(x = long_profile, y = lat_profile, colour = day), 
               shape = 10, size = 5)+
    # add fixed point representing BATS site
    geom_point(data = .station, aes(x = longitude, y = latitude),
               shape = 13, size = 4, 
               colour = "black",
               alpha = 0.5)+
    # add fixed map representing Bermuda
    {if(!is.null(.maps)){ # only draw map if data is present
      geom_polygon(data = .maps, aes(x = longitude, y = latitude),
                   alpha = 0.5, linewidth = 0.5, 
                   colour = "black", fill = "hotpink")}}+
    facet_grid(rows = vars(month), # facet by month and type of eddy
               cols = vars(type))+
    # add colour scales
    scale_colour_gradientn(colours = rainbow(7), 
                           limits = c(1,31))+
    ylab("Latitude")+
    xlab("Longitude")+
    labs(title = paste(c(unique(.tracks$year)), "Eddy", "Radius", "and", "Station", "Coordinates"), # paste unique years as title
         shape = "eddy", linetype = "eddy")+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = .legend,
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
  .means <- .means %>% 
    filter(month %in% unique(.profiles$month))
  
  # pivot on selected variables according to drop down list
  .profiles <- .profiles %>% 
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values")
  
  .means <- .means %>% 
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values")
  
  # generate facets plot
  .profiles %>% 
    # x and y are flipped downstream, so for naming purposes x = y and y = x
    ggplot(aes(x = depth, y = values))+
    geom_line(data = .means, # add monthly mean lines
              colour = "black", alpha = 0.7, linewidth = 1)+
    geom_point(aes(colour = day), # add cast specific lines
              alpha = 0.8)+
    facet_grid(cols = vars(vars), # facet on variables and months
               rows = vars(month),
               scales = "free")+
    scale_x_reverse()+ # remember x & y are flipped, so x will become y later on
    coord_flip()+ # flip x and y axis to facilitate line drawings and calculations
    scale_colour_gradientn(colours = rainbow(7),
                           limits = c(1, 31))+
    labs(title = paste(as.character(unique(.profiles$year)), "Depth", "Profiles"))+ # use years in filtered data for plot title
    xlab(Depth ~ (m))+ # remember y and x-axis are flipped
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = .legend,
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 13),
          axis.title.x = element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
}

# plot faceted variables from anomalies against depth

anomalies_plot <- function(.profiles, .anomalies,
                           .vars, .depths, .functions,
                           .legend = TRUE){
  
  # set legend position depending on dashboardSidebar toggle switch
  .legend <- if(.legend == TRUE){
    "right"
  } else ("none")
  
  # filter on secondary depth settings
  .anomalies <- .anomalies %>% 
    filter(between(depth, .depths[1], .depths[2]))
  
  # filter anomaly data on months and years present in profiles
  .anomalies <- .anomalies %>% 
    filter(month %in% unique(.profiles$month) &
             year %in% unique(.profiles$year))
  
  # filter on function selection from UI
  .anomalies <- .anomalies %>% 
    filter(func %in% .functions)
  
  # pivot on selected variables according to drop down list
  .anomalies <- .anomalies %>%
    pivot_longer(cols = all_of(.vars),
                 names_to = "vars", values_to = "values")

  # generate facets plot
  .anomalies %>% 
    # x and y are flipped downstream, so for naming purposes x = y and y = x
    ggplot(aes(x = depth, y = values))+
    geom_hline(yintercept = 0, colour = "black")+ # add line through 0 values
    geom_smooth(aes(linetype = func), # add smoothed lines through anomaly points
                colour = "hotpink", se = FALSE)+
    geom_point(colour = "black", # add anomaly value points 
               alpha = 0.8)+
    facet_grid(cols = vars(vars), # facet on variables and months
               rows = vars(month),
               scales = "free")+
    scale_x_reverse()+ # remember x & y are flipped, so x will become y later on
    coord_flip()+ # flip x and y axis to facilitate line drawings and calculations
    scale_colour_gradientn(colours = rainbow(7),
                           limits = c(1, 31))+
    labs(title = paste(as.character(unique(.anomalies$year)), "Anomaly", "Profiles"))+ # past unique years as title
    xlab(Depth ~ (m))+ # remember y and x-axis are flipped
    theme(axis.title = element_text(size = 15),
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
                          .bins = 10){
  
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
                       length.out = .bins + 1), 2)
  
  # set binwidth for colour scale
  .binwidth <- (legend_range[2] - legend_range[1]) / (.bins * 2)
  
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
                 breaks = .breaks)+
    # set text labels to match contour lines
    geom_text_contour(breaks = .breaks)+
    # set lines every hundred meters to help identify shifts
    geom_hline(yintercept = .depth_breaks,
               linewidth = 0.2, linetype = 2, alpha = 0.8)+
    # plot min and max lines as plot borders
    geom_hline(yintercept = c(depth_range[1], depth_range[2]))+
    scale_x_datetime(date_breaks = "month", date_labels = "%b",
                     expand = c(0.01, 0.01))+
    scale_fill_gradientn(colours = rainbow(7))+
    scale_y_reverse()+ # reverse y-axis to plot depth
    labs(title = paste(as.character(unique(my_data_plot$year)), as.character(.vars), "Contours"))+ # paste unique years and selected variable as title
    xlab("Month")+
    ylab(Depth ~ (m))+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          strip.text = element_text(size = 13),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
  
}

# plot temperature vs salinity along isopycnal lines

ts_plot <- function(.profiles, 
                    .include_dots = FALSE, .include_lines = TRUE,
                    .depths, .breaks = 10){
  
  # filter according to secondary depth settings
  my_data_plot <- .profiles %>% 
    filter(between(depth, .depths[1], .depths[2]))
  
  # extract depth and salinity ranges for grid construction
  temp_range <- range(my_data_plot$temperature, na.rm = TRUE)
  salt_range <- range(my_data_plot$salinity, na.rm = TRUE)
  
  temps <- seq(temp_range[1], temp_range[2], length.out = 20)
  salts <- seq(salt_range[1], salt_range[2], length.out = 20)
  
  ts <- expand_grid(temps, salts)
  
  # calculate potential densities according to the above grid
  ts <- ts %>% 
    mutate(density = oce::swSigma0(salinity = salts, temperature = temps, pressure = 0)) 
  
  density_range <- range(ts$density, na.rm = TRUE)
  
  # set contour lines according to density range and desired amount of breaks
  # adding two because ggplot drops the upper and lower value when visualizing
  .contour_breaks <- seq(density_range[1], density_range[2], 
                         length.out = .breaks + 2) %>%
    round(digits = 2)
  
  # generate plot
  ts %>% 
    # drop na values to prevent errors in plot
    drop_na() %>% 
    ggplot()+
    # add isopycnal contours and text labels
    geom_contour(aes(x = salts, y = temps, z = density), 
                 breaks = .contour_breaks)+
    geom_text_contour(aes(x = salts, y = temps, z = density), 
                      breaks = .contour_breaks, label.placer = label_placer_n(n = 2))+
    # add profile data based on selection in the UI 
    {if(.include_dots == TRUE){ # add dots if TRUE
      geom_point(data = my_data_plot, 
                 aes(x = salinity, y = temperature, colour = month, shape = month),
                 alpha = 0.5)}}+
    {if(.include_lines == TRUE){ # add lines if TRUE
      geom_line(data = my_data_plot, 
                aes(x = salinity, y = temperature, colour = month, linetype = month),
                alpha = 0.5, linewidth = 1.2)}}+
    scale_x_continuous(limits = c(salt_range[1], salt_range[2]),
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(temp_range[1], temp_range[2]),
                       expand = c(0, 0))+
    labs(title = paste(as.character(unique(my_data_plot$year)), "TS-plot"))+ # paste unique years and selected variable as title
    xlab("Salinity")+
    ylab("Temperature")+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 13),
          legend.position = "right",
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "coral4", linewidth = 1))
  
}




# 7: define server -----------------------------------------------------------

# Define server logic required to draw plots and reactive elements
server <- function(input, output) {
  
    
    observeEvent({input$refresh_all | input$dummy_start},{
      
    # 6a: filter data based on dashboard settings ---------------------------------
      
      date_filter <- interval(start = input$filter_dates[1], # set initial time filter values
                              end = input$filter_dates[2])
      
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
    
    
    # 6b: set server items for UI rendering ----------------------------------------------
    
    ## set items to be rendered in UI for secondary depth filters
    
    # depth range for CTD depth & density plots
    output$depths_profiles <- renderUI({
      sliderInput("depths_profiles",
                  "Depth",
                  min = input$filter_depth[1],
                  max = input$filter_depth[2],
                  value = c(input$filter_depth[1], input$filter_depth[2]),
                  step = 10)
    })
    
    output$depths_contours <- renderUI({
      # depth range for contour plot
      sliderInput("depths_contours",
                  "Depth Contours",
                  min = input$filter_depth[1],
                  max = input$filter_depth[2],
                  value = c(input$filter_depth[1], input$filter_depth[2]),
                  step = 10)
      
    })
    
    # depth range for isopycnal plot
    output$depths_ts <- renderUI({
      
      sliderInput("depths_ts",
                  "Depth TS",
                  min = input$filter_depth[1],
                  max = input$filter_depth[2],
                  value = c(400, 600),
                  step = 10)
      
    })
    
    # 6c: generate plots ----------------------------------------------------------

      ## generate plots according to function calls and output plots to fluidpage
      
      output$eddy_tracks <- renderPlot({
        # eddy tracks 
        eddy_tracks(.tracks = data_filt$eddy_tracks,
                    .maps = data_filt$map_coords,
                    .station = station_coordinates,
                    .legend = input$toggle_eddy_legends)
        
      })  
      
      output$eddy_radius <- renderPlot({
        # tracks with radius and station coordinates, faceted by month and type
        eddy_radius(.tracks = data_filt$eddy_tracks,
                    .cast_coords = data_filt$profiles,
                    .maps = data_filt$map_coords,
                    .station = station_coordinates,
                    .legend = input$toggle_eddy_legends)
        
      })
      
      output$depth_profiles <- renderPlot({
        # CTD variables vs depth
        profiles_plot(.profiles = data_filt$profiles,
                      .means = data_filt$profiles_mean,
                      .vars = input$profile_variables,
                      .depths = input$depths_profiles,
                      .legend = input$toggle_profile_legends)
        
      })
      
      output$depth_anomalies <- renderPlot({
        # anomaly values vs depth
        anomalies_plot(.profiles = data_filt$profiles,
                       .anomalies = data_filt$anomalies,
                       .vars = input$profile_variables,
                       .depths = input$depths_profiles,
                       .functions = input$anomaly_functions,
                       .legend = input$toggle_profile_legends)
      })
      
      output$depth_contours <- renderPlot({
        # Depth over Time with colour contours according to selected variable
        contours_plot(.profiles = data_filt$profiles,
                      .vars = input$contour_variable,
                      .depths = input$depths_contours,
                      .bins = input$colour_bins_contours)
        
      })
      
      output$ts_isopycnals <- renderPlot({
        # Temperature-salinity plot overlayed with isopycnals
        ts_plot(.profiles = data_filt$profiles,
                    .include_dots = input$show_dots_ts, 
                    .include_lines = input$show_lines_ts,
                    .depths = input$depths_ts,
                    .breaks = input$isopycnal_bins_ts)
        
      })  
  
  } # end of plots
  ) # end of observeEvent
  
} # end of server

# 8: run app -----------------------------------------------------------------

# run the application 
shinyApp(ui = ui, server = server)
