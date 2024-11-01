#creating a dashboard
install.packages("shiny")
install.packages("sf")
install.packages("leaflet", type = "binary")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
library(shiny)
library(sf)
library(leaflet)
library(readxl)
library(RColorBrewer)
library(dplyr)
library(tidyr)

#pH path:
pH_path <- "/Users/jenae/Library/CloudStorage/Dropbox/2024_TAPS_Veris_raw_spatial_data.xlsx"
pH_data <- read_excel(pH_path, sheet = "pH")
pH_data_avg <- pH_data
pH_data_avg <- pH_data_avg[,-4:-8]

#geogson file
geojson_path <- "/Users/jenae/Library/CloudStorage/Dropbox/ksu_taps_2024_main_area.geojson" #I think just main boarder of a square
geojson_data <- st_read(geojson_path)

#putting ph into spatial data: with WGS 84 CRS
pH_data_sf <- st_as_sf(pH_data_avg, coords = c("Long", "Lat"), crs = 4326)

#ensure both datasets have the same CRS:
if(!st_crs(geojson_data) == st_crs(pH_data_sf)) {
  geojson_data <- st_transform(geojson_data, crs = st_crs(pH_data_sf))
}

print(st_crs(pH_data_sf))
print(st_crs(geojson_data))

#shapefile path
shapefile_path <- "/Users/jenae/Library/CloudStorage/Dropbox/2024_Colby_TAPS_Harvest_Area.shp"
shapefile_plots <- st_read(shapefile_path)


##################################### DELETE BELOW?
#putting in coordinates for pH map:
pH_coords <- data.frame(
  id = 1:3, #identifier for each coordinate
  latitude = pH_data$Lat,
  longitude = pH_data$Long
)

pH_points <- st_as_sf(pH_coords, coords = c("longitude", "latitude"), crs = 4326)

##################################

ui <- fluidPage(
  #app title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    #sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = paste("Team", seq(1,34))),
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None","A", "B", "C", "D"),
                  selected = "None")
    ),
      mainPanel(
        leafletOutput("map"),
        textOutput("selectedOption1"), #display selected dropdown option
        textOutput("selectedOption2")
      )
    )
  )
server <- function(input, output) {
  #color palette for pH data
  pal <- colorNumeric(palette = "YlOrRd", domain = pH_data_sf$`pH Avg.`)
    #render the map with both layers:
    output$map <- renderLeaflet({
      leaflet(geojson_data) %>%
        addTiles() %>%
        #add Geojson polygon layer
        addPolygons(data = geojson_data,
                    color = "blue",
                    weight = 1,
                    fillColor = "lightblue")
    })
    #display the selected dropdown option
    output$selectedOption1 <- renderText ({
      paste("You Selected:", input$dropdown1)
    })
    output$selectedOption2 <- renderText ({
      #only show the selection if it's not "none"
      if(input$dropdown2 != "None") {
        paste("You selected from Dropdown 2:", input$dropdown2)
      } else {
        "No selection made in Dropdown 2"
      }
    })
  }
  
#run the shiny app:
shinyApp(ui = ui, server = server)

## trying to add in pH:
#aggregate pH data (calculating an avg. pH for each plot)
tot.plot_ph <- st_join(geojson_data, pH_data_sf, join = st_intersects)
nrow(tot.plot_ph) #140
print(tot.plot_ph) #140

#checking the CRS:
pH_data_sf <- st_transform(pH_data_sf, st_crs(geojson_data))

#calc avg pH for the plot
tot.plot_ph_levels <- tot.plot_ph %>%
  group_by((geometry)) %>% #how can I do it where it doesn't average them?

print(tot.plot_ph_levels)

leaflet(data = tot.plot_ph_levels) %>%
  addPolygons(fillColor = ~pal(`pH Avg.`),
              color = "black",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
              label = ~paste("Avg pH:", round(`pH Avg.`, 2)),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")))

  
  

  ############################
  #define a function to create small squares around each coordinate
  create_square <- function(coords, size = 0.0005) {
    lon = coords[1]
    lat = coords[2]
    list(lng1 = lon - size, lat1 = lat - size,
         lng2 = lon + size, lat2 = lat + size
         )
  }
  
  #generate blocks based on coordinates:
  blocks <- pH_data_sf %>%
    rowwise() %>%
    mutate(
      coords = list(st_coordinates(geometry)),
      square_coords = list(create_square(coords))) %>%
    ungroup() %>%
    mutate(
      lng1 = sapply(square_coords, `[[`, "lng1"),
      lat1 = sapply(square_coords, `[[`, "lat1"),
      lng2 = sapply(square_coords, `[[`, "lng2"),
      lat2 = sapply(square_coords, `[[`, "lat2")
    )
  
  #render the map with both layers:
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #add Geojson polygon layer
      addPolygons(data = geojson_data,
                  color = "blue",
                  weight = 1,
                  opacity = 0.7,
                  fillOpacity = 0.3,
                  label = ~paste("Region", geojson_data$region_column)) %>%
      #add the pH data points, colored by pH value
      addRectangles(data = blocks,
                       lng1 = ~ lng1, lat1 = ~lat1,
                    lng2 = ~lng2, lat2 = ~lat2,
                    fillColor = ~pal(`pH Avg.`), 
                    fillOpacity = 0.8,
                    color = "black",
                    weight = 0.5,
                    label = ~paste("pH Avg.", `pH Avg.`))
  })
  
  #display the selected dropdown option
  output$selectedOption1 <- renderText ({
    paste("You Selected:", input$dropdown1)
  })
  output$selectedOption2 <- renderText ({
    #only show the selection if it's not "none"
    if(input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
}

#run the shiny app:
shinyApp(ui = ui, server = server)

shiny::runApp("app.R")

################################################################################
#baseline map with plot borders:
##################################

ui <- fluidPage(
  #app title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    #sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = paste("Team", seq(1,34))),
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None","A", "B", "C", "D"),
                  selected = "None")
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("selectedOption1"), #display selected dropdown option
      textOutput("selectedOption2")
    )
  )
)
server <- function(input, output) {
  #render the map with both layers:
  output$map <- renderLeaflet({
    leaflet(shapefile_plots) %>%
      addTiles() %>%
      #add Geojson polygon layer
      addPolygons(data = shapefile_plots,
                  color = "blue",
                  weight = 1,
                  fillColor = "lightblue",
                  label = ~paste("Block ID:", shapefile_plots$Block_ID))
  })
  #display the selected dropdown option
  output$selectedOption1 <- renderText ({
    paste("You Selected:", input$dropdown1)
  })
  output$selectedOption2 <- renderText ({
    #only show the selection if it's not "none"
    if(input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
}

#run the shiny app:
shinyApp(ui = ui, server = server)

#find out the CRS of the shapefile:
st_crs(shapefile_plots)
shapefile_plots <- st_set_crs(shapefile_plots, 4326)

#try it with 4326 CRS code:
LL_plots <- shapefile_plots
LL_plots <- st_set_crs(LL_plots, 4326)

#testing the plot/map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = LL_plots, color = "blue", weight = 2, fillOpacity = 0.5)

######################################################
###########trying to make a plot with pH:
#####################################################

#shapefile uploading

#pH data uploading

#aggregate pH data for each plot polygon (calculating an avg. pH for each plot)
plot_ph <- st_join(shapefile_plots, pH_data_sf, join = st_intersects)
nrow(plot_ph) #140
print(plot_ph) #140

#checking the CRS:
pH_data_sf <- st_transform(pH_data_sf, st_crs(shapefile_plots))

#calc avg pH for the plot
plot_ph_levels <- plot_ph %>%
  group_by(geometry, Plot_ID, Name) %>%
  summarize(`pH Avg.` = mean(`pH Avg.`, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()
 

print(plot_ph_levels)

#got all the pH levels: potentially drop the NaN values though???
#dropping the NaN values
dropped.plot_ph_levels <- plot_ph_levels %>%
  filter(!is.na(`pH Avg.`))
#how many is NA in plot_ph
sum(is.na(plot_ph$`pH Avg.`)) #78 observations have NA instead of a pH value

#WHY AM I DROPPING FROM 884 OBSERVATIONS TO ONLY 60 PLOTS LEFT??

#create map with varying colors for the pH levels:
pal <- colorNumeric(palette = "viridis", domain = plot_ph_levels$`pH Avg.`)

#create map:
leaflet(data = plot_ph_levels) %>%
  addPolygons(fillColor = ~pal(`pH Avg.`),
              color = "black",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
              label = ~paste("Avg pH:", round(`pH Avg.`, 2)),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
              ) %>%
  addLegend("bottomright",                    # Add a legend for the color scale
            pal = pal,
            values = ~`pH Avg.`,
            title = "pH Levels",
            opacity = 1)

#####################################################
######## TRYING TO PLOT JUST THE PH DATA COLLECTIONS
######################################################

# Create a color palette for pH levels
pal <- colorNumeric(palette = "viridis", domain = pH_data_sf$`pH Avg.`)

# Create the leaflet map
leaflet(data = pH_data_sf) %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Base map
  addCircleMarkers(
    radius = 5,                               # Set marker size
    color = ~pal(`pH Avg.`),                     # Color by pH level
    fillColor = ~pal(`pH Avg.`),                 # Fill color by pH level
    fillOpacity = 0.8,                        # Transparency of markers
    stroke = FALSE,                           # No border around the points
    label = ~paste("pH Level:", round(`pH Avg.`, 2)), # Popup label
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      direction = "auto"
    )
  ) %>%
  addLegend("bottomright",                    # Add a legend for the color scale
            pal = pal,
            values = ~`pH Avg.`,
            title = "pH Levels",
            opacity = 1)

########################
### TRYING IT WITH THE GEOJSON FILE AS BORDER
########################
# Create the leaflet map
leaflet(data = pH_data_sf) %>%
  addTiles() %>%
  addPolygons(data = shapefile_plots) %>%  # Base map
  addCircleMarkers(
    radius = 5,                               # Set marker size
    color = ~pal(`pH Avg.`),                     # Color by pH level
    fillColor = ~pal(`pH Avg.`),                 # Fill color by pH level
    fillOpacity = 0.8,                        # Transparency of markers
    stroke = FALSE,                           # No border around the points
    label = ~paste("pH Level:", round(`pH Avg.`, 2)), # Popup label
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      direction = "auto"
    )
  ) %>%
  addLegend("bottomright",                    # Add a legend for the color scale
            pal = pal,
            values = ~`pH Avg.`,
            title = "pH Levels",
            opacity = 1)

################################################################################
#creating the team plot mapping:
team_plot_mapping <- data.frame(
  Team_ID = paste("Team", seq(1,34)),
  Plot_IDs = I(list(
    c(2703,201,1204,2005), #team 1 plots
    c(1903,602,2704,205), #team 2 plots
    c(1502,501,2004,1005), #team 3 plots
    c(2301,1102,1806,304), #4
    c(2303,301,2504,505), #5
    c(2001,802,1606,905), #6
    c(1803,901,2505,1006), #7
    c(2503,601,1804,506), #8
    c(2201,1503,1506,1104), #9
    c(2401,303,1706,604), #10
    c(2402,801,2404,906), #11
    c(2203,402,2506,805), #12
    c(2702,1201,1704,405), #13
    c(1701,203,1605,1304), #14
    c(2502,903,2206,404), #15
    c(2002,1003,2204,1105), #16
    c(2602,503,2205,606), #17
    c(1802,1101,1805,1206), #18
    c(2501,502,1906,305), #19
    c(1501,302,2405,806), #20
    c(2403,1001,1705,406), #21
    c(1901,1203,1604,1305), #22
    c(2601,603,1904,1306), #23
    c(2701,1202,2606,804), #24
    c(1801,403,2304,1106), #25
    c(2603,202,2406,605), #26
    c(2202,1303,2306,204), #27
    c(2003,1302,2604,506), #28
    c(1902,1103,2006,1205), #29
    c(2302,1301,2605,206), #30
    c(1601,803,1905,1004), #31
    c(1602,401,1505,904), #32
    c(1703,1002,2305,1504), #33
    c(1603,902,2705,504) #34
  ))
)

#putting it into a dashboard:
ui <- fluidPage(
  #app title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    #sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = paste("Team", seq(1,34))),
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None","Span A", "Span B", "Span C", "Span D"),
                  selected = "None"),
      uiOutput("plot_id_output") #U output for plot IDs
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("selectedOption1"), #display selected dropdown option
      textOutput("selectedOption2")
    )
  )
)
server <- function(input, output) {
  #reactive expression to filter plot IDS based on Teams
  filtered_plot_ids <- reactive({
    selected_team <- input$dropdown1
    team_index <- as.numeric(sub("Team", "", selected_team))
    team_plot_mapping$Plot_IDs[[team_index]] #get plot ids for the selected team
  })
  
#reactive expression to filter plot_ph_levles based on selected block
filtered_data <- reactive({
  if (input$dropdown2 == "None") {
    return(plot_ph_levels) #if no block is selected, return all data
  } else {
    data <- plot_ph_levels %>% filter(Name == input$dropdown2) #filter for selected block
    print(data)
    return(data)
  }
})
#render the plot IDs in the US
output$plot_id_output <- renderUI({
  plot_ids <- filtered_plot_ids()
  if(length(plot_ids) > 0) {
    tags$div(
      h4("Plot IDs for Selected Team:"),
      tags$ul(lapply(plot_ids, function(id) tags$li(id))) #create list of plot IDs
    )
  } else {
    "No plot IDs available for the selected team."
  }
})


# Create the leaflet map
output$map <- renderLeaflet({
  #create color palette for ph levels
  pal <- colorNumeric(palette = "viridis", domain = filtered_data()$`pH Avg.`)
  leaflet(data = filtered_data()) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(fillColor = ~pal(`pH Avg.`),
                color = "black",
                weight = 1,
                opacity = 1,
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
                label = ~paste(
                  "Avg pH:", round(`pH Avg.`, 2),
                  "Block ID:", Name,
                  "Plot ID:", Plot_ID
                  ),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
    ) %>%
    addLegend("bottomright",                    # Add a legend for the color scale
              pal = pal,
              values = ~`pH Avg.`,
              title = "pH Levels",
              opacity = 1)
})
#display the selected dropdown option
output$selectedOption1 <- renderText ({
  paste("You Selected:", input$dropdown1)
})
output$selectedOption2 <- renderText ({
  #only show the selection if it's not "none"
  if(input$dropdown2 != "None") {
    paste("You selected from Dropdown 2:", input$dropdown2)
  } else {
    "No selection made in Dropdown 2"
  }
})

}

# Run the Shiny app
shinyApp(ui = ui, server = server)

### for next time: how to get when the team is selected to show the plots that it belongs to and to blur out the rest in the map?
################################################################################
#################################################################################
################################################################################

#shapefile uploading
save(shapefile_plots, file = "shapefile_plots.RData")

#pH data uploading
save(plot_ph, file = "plot_ph.RData")

#aggregate pH data for each plot polygon (calculating an avg. pH for each plot)
plot_ph <- st_join(shapefile_plots, pH_data_sf, join = st_intersects)
nrow(plot_ph) #140
print(plot_ph) #140

#checking the CRS:
pH_data_sf <- st_transform(pH_data_sf, st_crs(shapefile_plots))

#calc avg pH for the plot
plot_ph_levels <- plot_ph %>%
  group_by(geometry, Plot_ID, Name) %>%
  summarize(`pH Avg.` = mean(`pH Avg.`, na.rm = TRUE), .groups = 'drop') %>%
  ungroup()


print(plot_ph_levels)

#Irrigation Data
irrigation_data <- read_excel("/Users/jenae/Library/CloudStorage/Dropbox/2024_TAPS_management.xlsx", sheet = "Irrigation amounts") %>%
  select(ID, Total)

#rename ID to Team_ID for clarity
irrigation_data <- irrigation_data %>% rename(Team_ID = ID)
#converting from number to add team in front:
irrigation_data <- irrigation_data %>%
  mutate(Team_ID = paste0("Team ", Team_ID))

#creating the team plot mapping:
team_plot_mapping <- data.frame(
  Team_ID = paste("Team", seq(1,34)),
  Plot_IDs = I(list(
    c(2703,201,1204,2005), #team 1 plots
    c(1903,602,2704,205), #team 2 plots
    c(1502,501,2004,1005), #team 3 plots
    c(2301,1102,1806,304), #4
    c(2303,301,2504,505), #5
    c(2001,802,1606,905), #6
    c(1803,901,2505,1006), #7
    c(2503,601,1804,506), #8
    c(2201,1503,1506,1104), #9
    c(2401,303,1706,604), #10
    c(2402,801,2404,906), #11
    c(2203,402,2506,805), #12
    c(2702,1201,1704,405), #13
    c(1701,203,1605,1304), #14
    c(2502,903,2206,404), #15
    c(2002,1003,2204,1105), #16
    c(2602,503,2205,606), #17
    c(1802,1101,1805,1206), #18
    c(2501,502,1906,305), #19
    c(1501,302,2405,806), #20
    c(2403,1001,1705,406), #21
    c(1901,1203,1604,1305), #22
    c(2601,603,1904,1306), #23
    c(2701,1202,2606,804), #24
    c(1801,403,2304,1106), #25
    c(2603,202,2406,605), #26
    c(2202,1303,2306,204), #27
    c(2003,1302,2604,506), #28
    c(1902,1103,2006,1205), #29
    c(2302,1301,2605,206), #30
    c(1601,803,1905,1004), #31
    c(1602,401,1505,904), #32
    c(1703,1002,2305,1504), #33
    c(1603,902,2705,504) #34
  ))
)

#adding Team_ID column to plot_ph_levels 
plot_ph_levels <- plot_ph_levels %>%
  left_join(
    team_plot_mapping %>%
      unnest(cols = c(Plot_IDs)) %>%
      rename(Plot_ID = Plot_IDs),
    by = "Plot_ID"
  )

#merge the irrigation data to each plot by Team_ID
plot_ph_levels <- plot_ph_levels %>%
  left_join(irrigation_data, by = "Team_ID")


ui <- fluidPage(
  # App title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    # Sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = c("None", paste("Team", seq(1, 34))),
                  selected = "None"), # starting with none as the default option
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None", "Span A", "Span B", "Span C", "Span D"),
                  selected = "None"),
      uiOutput("plot_id_output") # U output for plot IDs
    ),
    mainPanel(
      tabsetPanel(
        # Tabs for pH map and irrigation map
        tabPanel("pH Map", leafletOutput("ph_map"),
                 textOutput("selectedOption1"),
                 textOutput("selectedOption2")),
        tabPanel("Irrigation Map", leafletOutput("irrigation_map"))
      )
    )
  )
)

server <- function(input, output) {
  # Reactive expression to filter plot IDs based on Teams
  filtered_plot_ids <- reactive({
    selected_team <- input$dropdown1
    if (selected_team == "None") {
      return(NULL)
    } else {
      team_index <- as.numeric(sub("Team", "", selected_team))
      team_plot_mapping$Plot_IDs[[team_index]] # get plot ids for the selected team
    }
  })
  
  # Reactive expression to filter plot_ph_levels based on selected block
  filtered_data <- reactive({
    if (input$dropdown2 == "None") {
      return(plot_ph_levels) # if no block is selected, return all data
    } else {
      data <- plot_ph_levels %>% filter(Name == input$dropdown2) # filter for selected block
      print(data)
      return(data)
    }
  })
  
  # Create the pH map
  output$ph_map <- renderLeaflet({
    # Create color palette for pH levels
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$`pH Avg.`)
    # Get the IDs of the plots for the selected team
    selected_plots <- filtered_plot_ids()
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "red", pal(`pH Avg.`)),
        color = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "black", "gray"),
        weight = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 2, 1),
        opacity = 1,
        fillOpacity = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 0.9, 0.5),
        highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
        label = ~paste(
          "Avg pH:", round(`pH Avg.`, 2),
          "Block ID:", Name,
          "Plot ID:", Plot_ID
        ),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~`pH Avg.`,
                title = "pH Levels",
                opacity = 1)
  })
  
  # Create the irrigation map
  output$irrigation_map <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = plot_ph_levels$Total)
    selected_data <- filtered_plot_ids
    
    leaflet(data = plot_ph_levels) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(Total),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste("Team:", Team_ID,
                       "Total Irrigation:", Total, "inches"),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Total, title = "Total Irrigation (inches)", opacity = 1)
  })
  
  # Display the selected dropdown option
  output$selectedOption1 <- renderText({
    paste("You Selected:", input$dropdown1)
  })
  
  output$selectedOption2 <- renderText({
    # Only show the selection if it's not "none"
    if (input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
  #selected dropdown option for irrigation map
  output$selectedIrrigationOption1 <- renderText({
    paste("You Selected Team:", input$dropdown1)
  })
  output$selectedIrrigationOption2 <- renderText({
    if(input$dropdown2 != "None") {
      paste("You selected from Block ID:", input$dropdown2)
    } else {
      "No selection made in Block ID"
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
















#putting it into a dashboard:
ui <- fluidPage(
  #app title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    #sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = c("None", paste("Team", seq(1,34))),
                  selected = "None"), #starting with none as the default option
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None","Span A", "Span B", "Span C", "Span D"),
                  selected = "None"),
      uiOutput("plot_id_output") #U output for plot IDs
    ),
    mainPanel(
      tabsetPanel(
        #tabs for pH map and irrigation map
        tabPanel("pH Map", leafletOutput("ph_map"),
                 textOutput("selectedOption1"),
                 textOutput("selectedOption2")),
        tabPanel("Irrigation Map", 
                 leafletOutput("irrigation_map"),
                 textOutput("selectedIrrigationOption1"),
                 textOutput("selectedIrrigationOption2"))
      )
    )
  )
)
server <- function(input, output) {
  #reactive expression to filter plot IDS based on Teams
  filtered_plot_ids <- reactive({
    selected_team <- input$dropdown1
    if(selected_team == "None") {
      return(NULL)
    } else {
    team_index <- as.numeric(sub("Team", "", selected_team))
    team_plot_mapping$Plot_IDs[[team_index]] #get plot ids for the selected team
    }
  })
  
  #reactive expression to filter plot_ph_levles based on selected block
  filtered_data <- reactive({
    if (input$dropdown2 == "None") {
      str(plot_ph_levels)
      return(plot_ph_levels) #if no block is selected, return all data
    } else {
      data <- plot_ph_levels %>% filter(Name == input$dropdown2) #filter for selected block
      print(data)
      return(data)
    }
  })
  #irrigation team selection
  filtered_irr_data_ids <- reactive({
    selected_team <- input$dropdown1
    if (selected_team == "None") {
      return(NULL)
    } else {
      team_index <- as.numeric(sub("Team", "", selected_team))
      team_plot_mapping$Plot_IDs[[team_index]]
    }
  })
  #irrigation block selection
  filtered_irr_data <- reactive({
    if(input$dropdown2 == "None") {
      return(plot_ph_levels)
    } else{
      data_2 <- plot_ph_levels %>% filter(Name == input$dropdown2)
      return(data_2)
    }
  })
  
  # Create the pH leaflet map:
  output$ph_map <- renderLeaflet({
    #create color palette for ph levels
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$`pH Avg.`)
    #get the IDs of the plots for the selected team
    selected_plots <- filtered_plot_ids()
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "red", pal(`pH Avg.`)), #Red for selected teams plots
                  color = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "black", "gray"), #bold outline for selected teams' pltos
                  weight = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 2,1), #thicket border for selected teams plots
                  opacity = 1,
                  fillOpacity = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 0.9, 0.5), #higher opacity for selected plots
                  highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
                  label = ~paste(
                    "Avg pH:", round(`pH Avg.`, 2),
                    "Block ID:", Name,
                    "Plot ID:", Plot_ID
                  ),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright",                    # Add a legend for the color scale
                pal = pal,
                values = ~`pH Avg.`,
                title = "pH Levels",
                opacity = 1)
  })
  #create the irrigation map:
  output$irrigation_map <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = filtered_data$Total)
    selected_plots <- filtered_plot_ids()
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, pal(Total), "lightgray"),
                  color = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "black","gray"),
                  weight = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 2, 1),
                  opacity = 1,
                  fillOpacity = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 0.7, 0.2),
                  label = ~paste("Team:", Team_ID,
                                 "Total Irrigation:", Total, "inches"),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Total, title = "Total Irrigation (inches)", opacity = 1)
  })
  
  #display the selected dropdown option for pH map:
  output$selectedOption1 <- renderText ({
    paste("You Selected:", input$dropdown1)
  })
  output$selectedOption2 <- renderText ({
    #only show the selection if it's not "none"
    if(input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
  #selected dropdown option for irrigation map
  output$selectedIrrigationOption1 <- renderText({
    paste("You Selected Team:", input$dropdown1)
  })
  output$selectedIrrigationOption2 <- renderText({
    if(input$dropdown2 != "None") {
      paste("You selected from Block ID:", input$dropdown2)
    } else {
      "No selection made in Block ID"
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





### CODE FOR THE IRRIGATION MAP:
# Update map to reflect irrigation data
output$map <- renderLeaflet({
  pal <- colorNumeric(palette = "Blues", domain = plot_data_with_irrigation$`Total Irrigation`)
  leaflet(data = plot_data_with_irrigation) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(fillColor = ~pal(`Total Irrigation`),
                color = "black",
                weight = 1,
                opacity = 1,
                fillOpacity = 0.7,
                label = ~paste("Team:", Team_ID, "<br>",
                               "Total Irrigation:", `Total Irrigation`, "inches", "<br>",
                               "Avg pH:", round(`pH Avg.`, 2)),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
    ) %>%
    addLegend("bottomright", pal = pal, values = ~`Total Irrigation`, title = "Total Irrigation (inches)", opacity = 1)
})




################################################################################
################################################################################
################################################################################
#ORIGINAL: without irrigation map
#putting it into a dashboard:
ui <- fluidPage(
  #app title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    #sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = c("None", paste("Team", seq(1,34))),
                  selected = "None"), #starting with none as the default option
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None","Span A", "Span B", "Span C", "Span D"),
                  selected = "None"),
      uiOutput("plot_id_output") #U output for plot IDs
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("selectedOption1"), #display selected dropdown option
      textOutput("selectedOption2")
    )
  )
)
server <- function(input, output) {
  #reactive expression to filter plot IDS based on Teams
  filtered_plot_ids <- reactive({
    selected_team <- input$dropdown1
    if(selected_team == "None") {
      return(NULL)
    } else {
      team_index <- as.numeric(sub("Team", "", selected_team))
      team_plot_mapping$Plot_IDs[[team_index]] #get plot ids for the selected team
    }
  })
  
  #reactive expression to filter plot_ph_levles based on selected block
  filtered_data <- reactive({
    if (input$dropdown2 == "None") {
      return(plot_ph_levels) #if no block is selected, return all data
    } else {
      data <- plot_ph_levels %>% filter(Name == input$dropdown2) #filter for selected block
      print(data)
      return(data)
    }
  })
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    #create color palette for ph levels
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$`pH Avg.`)
    #get the IDs of the plots for the selected team
    selected_plots <- filtered_plot_ids()
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "red", pal(`pH Avg.`)), #Red for selected teams plots
        color = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "black", "gray"), #bold outline for selected teams' pltos
        weight = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 2,1), #thicket border for selected teams plots
        opacity = 1,
        fillOpacity = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 0.9, 0.5), #higher opacity for selected plots
        highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
        label = ~paste(
          "Avg pH:", round(`pH Avg.`, 2),
          "Block ID:", Name,
          "Plot ID:", Plot_ID
        ),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright",                    # Add a legend for the color scale
                pal = pal,
                values = ~`pH Avg.`,
                title = "pH Levels",
                opacity = 1)
  })
  #display the selected dropdown option
  output$selectedOption1 <- renderText ({
    paste("You Selected:", input$dropdown1)
  })
  output$selectedOption2 <- renderText ({
    #only show the selection if it's not "none"
    if(input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


##########################
###########################
##########################
ui <- fluidPage(
  # App title
  titlePanel("2024_TAPS_Dashboard"),
  sidebarLayout(
    # Sidebar panel with dropdown menu and slider input
    sidebarPanel(
      selectInput("dropdown1",
                  "Select a Team",
                  choices = c("None", paste("Team", seq(1, 34))),
                  selected = "None"), # starting with none as the default option
      selectInput("dropdown2",
                  "(Optional) Select a Block",
                  choices = c("None", "Span A", "Span B", "Span C", "Span D"),
                  selected = "None"),
      uiOutput("plot_id_output") # U output for plot IDs
    ),
    mainPanel(
      tabsetPanel(
        # Tabs for pH map and irrigation map
        tabPanel("pH Map", leafletOutput("ph_map"),
                 textOutput("selectedOption1"),
                 textOutput("selectedOption2")),
        tabPanel("Irrigation Map", leafletOutput("irrigation_map"))
      )
    )
  )
)

server <- function(input, output) {
  # Reactive expression to filter plot IDs based on Teams
  filtered_plot_ids <- reactive({
    selected_team <- input$dropdown1
    if (selected_team == "None") {
      return(NULL)
    } else {
      team_index <- as.numeric(sub("Team", "", selected_team))
      team_plot_mapping$Plot_IDs[[team_index]] # get plot ids for the selected team
    }
  })
  
  # Reactive expression to filter plot_ph_levels based on selected block
  filtered_data <- reactive({
    if (input$dropdown2 == "None") {
      return(plot_ph_levels) # if no block is selected, return all data
    } else {
      data <- plot_ph_levels %>% filter(Name == input$dropdown2) # filter for selected block
      print(data)
      return(data)
    }
  })
  
  # Create the pH map
  output$ph_map <- renderLeaflet({
    # Create color palette for pH levels
    pal <- colorNumeric(palette = "viridis", domain = filtered_data()$`pH Avg.`)
    # Get the IDs of the plots for the selected team
    selected_plots <- filtered_plot_ids()
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "red", pal(`pH Avg.`)),
        color = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, "black", "gray"),
        weight = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 2, 1),
        opacity = 1,
        fillOpacity = ~ifelse(!is.null(selected_plots) & Plot_ID %in% selected_plots, 0.9, 0.5),
        highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE),
        label = ~paste(
          "Avg pH:", round(`pH Avg.`, 2),
          "Block ID:", Name,
          "Plot ID:", Plot_ID
        ),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~`pH Avg.`,
                title = "pH Levels",
                opacity = 1)
  })
  
  # Create the irrigation map
  output$irrigation_map <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = plot_ph_levels$Total)
    selected_data <- filtered_plot_ids
    
    leaflet(data = plot_ph_levels) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(Total),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste("Team:", Team_ID,
                       "Total Irrigation:", Total, "inches"),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Total, title = "Total Irrigation (inches)", opacity = 1)
  })
  
  # Display the selected dropdown option
  output$selectedOption1 <- renderText({
    paste("You Selected:", input$dropdown1)
  })
  
  output$selectedOption2 <- renderText({
    # Only show the selection if it's not "none"
    if (input$dropdown2 != "None") {
      paste("You selected from Dropdown 2:", input$dropdown2)
    } else {
      "No selection made in Dropdown 2"
    }
  })
  #selected dropdown option for irrigation map
  output$selectedIrrigationOption1 <- renderText({
    paste("You Selected Team:", input$dropdown1)
  })
  output$selectedIrrigationOption2 <- renderText({
    if(input$dropdown2 != "None") {
      paste("You selected from Block ID:", input$dropdown2)
    } else {
      "No selection made in Block ID"
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

############################
#@#########################
#################
########
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Irrigation Advisory Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # Location and Crop Information
      textInput("town", "Town", value = "Manhattan, Kansas"),
      dateInput("planting_date", "Planting Date", value = Sys.Date()),
      selectInput("crop_type", "Crop Type", choices = c("Corn", "Wheat", "Soybean")),
      
      # Initial Soil Moisture and Irrigation Events
      numericInput("initial_soil_moisture", "Initial Soil Moisture (%)", min = 0, value = 50),
      
      # Multi-date picker for irrigation events
      dateInput("irrigation_dates", "Select Irrigation Days"),
      
      # Dynamic input for irrigation amounts
      uiOutput("irrigation_inputs"),
      
      # Action button to get irrigation advice
      actionButton("get_advice", "Get Irrigation Advice")
    ),
    
    mainPanel(
      # Display results
      textOutput("estimated_soil_moisture"),
      textOutput("irrigation_advice")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Reactive variable to store irrigation amounts
  irrigation_events <- reactive({
    req(input$irrigation_dates)
    lapply(input$irrigation_dates, function(date) {
      list(date = date, amount = input[[paste0("irrigation_amount_", date)]])
    })
  })
  
  # Dynamic UI for irrigation amount inputs based on selected dates
  output$irrigation_inputs <- renderUI({
    req(input$irrigation_dates)
    lapply(input$irrigation_dates, function(date) {
      numericInput(paste0("irrigation_amount_", date), 
                   paste("Irrigation amount on", date), 
                   min = 0, value = 0)
    })
  })
  
  # Fetch historical weather data from Visual Crossing API
  get_historical_weather_data <- function(town, start_date, end_date) {
    api_key <- "YOUR_VISUALCROSSING_API_KEY"  # Replace with your actual API key
    url <- paste0(
      "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/", 
      URLencode(town), "/", start_date, "/", end_date,
      "?unitGroup=metric&key=", api_key, "&contentType=json"
    )
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      weather_data <- data.frame(
        date = as.Date(sapply(data$days, `[[`, "datetime")),
        temp = sapply(data$days, `[[`, "temp"),
        humidity = sapply(data$days, `[[`, "humidity"),
        windspeed = sapply(data$days, `[[`, "windspeed"),
        precip = sapply(data$days, `[[`, "precip"),
        solarradiation = sapply(data$days, function(day) ifelse(is.null(day$solarradiation), 10, day$solarradiation))
      )
      return(weather_data)
    } else {
      return(NULL)
    }
  }
  
  # Calculate ET₀ using FAO Penman-Monteith formula
  calculate_et0 <- function(temp, wind_speed, humidity, solar_radiation, latitude) {
    T <- temp  # Celsius
    u2 <- wind_speed / 3.6  # Convert km/h to m/s
    e_s <- 0.6108 * exp((17.27 * T) / (T + 237.3))
    e_a <- e_s * (humidity / 100)
    vpd <- e_s - e_a
    gamma <- 0.665 * 0.001 * 101.3
    J <- yday(Sys.Date())
    lat_rad <- latitude * pi / 180
    dr <- 1 + 0.033 * cos(2 * pi / 365 * J)
    delta <- 0.409 * sin(2 * pi / 365 * J - 1.39)
    omega <- acos(-tan(lat_rad) * tan(delta))
    Ra <- (24 * 60 / pi) * 0.0820 * dr * (omega * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(delta) * sin(omega))
    Rns <- (1 - 0.23) * solar_radiation
    Rnl <- (4.903e-9 * ((T + 273.16)^4) * (0.34 - 0.14 * sqrt(e_a)) * ((1.35 * (solar_radiation / Ra)) - 0.35))
    Rn <- Rns - Rnl
    delta_slope <- (4098 * e_s) / ((T + 237.3)^2)
    ET0 <- ((0.408 * delta_slope * Rn) + (gamma * (900 / (T + 273)) * u2 * vpd)) / (delta_slope + gamma * (1 + 0.34 * u2))
    return(ET0 * 0.0393701)  # Convert to inches/day
  }
  
  # Calculate soil moisture based on initial moisture, irrigation, and ET₀
  calculate_soil_moisture <- function(initial_moisture, weather_data, irrigation_events) {
    current_moisture <- initial_moisture
    for (i in 1:nrow(weather_data)) {
      date <- weather_data$date[i]
      temp <- weather_data$temp[i]
      wind_speed <- weather_data$windspeed[i]
      humidity <- weather_data$humidity[i]
      solar_radiation <- weather_data$solarradiation[i]
      precip <- weather_data$precip[i]
      
      et0 <- calculate_et0(temp, wind_speed, humidity, solar_radiation, latitude = 39.1836)  # Latitude for Kansas
      
      irrigation_amount <- sum(sapply(irrigation_events(), function(event) {
        if (event$date == date) return(event$amount) else return(0)
      }))
      
      # Adjust soil moisture with precipitation, irrigation, and ET₀
      current_moisture <- max(0, min(100, current_moisture + (precip + irrigation_amount) * 0.995 - et0))
    }
    return(current_moisture)
  }
  
  # Observe event to calculate and display irrigation advice
  observeEvent(input$get_advice, {
    start_date <- format(input$planting_date, "%Y-%m-%d")
    end_date <- format(Sys.Date(), "%Y-%m-%d")
    
    # Retrieve historical weather data
    weather_data <- get_historical_weather_data(input$town, start_date, end_date)
    if (is.null(weather_data)) {
      output$estimated_soil_moisture <- renderText("Error retrieving weather data.")
      return(NULL)
    }
    
    # Calculate soil moisture
    estimated_moisture <- calculate_soil_moisture(input$initial_soil_moisture, weather_data, irrigation_events)
    output$estimated_soil_moisture <- renderText(paste("Estimated Soil Moisture:", round(estimated_moisture, 2), "%"))
    
    # Generate irrigation advice
    if (estimated_moisture < 40) {
      output$irrigation_advice <- renderText("High irrigation needed - apply 1.0 inch of water.")
    } else if (estimated_moisture > 70) {
      output$irrigation_advice <- renderText("No irrigation needed; soil moisture is sufficient.")
    } else {
      output$irrigation_advice <- renderText("Moderate irrigation recommended - apply 0.5 inches of water.")
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
