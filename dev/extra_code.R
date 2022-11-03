      leafletProxy("map") %>%
        clearControls() %>%
        clearMarkers() %>%
        addMarkers(data = map_df(), lat = ~lat, lng = ~lon, layerId = ~id, clusterOptions = markerClusterOptions()) %>%
        addLegend(
          position = "bottomright", 
          pal = leaflet::colorFactor(palette = map_pal()$color, domain = map_pal()$group), 
          values = map_pal()$group
        )