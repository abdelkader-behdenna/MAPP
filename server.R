library(shiny)
library(shinyBS)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(ggmap)
library(mapproj)
library(rgdal)
library(sp)
library(MASS)
library(ggplot2)
library(scales)
library(d3heatmap)
library(RColorBrewer)
library(grid)
library(heatmaply)


#counts the number of occurrences of x in the list l
count <- function(x,l){
	sum(l == x)
}


function(input, output, session) {

	updateNavbarPage(session, "nav",
      selected = "upload"
    )

###### upload files #####################################################

		datasetInput <- reactive({
		    switch(input$dataset,
		    	   "Select dataset" = "select_ex",
		           "Two correlated examples" = "correl_ex",
		           "Multi-sampling example" = "multi_samp_ex")
		  })

	file1_init <- reactive({
		inFile1 <- input$file1
		#if ((datasetInput() == "correl_ex") && (is.null(inFile1))){
		if ((datasetInput() == "correl_ex")){
			file_buf1 <- read.csv("generated_data.csv",header = FALSE, sep = ",")
		#} else if ((datasetInput() == "multi_samp_ex") && (is.null(inFile1))){
		} else if ((datasetInput() == "multi_samp_ex")){
			file_buf1 <- read.csv("generated_data_multi_sampling.csv",header = FALSE, sep = ",")
		} else if ((datasetInput() == "select_ex") && (is.null(inFile1))){
			file_buf1 <- read.csv("empty.csv",header = FALSE, sep = ",")
			return(NULL)
		} else if (is.null(inFile1)){
	    	return(NULL)
		} else {
			file_buf1 <- read.csv(inFile1$datapath, header = input$header,
	    	sep = input$sep, quote = input$quote)
		}
	    if (ncol(file_buf1) == 4){
	    	file_buf1$Status = "1"
	    }
	    buf_names <- c("Id","lat","lon","Year","Status")
	    if (ncol(file_buf1) > 5){
	    	for (i in seq(6,ncol(file_buf1))){
	    		buf_names <- append(buf_names,toString(i))
	    	}
	    }
	    colnames(file_buf1) <- buf_names
	    file_buf1 <- file_buf1[order(file_buf1$Year),]
	    file_buf1
	    })

	file2_init <- reactive({
		inFile2 <- input$file2
	    #if ((datasetInput() == "correl_ex") && (is.null(inFile2))){
	    if ((datasetInput() == "correl_ex")){
			file_buf2 <- read.csv("generated_data_2.csv",header = FALSE, sep = ",")
		#} else if ((datasetInput() == "multi_samp_ex") && (is.null(inFile2))){
		} else if ((datasetInput() == "multi_samp_ex")){
			file_buf2 <- read.csv("generated_data_multi_sampling.csv",header = FALSE, sep = ",")
		# } else if ((datasetInput() == "select_ex") && (is.null(inFile2))){
		# 	file_buf2 <- read.csv("empty.csv",header = FALSE, sep = ",")
		# 	return(NULL)
		} else if (is.null(inFile2)){
	    	return(file1_init())
		} else {
			file_buf2 <- read.csv(inFile2$datapath, header = input$header,
	    	sep = input$sep, quote = input$quote)
		}
	    if (ncol(file_buf2) == 4){
	    	file_buf2$Status = "1"
	    }
	    buf_names <- c("Id","lat","lon","Year","Status")
	    if (ncol(file_buf2) > 5){
	    	for (i in seq(6,ncol(file_buf2))){
	    		buf_names <- append(buf_names,toString(i))
	    	}
	    }
	    colnames(file_buf2) <- buf_names 
	    file_buf2 <- file_buf2[order(file_buf2$Year),]
	    file_buf2
	    })

	file_current <- reactive({
		if (input$file_sel == "f1"){
			file1_init()
		} else {
			file2_init()
		}
	})

	output$contents <- renderTable({
	    file_current()
  	})

  	observe ({
	  		if (is.null(input$file2)) {
		  		output$radio_file2 <- renderUI({
		    	tagList(
		    		radioButtons('Data2', 'Status', c('0', '1', "All"), inline = TRUE, selected = "0")
		    	)
		  		})
		  	} else {
		  		output$radio_file2 <- renderUI({
		    	tagList(
		    		radioButtons('Data2', 'Status', c('0', '1', "All"), inline = TRUE, selected = "1")
		    	)
		  		})
		  	}
	})

	observe({
	#if (is.null(file1()) | is.null(file2())) {
		if (is.null(file1_init()) | is.null(file2_init())) {

		} else {

		file1 <- reactive({
				if (input$File1 == "file1"){
					if (input$Data1 == "0"){
						subset(file1_init(), Status == "0")
					} else {
						if (input$Data1 == "1"){
							subset(file1_init(), Status == "1")
						} else {
							subset(file1_init(), Status == "1" | Status == "0")
						}
					}
				} else {
					if (input$Data1 == "0"){
						subset(file2_init(), Status == "0")
					} else {
						if (input$Data1 == "1"){
							subset(file2_init(), Status == "1")
						} else {
							subset(file2_init(), Status == "1" | Status == "0")
						}
					}
				}
			})

		file2 <- reactive({
				if (input$File2 == "file1"){
					if (input$Data2 == "0"){
						subset(file1_init(), Status == "0")
					} else {
						if (input$Data2 == "1"){
							subset(file1_init(), Status == "1")
						} else {
							subset(file1_init(), Status == "1" | Status == "0")
						}
					}
				} else {
					if (input$Data2 == "0"){
						subset(file2_init(), Status == "0")
					} else {
						if (input$Data2 == "1"){
							subset(file2_init(), Status == "1")
						} else {
							subset(file2_init(), Status == "1" | Status == "0")
						}
					}
				}
			})

		file_both <- reactive({
			rbind(file1()[c("Id","lat","lon","Year")],file2()[c("Id","lat","lon","Year")])
			})


		observe({
			output$info_file1 <- renderText({
		    	paste0("File 1: ",input$file1$name)
		  	})
			if (is.null(input$file2)) {} else {
			  	output$info_file2 <- renderText({
			    	paste0("File 2: ",input$file2$name)
			  	})
			}
		})

	  	mid_lng <- reactive({(min(file_both()$lon)+max(file_both()$lon))/2})
		mid_lat <- reactive({(min(file_both()$lat)+max(file_both()$lat))/2})

		nb_Year <- reactive({max(file_both()$Year) - min(file_both()$Year)})
		year_max <- reactive({max(file_both()$Year)})
		year_min <- reactive({min(file_both()$Year)})

		output$min_year_choice1 <- renderUI({
    	tagList(
      	numericInput("min_year_choice", "Min year", min = year_min(), max = year_max(), value = year_min())
    	)
  		})
  		output$max_year_choice1 <- renderUI({
    	tagList(
      	numericInput("max_year_choice", "Max year", min = year_min(), max = year_max(), value = year_max())
    	)
  		})
  		output$min_year_1 <- renderUI({
    	tagList(
      	numericInput("minYear1", "Min year", min=min(file1()$Year), max=max(file1()$Year), value=min(file1()$Year))
    	)
  		}) 
  		output$max_year_1 <- renderUI({
    	tagList(
      	numericInput("maxYear1", "Max year", min=min(file1()$Year), max=max(file1()$Year), value=max(file1()$Year))
    	)
  		})  
  		output$min_year_2 <- renderUI({
    	tagList(
      	numericInput("minYear2", "Min year", min=min(file2()$Year), max=max(file2()$Year), value=min(file2()$Year))
    	)
  		})  
  		output$max_year_2 <- renderUI({
    	tagList(
      	numericInput("maxYear2", "Max year", min=min(file2()$Year), max=max(file2()$Year), value=max(file2()$Year))
    	)
  		})

###### Map ##############################################################

	  output$map <- renderLeaflet({
	    leaflet() %>%
	      addTiles(
	        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
	      ) %>%
	      fitBounds(lng1 = max(file_both()$lon),lat1 = max(file_both()$lat),
            lng2 = min(file_both()$lon),lat2 = min(file_both()$lat))
	  })

	  file1InTime <- reactive({
	  	years <- c(input$min_year_choice,input$max_year_choice)
	  	subset(file1(),
	      Year >= years[1] & Year <= years[2])
		})

	  file2InTime <- reactive({
	  	years <- c(input$min_year_choice,input$max_year_choice)
	  	subset(file2(),
	      Year >= years[1] & Year <= years[2])
		})

	  file1InBounds <- reactive({
	  	choiceFile <- input$File1
	  	choiceData <- input$Data1
	    bounds <- input$map_bounds
	    latRng <- range(bounds$north, bounds$south)
	    lngRng <- range(bounds$east, bounds$west)

	    subset(file1InTime(),
	      lat >= latRng[1] & lat <= latRng[2] &
	        lon >= lngRng[1] & lon <= lngRng[2])
	  })

	  file2InBounds <- reactive({
	  	choiceFile <- input$File2
	  	choiceData <- input$Data2
	    bounds <- input$map_bounds
	    latRng <- range(bounds$north, bounds$south)
	    lngRng <- range(bounds$east, bounds$west)

	    subset(file2InTime(),
	      lat >= latRng[1] & lat <= latRng[2] &
	        lon >= lngRng[1] & lon <= lngRng[2])
	  })

	  vect_distrib_file1 <- reactive({
	  	sapply(general_breaks, count, file1InBounds()$Year)
	  	})

	  vect_distrib_file2 <- reactive({
	  	sapply(general_breaks, count, file2InBounds()$Year)
	  	})

	  general_breaks <- hist(plot = FALSE, file_both()$Year, breaks = max(file_both()$Year)-min(file_both()$Year))$breaks

		colorDatafile1 <- file1()$Year
		colorDatafile2 <- file2()$Year


		pal1 <- colorNumeric(palette = c("Reds"), domain = colorDatafile1)
		pal2 <- colorNumeric(palette = c("Blues"), domain = colorDatafile2)

		observe({

			fRadiusfile1 <- 5+5*(1-(file1InTime()[["Year"]]-year_min())/(year_max()+1-year_min()))
			fRadiusfile2 <- 5+5*(1-(file2InTime()[["Year"]]-year_min())/(year_max()+1-year_min()))
		    

		    leafletProxy("map", data = file2InTime()) %>%
		      clearMarkers() %>%
		      addCircleMarkers(~lon, ~lat, radius=fRadiusfile2, layerId=~Year,
		        stroke=FALSE, fillOpacity=0.6, fillColor=pal2(file2InTime()$Year), group="points2")
			leafletProxy("map", data = file1InTime()) %>%
		      addCircleMarkers(~lon, ~lat, radius=fRadiusfile1, layerId=~Year,
		        stroke=FALSE, fillOpacity=0.6, fillColor=pal1(file1InTime()$Year), group="points1")
		      


		    if (input$hide_points1 == FALSE) {
		    	leafletProxy("map") %>% clearGroup("points1")
		    } else {

		    }

		    if (input$hide_points2 == FALSE) {
		    	leafletProxy("map") %>% clearGroup("points2")
		    } else {

		    }

		  })

	  observe({
	    leafletProxy("map") %>% clearPopups()
	    event <- input$map_click
	    output$histfile1 <- renderPlot({
	    hist(file1InBounds()$Year,
	      breaks = general_breaks,
	      main = "Dataset 1",
	      xlab = "Year",
	      ylab = "Number of individuals",
	      col = colorRampPalette(brewer.pal(9, "Reds"))(length(general_breaks)),
	      border = 'white')
	  })

	  output$histfile2 <- renderPlot({
	    hist(file2InBounds()$Year,
	      breaks = general_breaks,
	      main = "Dataset 2",
	      xlab = "Year",
	      ylab = "Number of individuals",
	      col = colorRampPalette(brewer.pal(9, "Blues"))(length(general_breaks)),
	      border = 'white')
	  })

	  output$plot1 <- renderPlot({
	  	par(xpd=TRUE, mar=c(8,4,4,3))
	  	# if (max(vect_distrib_file1())>max(vect_distrib_file2())) {
	  	# 		plot(general_breaks, vect_distrib_file1(),type="n", ylab="Number of individuals",xlab="Year")
	  	# 	} else {
	  	# 		plot(general_breaks, vect_distrib_file2(),type="n", ylab="Number of individuals",xlab="Year")
	  	# 	}
	  	min_plot <- min(min(vect_distrib_file1()),min(vect_distrib_file2()))
	  	max_plot <- max(max(vect_distrib_file1()),max(vect_distrib_file2()))
	  	moy_plot <- (min_plot+max_plot)/2
	  	plot(general_breaks, c(min_plot,matrix(data = moy_plot,nrow=1,ncol=length(general_breaks)-2),max_plot),type="n", ylab="Number of individuals",xlab="Year")

	    lines(general_breaks, vect_distrib_file1(), lty = 1, col = "red")
	    lines(general_breaks, vect_distrib_file2(), lty = 1, col = "blue")
	    legend("top", inset = c(-0.06, -0.20), c("Dataset 1","Dataset 2"),lty=c(1,1),col=c("red","blue"))
	  })

	  output$plot2 <- renderPlot({
	  	points_to_plot <- vect_distrib_file1()/(vect_distrib_file1()+vect_distrib_file2())
	  	par(xpd=TRUE, mar=c(8,4,4,3))
	  	plot(general_breaks, seq(0,1,len=length(general_breaks)),type="n", ylab="Prevalence",xlab="Year")
	   	points(general_breaks, points_to_plot, col = "blueviolet",bg = "blueviolet", pch = 16)

	   	std_err <- function(p,q) sqrt(var(c(rep(0,q),rep(1,p)))/(p+q))
	  	sd <- c()
	  	for (i in 1:length(general_breaks)){
		  	sd <- c(sd,std_err(vect_distrib_file1()[i],vect_distrib_file2()[i]))
		}
	   	segments(general_breaks,points_to_plot-sd,general_breaks,points_to_plot+sd, col = "blueviolet")
		epsilon <- 0.1
		segments(general_breaks-epsilon,points_to_plot-sd,general_breaks+epsilon,points_to_plot-sd, col = "blueviolet")
		segments(general_breaks-epsilon,points_to_plot+sd,general_breaks+epsilon,points_to_plot+sd, col = "blueviolet")

		lines(general_breaks, points_to_plot, lty = 1, col = "blueviolet")
		lines(general_breaks, rep(0.5,length(general_breaks)), lty = 2, col = "blueviolet")
	    lines(general_breaks, rep(nrow(file1InBounds())/(nrow(file2InBounds())+nrow(file1InBounds())),length(general_breaks)), lty = 1, col = "mediumpurple4")
	    legend("top", inset = c(-0.06, -0.20), c("Prevalence (Data1)", "Overall prevalence"),lty=c(1),col=c("blueviolet","mediumpurple4"))
	  })

	    output$info <- renderText({
	    paste0("correl. coeff.=", cor(vect_distrib_file1(),vect_distrib_file2(), method = "pearson"))
	  })

	    output$info2 <- renderText({
	    paste0("overall prevalence=", sum(vect_distrib_file1())/sum(vect_distrib_file2()+vect_distrib_file1()))
	  })

	  })

		# Show a popup at the given location
	  showPopup <- function(latt, long) {
	    selectedPointsfile1 <- subset(file1(),lon == long & lat == latt)
	    selectedPointsfile2 <- subset(file2(),lon == long & lat == latt)
	    output$histfile1 <- renderPlot({
	    hist(selectedPointsfile1$Year,
	      breaks = general_breaks,
	      main = "Dataset 1",
	      xlab = "Year",
	      ylab = "Number of individuals",
	      col = colorRampPalette(brewer.pal(9, "Reds"))(length(general_breaks)),
	      border = 'white')
	  })

	  output$histfile2 <- renderPlot({
	    hist(selectedPointsfile2$Year,
	      breaks = general_breaks,
	      main = "Dataset 2",
	      xlab = "Year",
	      ylab = "Number of individuals",
	      col = colorRampPalette(brewer.pal(9, "Blues"))(length(general_breaks)),
	      border = 'white')
	  })
	    content <- as.character(tagList(
	      tags$h6("Lat:",latt),
	      tags$h6("Lon:",long)
	    ))
	    leafletProxy("map") %>% addPopups(long, latt, content, layerId = file_both())
	  }

	  # When map is clicked, show a popup with location info
	  observe({
	    leafletProxy("map") %>% clearPopups()
	    event <- input$map_marker_click
	    if (is.null(event))
	      return()
	    isolate({
	      showPopup(event$lat, event$lng)
	    })
	  })

	showPopupRect <- function(id,latt, long) {

		w <- get_rect(latt,long, input$nb_bins_corr)
		vect_distrib_file1_new <- sapply(general_breaks, count, subset(file1InBounds(),lat >= w[1] & lat <= w[2] & lon >= w[3] & lon <= w[4])$Year)
		vect_distrib_file2_new <- sapply(general_breaks, count, subset(file2InBounds(),lat >= w[1] & lat <= w[2] & lon >= w[3] & lon <= w[4])$Year)
		output$plot1 <- renderPlot({
		  	par(xpd=TRUE, mar=c(8,4,4,3))
		  	if (max(vect_distrib_file1_new)>max(vect_distrib_file2_new)) {
		  			plot(general_breaks, vect_distrib_file1_new,type="n", ylab="Number of individuals",xlab="Year")
		  		} else {
		  			plot(general_breaks, vect_distrib_file2_new,type="n", ylab="Number of individuals",xlab="Year")
		  		}
		    lines(general_breaks, vect_distrib_file1_new, lty = 1, col = "red")
		    lines(general_breaks, vect_distrib_file2_new, lty = 1, col = "blue")
		    legend("top", inset = c(-0.06, -0.20), c("Dataset 1","Dataset 2"),lty=c(1,1),col=c("red","blue"))
	  	})
	  	if (id[1] >= 2){
	  			h_corr_prev <- id[1] - 2
	  		} else {
	  			h_corr_prev <- id[1]
	  		}
	  	output$plot2 <- renderPlot({
		  	par(xpd=TRUE, mar=c(8,4,4,3))
		  	plot(general_breaks, seq(0,1,len=length(general_breaks)),type="n", ylab="Prevalence",xlab="Year")
		  	points(general_breaks, vect_distrib_file1_new/(vect_distrib_file1_new+vect_distrib_file2_new), col = "blueviolet",bg = "blueviolet", pch = 16)
		    lines(general_breaks, vect_distrib_file1_new/(vect_distrib_file1_new+vect_distrib_file2_new), lty = 1, col = "blueviolet")
		    lines(general_breaks, rep(h_corr_prev,length(general_breaks)), lty = 1, col = "mediumpurple4")
		    lines(general_breaks, rep(0.5,length(general_breaks)), lty = 2, col = "blueviolet")
		    legend("top", inset = c(-0.06, -0.20), c("Prevalence (Data1)", "Overall prevalence"),lty=c(1),col=c("blueviolet","mediumpurple4"))
		  })

	  	output$info <- renderText({
	    	paste0("correl. coeff.=", cor(vect_distrib_file1_new,vect_distrib_file2_new, method = "pearson"))
	  	})
	  	output$info2 <- renderText({
	    	paste0("overall prevalence=", sum(vect_distrib_file1_new)/sum(vect_distrib_file2_new+vect_distrib_file1_new))
	  	})

	  	if (id[1] >= 2){
	  			content <- as.character(tagList(
			      tags$h6("Prevalence:", id[1]-2)
			    ))
	  		} else {
			    content <- as.character(tagList(
			      tags$h6("Correlation:", id[1])
			    ))
			}
	    leafletProxy("map") %>% addPopups(long, latt, content, layerId = file_both())
	  }

	  # When map is clicked, show a popup with rectangle info
	  observe({
	    leafletProxy("map") %>% clearPopups()
	    event <- input$map_shape_click
	    if (is.null(event) | is.null(event$id))
	      return()
	    isolate({
	      showPopupRect(event$id, event$lat, event$lng)
	    })
	  })

####### Pearson per bin #################################################

	  get_rect <- function(latt, long, nb_bins) {
	  	cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  	cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)
	  	for (i in seq(1,nb_bins,1)){
	  		if (latt >= cut_lat[i] & latt <= cut_lat[i+1]){
	  			lat1 <- cut_lat[i]
	  			lat2 <- cut_lat[i+1]
	  		}
	  	}
	  	for (j in seq(1,nb_bins,1)){
	  		if (long >= cut_lon[j] & long <= cut_lon[j+1]){
	  			lon1 <- cut_lon[j]
	  			lon2 <- cut_lon[j+1]
	  		}
	  	}
	  	c(lat1,lat2,lon1,lon2)
	  }

	  mat_correlation_points <- reactive({
	  	nb_bins <- ifelse(is.na(input$nb_bins_corr) | input$nb_bins_corr == 0,2,input$nb_bins_corr)
	  	cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  	cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)
	  	for (i in seq(1,nb_bins,1)){
	  		for (j in seq(1,nb_bins,1)){
	  			list_file1 <- subset(file1InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
	  			list_file2 <- subset(file2InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
	  			for (year in general_breaks){
	  				if (year == general_breaks[1]){
	  					vectfile1 <- sum(list_file1$Year == year)
	  					vectfile2 <- sum(list_file2$Year == year)
					} else {
						vectfile1 <- append(vectfile1, sum(list_file1$Year == year))
						vectfile2 <- append(vectfile2, sum(list_file2$Year == year))
					}
	  			}
		  		if (i == 1 & j == 1){
		  			matrix_final <- ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson"))
		  		} else {
		  			matrix_final <- append(matrix_final, ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson")))
		  		}
	  		}
	  	}
	  	matrix(matrix_final,nrow = nb_bins, ncol = nb_bins, byrow = TRUE)
	  })

	  observe({
			choiceCorr <- input$temporal_corr
			nb_bins <- ifelse(is.na(input$nb_bins_corr) | input$nb_bins_corr == 0,2,input$nb_bins_corr)
	  		cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  		cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)    
		    if (choiceCorr == TRUE) {
		    	leafletProxy("map") %>% clearGroup("rect")
		    	for (i in seq(1,nb_bins,1)){
		    		for (j in seq(1,nb_bins,1)){
		    			coeff = max(0,mat_correlation_points()[i,j])
		    			if (coeff != 0){
				  			leafletProxy("map") %>%
				  			addRectangles(lng1=cut_lon[j], lat1=cut_lat[i],
									lng2=cut_lon[j+1], lat2=cut_lat[i+1],
									fillColor = rgb(1-coeff,1-coeff/2,1-coeff),
									weigh = 0, fillOpacity = 0.6,
									layerId=coeff, group="rect")
				  		}
		    		}
		    	}
		    } else {
		    	leafletProxy("map") %>% clearGroup("rect")
			}
		  })

####### Incidence per bin ###############################################

	  mat_prevalence_points <- reactive({
	  	nb_bins <- ifelse(is.na(input$nb_bins_prev) | input$nb_bins_prev == 0,2,input$nb_bins_prev)
	  	cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  	cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)
	  	for (i in seq(1,nb_bins,1)){
	  		for (j in seq(1,nb_bins,1)){
	  			list_file1 <- subset(file1InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
	  			list_file2 <- subset(file2InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
		  		if (i == 1 & j == 1){
		  			matrix_final <- ifelse(nrow(list_file1)==0 & nrow(list_file2)==0,0,nrow(list_file1)/(nrow(list_file2)+nrow(list_file1)))
		  		} else {
		  			matrix_final <- append(matrix_final, ifelse(nrow(list_file1)==0 & nrow(list_file2)==0,0,nrow(list_file1)/(nrow(list_file2)+nrow(list_file1))))
		  		}
	  		}
	  	}
	  	matrix(matrix_final,nrow = nb_bins, ncol = nb_bins, byrow = TRUE)
	  })

	  observe({
			choicePrev <- input$temporal_prev
			nb_bins <- ifelse(is.na(input$nb_bins_prev) | input$nb_bins_prev == 0,2,input$nb_bins_prev)
	  		cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  		cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)    
		    if (choicePrev == TRUE) {
		    	leafletProxy("map") %>% clearGroup("rect")
		    	for (i in seq(1,nb_bins,1)){
		    		for (j in seq(1,nb_bins,1)){
		    			coeff = max(0,mat_prevalence_points()[i,j])
		    			if (coeff != 0){
				  			leafletProxy("map") %>%
				  			addRectangles(lng1=cut_lon[j], lat1=cut_lat[i],
									lng2=cut_lon[j+1], lat2=cut_lat[i+1],
									fillColor = rgb(1-coeff/2,1-coeff,1),
									weigh = 0, fillOpacity = 0.6,
									layerId=(2+coeff), group="rect")
				  		}
		    		}
		    	}
		    } else {
		    	leafletProxy("map") %>% clearGroup("rect")
			}
		  })

######## Min/Max year ###################################################


	  observe({
			choicePrev1 <- input$min_max_data1
			choicePrev2 <- input$min_max_data2
			nb_bins <- ifelse(is.na(input$nb_bins_min_max) | input$nb_bins_min_max == 0,2,input$nb_bins_min_max)
	  		cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
	  		cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)    
		    if (choicePrev1 == TRUE) {
		    	leafletProxy("map") %>% clearGroup("rect")
		    	for (i in seq(1,nb_bins,1)){
		    		for (j in seq(1,nb_bins,1)){
		    			list_file1 <- subset(file1InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
			    		if (nrow(list_file1) > 0) {	
			    			coeff_min = (min(list_file1$Year) - year_min())/(year_max() - year_min())
			    			coeff_max = (max(list_file1$Year) - year_min())/(year_max() - year_min())
				  			leafletProxy("map") %>%
				  			addPolygons(lng=c(cut_lon[j],cut_lon[j+1],cut_lon[j]), lat=c(cut_lat[i],cut_lat[i],cut_lat[i+1]),
									# lng2=cut_lon[j+1], lat2=cut_lat[i+1],
									fillColor = rgb(1-coeff_min/2,1-coeff_min,1-coeff_min),
									weigh = 0, fillOpacity = 0.6, group="rect") %>%
				  			 addPolygons(lng=c(cut_lon[j+1],cut_lon[j+1],cut_lon[j]), lat=c(cut_lat[i+1],cut_lat[i],cut_lat[i+1]),
									# lng2=cut_lon[j+1], lat2=cut_lat[i+1],
									fillColor = rgb(1-coeff_max/2,1-coeff_max,1-coeff_max),
									weigh = 0, fillOpacity = 0.6, group="rect")
				  		}
		    		}
		    	}
		    } else {
		    	if (choicePrev2 == TRUE) {
			    	leafletProxy("map") %>% clearGroup("rect")
			    	for (i in seq(1,nb_bins,1)){
			    		for (j in seq(1,nb_bins,1)){
			    			list_file2 <- subset(file2InTime(),lat >= cut_lat[i] & lat <= cut_lat[i+1] & lon >= cut_lon[j] & lon <= cut_lon[j+1])
				    		if (nrow(list_file2) > 0) {	
				    			coeff_min = (min(list_file2$Year) - year_min())/(year_max() - year_min())
				    			coeff_max = (max(list_file2$Year) - year_min())/(year_max() - year_min())
					  			leafletProxy("map") %>%
					  			addPolygons(lng=c(cut_lon[j],cut_lon[j+1],cut_lon[j]), lat=c(cut_lat[i],cut_lat[i],cut_lat[i+1]),
										# lng2=cut_lon[j+1], lat2=cut_lat[i+1],
										fillColor = rgb(1-coeff_min,1-coeff_min,1-coeff_min/2),
										weigh = 0, fillOpacity = 0.6, group="rect") %>%
					  			 addPolygons(lng=c(cut_lon[j+1],cut_lon[j+1],cut_lon[j]), lat=c(cut_lat[i+1],cut_lat[i],cut_lat[i+1]),
										# lng2=cut_lon[j+1], lat2=cut_lat[i+1],
										fillColor = rgb(1-coeff_max,1-coeff_max,1-coeff_max/2),
										weigh = 0, fillOpacity = 0.6, group="rect")
					  		}
			    		}
			    	}
		    	} else {
		    		leafletProxy("map") %>% clearGroup("rect")
				}
			}
		  })

####### MultiSampling ###################################################
	  
	  # id, buf_list, buf_list_0, buf_list_1
	  data_sum <- data.frame()
	  file1_init_order <- file1_init()[order(file1_init()$Year),]
	  n_ind = length(unique(file1_init()$Id[duplicated(file1_init()$Id)]))
	  n <- 0
	  for (id in unique(file1_init_order$Id[duplicated(file1_init_order$Id)])) {
	  		buf_list <- subset(na.omit(file1_init()), Id == id)
			buf_list <- subset(buf_list, Status == "0" | Status == "1")
			if (nrow(buf_list) > 0) {
				buf_list <- buf_list[order(buf_list$Year),]
				buf_list$Individual <- n
				n <- n + 1/n_ind
		  	}
		  	data_sum <- rbind(data_sum,buf_list)
	  	}

	  output$plot_multi <- renderPlot ({
	  	factor_curve = 0
	  	par(xpd=TRUE, mar=c(8,4,4,3))
	  	g <- qplot(general_breaks, general_breaks, type="n",
	  		xlab = expression(paste("Year 1" %->% "0")), ylab = expression(paste("Year 0" %->% "1")),
	  		scale_x_discrete(breaks=general_breaks),scale_y_discrete(breaks=general_breaks),
	  		geom="blank")
	  	g <- g + geom_abline(slope=1,lty = 2, col = "darkolivegreen")
	  	nb_ind_dupl <- 1+length(unique(file1_init()$Id[duplicated(file1_init()$Id)]))
	  	for (id in unique(file1_init()$Id[duplicated(file1_init()$Id)])) {
  			years_tot <- c(NA,NA)
			buf_list <- subset(na.omit(file1_init()), Id == id)
			buf_list <- subset(buf_list, Status == "0" | Status == "1")
			buf_list <- buf_list[order(buf_list$Year),]
			state <- buf_list[1,]$Status
			year_state <- buf_list[1,]$Year
			if (nrow(buf_list) > 1) {
  				for (i in seq(2,nrow(buf_list),1)){
  					if (buf_list[i,]$Status != state){
  						if (state == 0) {
							years_tot <- rbind(years_tot,c(year_state,buf_list[i,]$Year))
						} else {
							years_tot <- rbind(years_tot,c(buf_list[i,]$Year,year_state))
						}
						year_state <- buf_list[i,]$Year
						state <- buf_list[i,]$Status
					} else {
						year_state <- buf_list[i,]$Year
					}
  				}
  			}
  			if (!is.null(nrow(years_tot))){
				years_tot_frame <- data.frame(years_tot)
	  			colnames(years_tot_frame) <- c("X","Y")
	  			years_tot_frame$X <- years_tot_frame$X + runif(1,-0.06,0.06)
	  			years_tot_frame$Y <- years_tot_frame$Y + runif(1,-0.06,0.06)
  				if (nrow(years_tot) > 1) {
	  				g <- g + geom_point(aes(x=X,y=Y),years_tot_frame,col = "darkolivegreen",pch=16,group="INDEX")
	  			}
				if (nrow(years_tot) > 2) {
					factor_curve <- factor_curve+(1/nb_ind_dupl)*0.1
					myCurve<-curveGrob(0, 0, 1, 1, default.units = "npc",
			               curvature = -0.1-factor_curve, angle = 30, ncp = 10, shape = 1,
			               square = FALSE, squareShape = 1,
			               inflect = FALSE, arrow = arrow(), open = TRUE,
			               debug = FALSE, 
			               name = NULL, gp = gpar(col="darkolivegreen"), vp = NULL)
					for (i in seq(2,nrow(years_tot)-1,1)) {
						if (years_tot[i,1] != years_tot[i,2] | years_tot[i+1,1] != years_tot[i+1,2]) {
							g <-  g + annotation_custom(grob=myCurve,years_tot_frame[i,1],years_tot_frame[i+1,1],years_tot_frame[i,2],years_tot_frame[i+1,2])
						}
					}
				}
			}
		}
		legend("top", inset = c(-0.06, -0.20), c("Changes of state"),lty=c(1),col=c("darkolivegreen"), pch=c(16))
		g <- g #+ geom_density2d()
		g
	  })

	  ranges <- reactiveValues(x = NULL, y = NULL)

	  	output$plot_multi2 <- renderPlot ({
	  	g <- ggplot(file1_init(),aes(Years,Individuals),type="n",
	  		xlab = "Years", ylab = "Individuals", yaxt='n',geom="blank")
	  		n_ind = length(unique(file1_init()$Id[duplicated(file1_init()$Id)]))
	  	g <- g + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
	  	g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	  				   panel.background = element_blank(), axis.line = element_line(colour = "black"))
	  	g <- g + geom_point(data = subset(data_sum,Status==0),aes(Year,Individual), col="blue",bg="blue",pch=16)
	  	g <- g + geom_point(data = subset(data_sum,Status==1),aes(Year,Individual), col="red",bg="red",pch=16)
		g <- g + geom_line(data=data_sum,aes(Year,Individual,group=Id), col="darkolivegreen",lty=1)	
		g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
	  	g
	  })

	observeEvent(input$plot_multi2_dblclick, {
	    brush <- input$plot_multi2_brush
	    if (!is.null(brush)) {
	      ranges$x <- c(brush$xmin, brush$xmax)
	      ranges$y <- c(brush$ymin, brush$ymax)
	    } else {
	  	  ranges$x <- NULL
	      ranges$y <- NULL
	    }
 	})

####### HEATMAPS ########################################################

	  # creates the matrices for the heatmap
	  # lat heatmap
	  mat_years_lat <- reactive({
	  	nb_bins <- input$nb_bins_temp
		cut_lat <- seq(min(file_both()$lat),max(file_both()$lat),length.out = nb_bins+1)
		for (year1 in general_breaks){
			vectfile1 <- sum(file1()[which(file1()$lat >= cut_lat[1] & file1()$lat <= cut_lat[2]),]$Year == year1)
			for (i in seq(2,length(cut_lat)-1,1)){
					vectfile1 <- append(vectfile1,sum(file1()[which(file1()$lat > cut_lat[i] & file1()$lat <= cut_lat[i+1]),]$Year == year1))
				}
			for (year2 in general_breaks){			
				vectfile2 <- sum(file2()[which(file2()$lat >= cut_lat[1] & file2()$lat <= cut_lat[2]),]$Year == year2)		
				for (i in seq(2,length(cut_lat)-1,1)){
					vectfile2 <- append(vectfile2,sum(file2()[which(file2()$lat > cut_lat[i] & file2()$lat <= cut_lat[i+1]),]$Year == year2))
				}
				if (year1 == general_breaks[1] & year2 == general_breaks[1]){
					list_temp <- ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson"))
				} else {
					list_temp <- append(list_temp,ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson")))
				}
			}
		}
		matrix(list_temp,nrow = length(general_breaks),ncol = length(general_breaks), byrow = TRUE, dimnames = list(general_breaks,general_breaks))
	  })

	#long heatmap
	mat_years_lon <- reactive({
	  nb_bins <- input$nb_bins_temp
		cut_lon <- seq(min(file_both()$lon),max(file_both()$lon),length.out = nb_bins+1)
		for (year1 in general_breaks){
			vectfile1 <- sum(file1()[which(file1()$lon >= cut_lon[1] & file1()$lon <= cut_lon[2]),]$Year == year1)
			for (i in seq(2,length(cut_lon)-1,1)){
					vectfile1 <- append(vectfile1,sum(file1()[which(file1()$lon > cut_lon[i] & file1()$lon <= cut_lon[i+1]),]$Year == year1))
				}
			for (year2 in general_breaks){			
				vectfile2 <- sum(file2()[which(file2()$lon >= cut_lon[1] & file2()$lon <= cut_lon[2]),]$Year == year2)		
				for (i in seq(2,length(cut_lon)-1,1)){
					vectfile2 <- append(vectfile2,sum(file2()[which(file2()$lon > cut_lon[i] & file2()$lon <= cut_lon[i+1]),]$Year == year2))
				}
				if (year1 == general_breaks[1] & year2 == general_breaks[1]){
					list_temp <- ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson"))
				} else {
					list_temp <- append(list_temp,ifelse(is.na(cor(vectfile1,vectfile2, method = "pearson")),0,cor(vectfile1,vectfile2, method = "pearson")))
				}
			}
		}
		matrix(list_temp,nrow = length(general_breaks),ncol = length(general_breaks), byrow = TRUE, dimnames = list(general_breaks,general_breaks))
	  })

		dendrogram <- reactive({
		  	if (input$dendrogram == TRUE) {
		  		"both"
		  	} else {
		  		"none"
		  	}
		  	})

		output$hm_lat <- renderPlotly({
			heatmaply(heatmapr(mat_years_lat(),dendrogram=dendrogram()),xlab="Year (data 2)",ylab="Year (data 1)",main="Latitude correlation", margins=c(70,70,NA,0))
		})
		output$hm_lon <- renderPlotly({
			heatmaply(heatmapr(mat_years_lon(),dendrogram=dendrogram()),xlab="Year (data 2)",ylab="Year (data 1)",main="Longitude correlation", margins=c(70,70,NA,0))
		})

####### Data Explorer ###################################################

	  output$file1_table <- DT::renderDataTable({
	    df <- file1_init() %>%
	      filter(
	        Year >= input$minYear1,
	        Year <= input$maxYear1,
	        Status >= ifelse(input$Status1 == 0, 0, ifelse(input$Status1 == 1, 1, 0)),
	        Status <= ifelse(input$Status1 == 0, 0, ifelse(input$Status1 == 1, 1, 1))
	      ) #%>%
	    #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-lng="', lon, '"><i class="fa fa-crosshairs"></i></a>',  sep=""))
	    action <- DT::dataTableAjax(session, df)

	    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
	  })

	  output$file2_table <- DT::renderDataTable({
	    df2 <- file2_init() %>%
	      filter(
	        Year >= input$minYear2,
	        Year <= input$maxYear2,
	        Status >= ifelse(input$Status2 == 0, 0, ifelse(input$Status2 == 1, 1, 0)),
	        Status <= ifelse(input$Status2 == 0, 0, ifelse(input$Status2 == 1, 1, 1))
	      ) #%>%
	    #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-lng="', lon, '"><i class="fa fa-crosshairs"></i></a>',  sep=""))
	    action <- DT::dataTableAjax(session, df2)

	    DT::datatable(df2, options = list(ajax = list(url = action)), escape = FALSE)
	  })


	}

####### Help ############################################################

output$input <- renderImage({
    return(list(
        src = "images/input.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$csv_file <- renderImage({
    return(list(
        src = "images/csv_file.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$map_example <- renderImage({
    return(list(
        src = "images/map.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$min_max <- renderImage({
    return(list(
        src = "images/min_max.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$explore <- renderImage({
    return(list(
        src = "images/explore.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$multi1 <- renderImage({
    return(list(
        src = "images/multi1.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$multi2 <- renderImage({
    return(list(
        src = "images/multi2.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$correl_select <- renderImage({
    return(list(
        src = "images/correl_select.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$distrib_select <- renderImage({
    return(list(
        src = "images/distrib_select.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$prev_select <- renderImage({
    return(list(
        src = "images/prev_select.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$dataset <- renderImage({
    return(list(
        src = "images/dataset.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

output$hm_dendo <- renderImage({
    return(list(
        src = "images/hm_dendo.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)


output$grid <- renderImage({
    return(list(
        src = "images/grid.png",
        contentType = "image/png",
        alt = "Grid",
        height = 300
      ))
  }, deleteFile = FALSE)

	}

	)

}