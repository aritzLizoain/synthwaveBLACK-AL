snippet lib
	library(${1:package})

snippet req
	require(${1:package})

snippet src
	source("${1:file.R}")

snippet ret
	return(${1:code})

snippet mat
	matrix(${1:data}, nrow = ${2:rows}, ncol = ${3:cols})

snippet sg
	setGeneric("${1:generic}", function(${2:x, ...}) {
		standardGeneric("${1:generic}")
	})

snippet sm
	setMethod("${1:generic}", ${2:class}, function(${2:x, ...}) {
		${0}
	})

snippet sc
	setClass("${1:Class}", slots = c(${2:name = "type"}))

snippet if
	if (${1:condition}) {
		${0}
	}

snippet el
	else {
		${0}
	}

snippet ei
	else if (${1:condition}) {
		${0}
	}

snippet fun
	${1:name} <- function(${2:variables}) {
		${0}
	}

snippet for
	for (${1:variable} in ${2:vector}) {
		${0}
	}

snippet while
	while (${1:condition}) {
		${0}
	}

snippet switch
	switch (${1:object},
		${2:case} = ${3:action}
	)

snippet apply
	apply(${1:array}, ${2:margin}, ${3:...})

snippet lapply
	lapply(${1:list}, ${2:function})

snippet sapply
	sapply(${1:list}, ${2:function})

snippet mapply
	mapply(${1:function}, ${2:...})

snippet tapply
	tapply(${1:vector}, ${2:index}, ${3:function})

snippet vapply
	vapply(${1:list}, ${2:function}, FUN.VALUE = ${3:type}, ${4:...})

snippet rapply
	rapply(${1:list}, ${2:function})

snippet ts
	`r paste("#", date(), "------------------------------\n")`

snippet shinyapp
	library(shiny)
	
	ui <- fluidPage(
	  ${0}
	)
	
	server <- function(input, output, session) {
	  
	}
	
	shinyApp(ui, server)

snippet shinymod
	${1:name}UI <- function(id) {
	  ns <- NS(id)
	  tagList(
		${0}
	  )
	}
	
	${1:name}Server <- function(id) {
	  moduleServer(
	    id,
	    function(input, output, session) {
	      
	    }
	  )
	}
	
snippet my_template
	######################################
	#                                    #
	#        PROJECT TITLE               #
	#                                    #
	#                                    #
	#   Author: Aritz Lizoain            #
	#                                    #
	#   Date created: xx.xx.20xx         #
	#   Date updated: xx.xx.20xx         #
	#                                    #
	#                                    #
	######################################
	
	#' *Load libraries & functions*
	# Load libraries if not already loaded
	if (!require("dplyr")) library("dplyr") # data manipulation
	if (!require("purrr")) library("purrr") # functionality
	if (!require("styler")) library("styler") # style code
	
	# Source utility functions from the utils folder
	file_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
	function_path <- paste0(file_path, "/utils/")
	invisible(sapply(list.files(pattern = "[.]R$", path = function_path, full.names = TRUE), source))
	
	#' *Always run, even sourced*
	# Example function
	my_function <- function(x, y, ...) {
			z <- sum(c(x, y, ...))
			return(z)
	}
	
	#' *Not run when sourced*
	if (sys.nframe() == 0 || sys.nframe() == 4) {
			# invisible()
			# Example code
			my_list <- list(two = c(1, 2), four = c(1, 2, 3, NA))
			result <- my_list %>% map(sum, na.rm = TRUE)
	}
	
	#' *Tidy style*
	style_file(utils::getSrcFilename(function() {}, full.names = TRUE))
	
snippet my_load_library
	if (!require("${1:library_name}")) library("${1:library_name}") # ${2:library function}

snippet my_load_utils
	# Source utility functions from the utils folder
	function_path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/utils/")
	invisible(sapply(list.files(pattern = "[.]R$", path = function_path, full.names = TRUE), source))

snippet my_get_script_location
	dirname(rstudioapi::getActiveDocumentContext()$path)

snippet my_trycatch
	${1:variable} <- tryCatch({
		# Try to ${2:do this}
		${3:try to do this}
	}, warning = function(w) {
		message("There was a warning trying to ${2:do this}")
		message("Here's the original warning message:")
		message(w)
	}, error = function(e) {
		message("There was an error trying to ${2:do this}")
		message("Here's the original error message:")
		message(e)
		message("\nNA returned")
		# Choose a return value in case of error
		return(NA)
	}, finally = {
		${4}
	})
	
	

snippet my_section
	#' *${1:section name}*

snippet my_gif_creator
	if (!require("animation")) library("animation") # Plots to gif
	saveGIF(
			{
					for (${1:i} in ${2:x}) {
					
					# do something

					# Show figure
						
					}
			},
			movie.name = paste0(${3:saving directory}, ".gif"),
			interval = ${4:in seconds},
			ani.height = ${5:in pixels},
			ani.width = ${6:in pixels}
			)

snippet my_style_formatter
	if (!require("styler")) library("styler")
	#' *Tidy style*
	style_file(utils::getSrcFilename(function() {}, full.names = TRUE))

snippet my_base_plot
	plot(${1:y} ~ ${2:x},
		ylab = "${3}",
		xlab = "${4}",
		pch = 20,
		main = "${6}",
		col = "${7}"
	)

snippet my_legend_1
	legend("${1:bottomright}", legend = c("${2:name on legend}"), col = c("${3:red}"), pch = 16, lty = 1, lwd = 2, cex = 0.9)

snippet my_legend_2
	legend("${1:bottomright}", legend = c("${2:name on legend}", "${3:name on legend}"), col = c("${4:red}", "${5:red}"), pch = 16, lty = 1, lwd = 2, cex = 0.9)

snippet my_legend_3
	legend("${1:bottomright}", legend = c("${2:name on legend}", "${3:name on legend}", "${4:name on legend}"), col = c("${5:red}", "${6:red}", "${7:red}"), pch = 16, lty = 1, lwd = 2, cex = 0.9)

snippet my_legend_4
	legend("${1:bottomright}", legend = c("${2:name on legend}", "${3:name on legend}", "${4:name on legend}", "${5:name on legend}"), col = c("${6:red}", "${7:red}", "${8:red}", "${9:red}"), pch = 16, lty = 1, lwd = 2, cex = 0.9)

snippet my_time_in_range_mg_dl
	 sum((${1:glucose values}>=70 & ${1:glucose values}<=180)) / length(${1:glucose values}) * 100

snippet my_time_in_range_mmol/l
	sum((${1:glucose values}>=3.9 & ${1:glucose values}<=10)) / length(${1:glucose values}) * 100

snippet my_fun
	${1:name} <- function(${2:variables}, ...) {
			# _${3:summary}_

			# Args:
						# ${2}: _${4:description}_

			# Returns:
						# ${5:output}: _${6:description}_

		${0}
	}

snippet my_plot_table
	# Plot df and color cells under condition
	# Table theme
	my_theme <- ttheme_default(
	core = list(fg_params = list(fontface = 3)),
	colhead = list(fg_params = list(fontface = 4L)),
	rowhead = list(fg_params = list(col = "white")) # row numbers on the left side
	)
	# Table title and note
	my_title <- paste0("FDA CGM criteria. Calibrated at ", calibration_concentration, "mmol/L")
	my_note <- "Lower one-sided 95% confidence bound (BCa method)"
	# Create plot
	g <- tableGrob(results,
	theme = my_theme
	)
	# Color cells based on condition
	greens <- list(inside = "darkolivegreen1", border = "darkolivegreen4")
	cells_to_color <- which(CONDITION, arr.ind = TRUE)
	# Function to color a cell given a row and col number
	color_cell <- function(table, row, col) {
	l <- table$layout
	which(l$t == row + 1 & l$l == col + 1 & l$name == "core-bg")
	}
	# Apply coloring function to all cells to color
	if (!empty(cells_to_color)) {
	for (cell in 1:nrow(cells_to_color)) {
	    coordinates <- cells_to_color[cell, ]
	    ind <- color_cell(table = g, row = coordinates[1], col = coordinates[2] + 1) # +1 in column because the RANGE column does not count
	    g$grobs[ind][[1]][["gp"]] <- gpar(fill = greens$inside, col = greens$border, lwd = 5)
	}
	}
	# Show plot
	grid.arrange(g,
	top = textGrob(my_title, vjust = 16),
	bottom = textGrob(
	    my_note,
	    gp = gpar(fontface = 3, fontsize = 11),
	    vjust = -18
	)
	)

snippet pipe
	%>%
	
snippet my_dplyr_summary
	# mutate() adds new variables that are functions of existing variables
	# select() picks variables based on their names
	# filter() picks cases based on their values
	# summarise() reduces multiple values down to a single summary
	# arrange() changes the ordering of the rows
	
	# These all combine naturally with group_by() which allows you to perform any operation “by group”

snippet my_pie_donut
	PieDonut(${1:dataframe},
		aes(pies = ${2:primary divider category}, donuts = ${3:secondary divider category}, count = ${4:counts}),
		ratioByGroup = FALSE,
		labelposition = 0,
		title = ${4:title},
		explode = 0,
		# showRatioThreshold=0.0001,
		# selected=c(1,2),
		# explodeDonut=TRUE,
		# r0=0,
		r1 = 1, r2 = 1.5,
		showPieName = FALSE
	)
	
snippet my_save_image_as_pdf
	pdf(paste0(${1:saving directory}, "${2:image name}.pdf"))
	# Plot image
	dev.off()
	
snippet my_gg_density_plot
	ggplot(${1:dataframe}, aes(
		x = ${2:x axis variable},
		y = ${3:y axis categorical variable},
		fill = after_stat(x)
	)) +
		geom_density_ridges_gradient(
			rel_min_height = 0.005,
			scale = 1.1,
			jittered_points = TRUE,
			position = position_points_jitter(
				width = 0.5,
				height = 0
			),
			point_size = 2,
			point_shape = 20,
			alpha = 0.5,
			quantile_lines = TRUE,
			quantiles = 2
		) +
		scale_fill_gradient(
			low = "${4:lower color value}",
			high = "${5:higher color value}",
			limits = c(0, 100)
		) +
		labs(
			x = "${6:x axis label}",
			y = "${7:y axis label}",
			title = "${8:title}""
		) +
		theme(legend.position = "none") +
		xlim(0, 100)
		
snippet my_gg_spaghetti_plot
	ggplot(${1:dataframe}, aes(x = ${2}, y = ${3}, color = ${4:individual id})) +
		geom_line() +
		theme_classic() +
		theme(legend.position = "none") +
		geom_smooth(data = by_month, method = "gam", se = TRUE, colour = "black", size = 2) +
		labs(
			x = "${5:x axis label}",
			y = "${6:y axis label}",
			title = "${7:title}",
			subtitle = "Black line: generalized additive model (GAM) fit of ALL users\nGray area: confidence interval"
		) +
		theme(legend.position = "none", plot.subtitle = element_text(face = "italic", color = "gray48")) +
		scale_x_continuous(breaks = 1:max(${2}), limits = c(1, max(${2}))) +
		geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1.1)

snippet my_gg_boxplot
	${1:dataframe} %>%
		ggplot(aes(x=${2:categorical},y=${3}, fill=interaction(${4:groups categorical}))) +
		geom_boxplot() +
		ggtitle("${5}") +
		xlab("${6}")+
		ylab("${7}")+
		facet_wrap(~${4:groups categorical},ncol = length(unique(${4:groups categorical}))) +
		theme(legend.position="none", axis.text=element_text(size=8), plot.title = element_text(size=12), axis.title=element_text(size=10), axis.text.x = element_text(angle = 0, hjust = 0.5)) +
		facet_grid(~group, scales = "free_x", space = "free") 
		
snippet my_gg_scatterplot
	ggplot(${1:dataframe}, aes(x = ${2},
	               y = ${3},
	               color = ${4:groups categorical},
	               shape=${4:groups categorical})) +
	  
	  geom_point(position = position_jitter(w = 0, h = 0.08)) +
	  
	  labs(title="${5}",
	       x ="${6}",
	       y = "${7}") +
	  
	  theme(axis.text=element_text(size=8), 
	        plot.title = element_text(size=12), 
	        axis.title=element_text(size=10), 
	        legend.title = element_text(size=10),
	        legend.text = element_text(size=9)) +
	  
	  scale_y_continuous(breaks = c(1,2,5,10,20,30),
	                   labels = c(1,2,5,10,20,30)) + 
	  
	  suppressWarnings(geom_line(data=${8:line values (e.g. means)}, aes(x=${9}, y=${10}), lwd=1.0, col='darkred'))
			