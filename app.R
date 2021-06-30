rm(list=ls())
library(shiny)
library(dplyr)
library(factoextra)
library(NbClust)
library(shinyBS)
library(TSP)
library(Rtsne)
source("data_processing.R")

library(spotifyr)
library(shiny)
Sys.setlocale("LC_ALL", "Hebrew")
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_REDIRECT_URI= "http://localhost//")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(tags$style('
     #my_tooltip {
      position: absolute;
      width: 350px;
      z-index: 100;
     }
  ')),
    tags$script('
    $(document).ready(function(){
      // id of the plot
      $("#plot1").mousemove(function(e){ 

        // ID of uiOutput
        $("#my_tooltip").show();         
        $("#my_tooltip").css({             
          top: (e.pageY + 5) + "px",             
          left: (e.pageX + 5) + "px"         
        });     
      });     
    });
  '),
    uiOutput("my_tooltip"),
    
    
    
    # App title ----
    titlePanel("Ido Visulaization project"),
    
    # Sidebar layout with input and output definitions ----
    fluidRow(
        
        # Sidebar panel for inputs ----
        column(width = 3,
               
               
               
               # Elbow graph and under it choose number of clusters according to it
               plotOutput("elbowplot", width='80%', height="250"),
               # bsTooltip("elbowplot", "The optimal number of cluster is the point
               #    of the elbow",
               #           placement = "right"
               #           , options = list(container = "body")),
               numericInput(
                   inputId = "n_cluster",
                   label="choose # clusters",
                   value =5,
                   min = 1,
                   max = 10,
                   step = 1,
                   width = "60"
                   
               ),
               tags$hr(),
               
               radioButtons(inputId = "color",
                            label = "color by",
                            choices = c(cluster = "cluster",
                                        release_year = "release_year"
                            )
               ),
               
               selectInput(inputId ="method",
                           label = "dim reduction method",
                           choices = c("PCA", "T_sne"),
                           width = "100"),
               plotOutput(outputId = "screenPlot", width='80%', height="250"),
               # bsTooltip("screenPlot", "Explains the precentage of explained variance of
               #    each principle component of the PCA algorithm ",placement = "right"
               #           , options = list(container = "body"))
               
               
        ),
        
        # Main panel for displaying outputs ----
        
        column(width = 6,
               
               # Output: Data file ----
               
               verbatimTextOutput("print",placeholder = TRUE),
               plotOutput("plot1",
                          click = "plot_click",
                          dblclick = "plot_dblclick",
                          hover = "plot_hover",
                          brush = "plot_brush",
                          height="800"),
               bsTooltip("plot1", "Hover over the plot to see track information. brush an
                  area to zoom (double click to zoom on the main plot) "
                         , options = list(container = "body"))
               
               
        ),
        
        column(width = 3,
               
               actionButton(inputId = "playlist_button", label = "create Playlist"),
             
               
               plotOutput(outputId = "miniMain",
                          hover = "mini_plot_hover",
                          width='80%', height="250"),
             
               actionButton(inputId = "reset", label = "reset the plot"),
               htmlOutput("album_img")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    addTooltip(session=session,id="elbowplot",title="The optimal number 
    of cluster is the point of the elbow")
    addTooltip(session=session,id="screenPlot",title="Explains the precentage
               of explained variance ofeach principle 
               component of the PCA algorithm ")
    addTooltip(session=session,id="plot1",title= " PCA \ T-SNE dimension
    reduction of the spotify saved track data.
    Hover over the plot to see track information. brush an 
    area and double click to zoom, 
               double click on empty area to reset")
    addTooltip(session=session,id="playlist_button",title="creater a playlist of
               from the brushed area. the playlist order is the solution of the
               'Traveling sales man problem'. a link to the playlist on spotify will be
               displayed. (not working in the demo!)")

    

    
    
    plot_var <- reactiveValues(mainPlot = NULL)
    hover_val <- reactiveValues(album_img = "", album_info ="",
                                location =NULL,
                                df= NULL,
                                track_anlys=NULL,
                                pca = NULL)
    ranges <- reactiveValues(x = NULL, y = NULL)
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    #____ function______#
    get_small_img <- function(img_df)
    {
        return(img_df[3,]$url)
    }
    
    
    get_medium_img <- function(img_df)
    {
        return(img_df[2,]$url)
    }
    
    data_processing <-function(df)
    {
        # browser()
        ids <- df$track.id
        anlys_df <- data.frame()
        i <- min(50, length(ids))
        p <-0
        while (dim(anlys_df)[1] != length(ids)) {
            cur_ids <- ids[p:i]
            anlys_df <- rbind(anlys_df, get_data_df(cur_ids))
            p <- i + 1
            i <- min(i + 50, length(ids))
            output$print <- renderText({ paste0(round( i / length(ids),digits = 2) *
                                                    100,"% ","complete")})
            print(paste0(round( i / length(ids),digits = 2) *
                             100,"% ","complete"))
        }
        dummies <- mclapply(df$artists, stri_enc_toutf8)  %>%
            I %>% data.frame() %>%
            dummy_cols(split=',', remove_selected_columns= TRUE)
        anlys_df <- cbind(anlys_df,release_year = as.numeric(df$release_year), dummies)
        output$print <- renderText({ "done processing!"})
        return(anlys_df)
    }
    
    get_plotdf<- function(){
        
        # meta_d <- get_saved_tracks #             uncheck for downloading 
        # track_anlys <- data_processing(meta_d)
        
        
        load("ido_data.rda")
        #remove duplicated song
        output$print <- renderText({ "Remove duplicates"})
        print("Remove duplicates")
        duplicates <- find_duplicates(track_anlys)
        
        if(!is.null(duplicates)){
            track_anlys <- track_anlys[-duplicates,]
            meta_d <- meta_d[-duplicates,]
        }
        output$print <- renderText({ "doing T-sne "})
        drop_cols <- c("sections.loudness"  ,    "sections.tempo"      ,   "sections.key"   ,        "sections.loudness.std", 
                       "sections.tempo.std"  ,   "sections.key.std"    ,   "sections.num"    ,       "average.beats.duration",
                       "beats.amount"       ,    "average.bars.duration"  ,"bars.amount"      ,     "mode" )
        no_year_df <- subset(track_anlys,select=-which(colnames(track_anlys) %in% drop_cols))
        res <- get_tsne_df(no_year_df)
        meta_d$T_sne.X <- res[,1]
        meta_d$T_sne.Y <- res[,2]
        pca_res <- prcomp(no_year_df, scale. = TRUE, center=TRUE, pca=TRUE)
        meta_d$PCA.X <- data.frame(pca_res$x)$PC1
        meta_d$PCA.Y <- data.frame(pca_res$x)$PC2
        meta_d$small.img <- unlist(mclapply(meta_d$track.album.image,get_small_img))
        meta_d$medium.img <- unlist(mclapply(meta_d$track.album.image, get_medium_img))
        hover_val$pca <- pca_res
        hover_val$df <- meta_d
        hover_val$track_anlys <- track_anlys
        output$print <- renderText({ "All done :)"})
    }
    
    
    
    
    
    #____getting the data ________________#
    output$print <- renderText({ "start to process the data (it takes some time)"})
    print( "start to process the data (it takes some time)")
    get_plotdf()
    
    
    
    #___________ elbow plot & PCA plot_________
    output$elbowplot <- renderPlot({
        
        fviz_nbclust(hover_val$track_anlys, kmeans, method = "wss") +
            labs(subtitle = "Elbow method") # add subtitle
    })
    
    output$screenPlot <- renderPlot({
        fviz_eig(hover_val$pca)
    })
    # ______________ main plot________________________#
    # ______________zoomable main plot___________
    
    
    
    observeEvent(c(input$plot_hover,input$mini_plot_hover),{
        near_points <- nearPoints(hover_val$df, input$plot_hover,threshold=5,
                                  xvar=paste0(input$method,".X"),
                                  yvar=paste0(input$method,".Y"))
        if(dim(near_points)[[1]] != 0)
        {
            y <- near_points[,c("track.name","medium.img","artists","release_year")]
            location <- input$plot_hover
            
        }else{
            y <- nearPoints(hover_val$df, input$mini_plot_hover,threshold=5)[,c("track.name",
                                                                                "medium.img", 
                                                                                "artists",
                                                                                "release_year")]
            location <- input$mini_plot_hover
        }
        hover_val$location <- location
        hover_val$album_info <- y
        req(nrow(y) != 0)
        hover_val$album_img <- c('<img src="',y$medium.img[[1]],'">')
        
    })
    
    output$my_tooltip <- renderUI({
        hover <- hover_val$location 
        y <- hover_val$album_info
        req(nrow(y) != 0)
        verbatimTextOutput("track_text")
    })
    
    
    
    output$track_text <- renderPrint({
        hover <- hover_val$location 
        y <- hover_val$album_info
        req(nrow(y) != 0)
        cat(paste0("title: ",y$track.name, "\nartist: ", y$artists, "\nrelease year: ", y$release_year))
    })
    
    observe({
        # browser()
        hover_val$df$cluster <- kmeans(hover_val$track_anlys, input$n_cluster)$cluster %>% 
            as.character()
        
        if(input$color == "cluster"){
            cur_theme <- theme( legend.position = "none",
                                axis.title = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks = element_blank()) 
        }else{
            cur_theme <- theme(legend.position = "right",
                               axis.title = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank()) 
        }
        plot_var$mainPlot <- ggplot(hover_val$df,
                                    aes_string(x=paste0(input$method, ".X"),
                                               y=paste0(input$method,".Y"),
                                               color=input$color)
        ) +geom_point(size=5)+
            cur_theme
    })
    
    output$plot1 <- renderPlot({
        
        plot_var$mainPlot +
            coord_cartesian(xlim = ranges$x,
                            ylim = ranges$y,
                            expand = TRUE) 
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    #__________end main plot code________________________#
    
    #______________ right panel side ___________________#
    #_______text __________#
    
    # 
    output$album_img <- renderText({
        if(is.null(hover_val$album_img)) return("")
        hover_val$album_img
    })
    
    
    
    
    
    
    #______mini Main plot _______#
    
    
    output$miniMain <- renderPlot({
        plot_var$mainPlot + 
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
            theme(legend.position = "none")
        
    })
    
    observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    
    #_______playlist button _____#
    get_tsp_result <- function(playlist_idx){
        # browser()
        filtered <- hover_val$df[playlist_idx,c(paste0(input$method,".X"),paste0(input$method,".Y"))]
        tsp <- dist(filtered) %>% TSP( labels = hover_val$df[playlist_idx, "track.name"]) 
        return(solve_TSP(tsp, as_TSP=TRUE)) 
    }
    
    
    
    
    create_spotify_playlist<- function(playlist_df){
        usr <- get_my_profile()
        playlist <- create_playlist(user_id =usr$id,
                                    name = "plot_playlist",
                                    description = "A playlist generated by Ido's
                                  Data visualization project :)")
        
        add_tracks_to_playlist(playlist_id = playlist$id,
                               uris= playlist_df$track.id)
        output$print <- renderText({paste0("listen to the playlist at: https://open.spotify.com/playlist/", playlist$id)})
        unfollow_playlist( playlist_id=playlist$id)
    }
    
    create_playlis_plot <- function(playlist_df,distance)
    {
        
        #____ create the plot of the playlist on the main screen ___#
        
        output$plot1 <- renderPlot({
            
            arrow_list <- playlist_df[,c(paste0(input$method,".X"),
                                         paste0(input$method,".Y"))]
            
            arrow_list <- cbind(arrow_list, data.frame(shift(arrow_list, fill = 0,
                                                             type="lead"))) 
            colnames(arrow_list) <- c("x.start", "y.start", "x.end", "y.end")
            
            arrow_list[nrow(arrow_list), c("x.end", "y.end")] <- c(NA,NA)
            playlist_df <- cbind(hover_val$df[rownames(playlist_df),], arrow_list)
            # hover_val$df <- playlist_df
            playlist_df$cluster <-  kmeans(hover_val$track_anlys, input$n_cluster)$cluster[
                as.integer(rownames(playlist_df))] %>% 
                as.character()
            
            arrows <- list()
            for (i in 1:nrow(arrow_list) -1) {
                if (i %% 2 == 0) {
                    colour <- "#fc7edc"
                    
                }else{
                    colour <- "#707acc"
                }
                cur_arrow <- geom_segment(data = arrow_list[i,],
                                          aes(x = x.start,
                                              y = y.start,
                                              xend = x.end,
                                              yend = y.end),
                                          arrow = arrow(length = unit(0.03, "npc"), 
                                                        type="closed"),
                                          colour = colour,
                                          size = 1.2,)
                arrows <-append(arrows,cur_arrow)
                
            }
            
            playlist_plot <-ggplot(playlist_df,
                                   aes_string(x=paste0(input$method, ".X"),
                                              y=paste0(input$method,".Y"),
                                              color=input$cluster))+
                arrows +
                geom_point(size=5) + 
                theme(axis.title = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank()) +
                coord_cartesian(xlim = ranges$x,
                                ylim = ranges$y,
                                expand = TRUE)
            
            
            playlist_plot
        })
        
    }
    
    
    
    
    
    observeEvent(input$playlist_button, {
        if (is.null(input$plot_brush)) {return(NULL)}
        e<- input$plot_brush
        playlist_idx <- which(hover_val$df[paste0(input$method,".X")] < e$xmax & 
                                  hover_val$df[paste0(input$method,".X")] > e$xmin & 
                                  hover_val$df[paste0(input$method,".Y")] < e$ymax &
                                  hover_val$df[paste0(input$method,".Y")] > e$ymin )
        tsp_res <- get_tsp_result(playlist_idx)
        playlist_df <- hover_val$df[playlist_idx,][order(tsp_res), c("track.name",
                                                                     "track.id",
                                                                     paste0(input$method,".X"),
                                                                     paste0(input$method,".Y"))]
        create_playlis_plot(playlist_df,tour_length(tsp_res))
        create_spotify_playlist(playlist_df)
        
    })
    
    observeEvent(input$reset,{
        hover_val$df$cluster <- kmeans(hover_val$track_anlys, input$n_cluster)$cluster %>% 
            as.character()
        # browser()
        
        if(input$color == "cluster"){
            cur_theme <- theme( legend.position = "none",
                                axis.title = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks = element_blank()) 
        }else{
            cur_theme <- theme(legend.position = "right",
                               axis.title = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank()) 
        }
        plot_var$mainPlot <- ggplot(hover_val$df,
                                    aes_string(x=paste0(input$method, ".X"),
                                               y=paste0(input$method,".Y"),
                                               color=input$color)
        ) +geom_point(size=5)+
            cur_theme
        
        output$plot1 <- renderPlot({ plot_var$mainPlot + coord_cartesian(xlim = NULL,ylim = NULL,expand = TRUE)})
        
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
