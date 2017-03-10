library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(toOrdinal)
library(Hmisc)
library(shinyBS)
library(ggiraph)

potter <- read.csv('final_potter021817.csv')

# allow for styling of action buttons
actionButton <- function(inputId, label, style = "" , additionalClass = "") {
  if (style %in% c("primary","info","success","warning","danger","inverse","link")) {
    class.style <- paste("btn",style,sep="-")
  } else class.style = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",class.style,additionalClass), label)
}

# create ui
ui <- navbarPage(
  theme = shinytheme('flatly'),
  "Harry Potter Data",
            tabPanel("Aggression Data",
                      tabsetPanel(
             tabPanel("By Book",
                      br(),
                      sidebarPanel(
                        # input for books
                        checkboxGroupInput(inputId = "inputbook", 
                                           label = "Filter by Book",
                                           choices = unique(potter$Book),
                                           selected = unique(potter$Book)[1]),
                        # select all books
                        actionButton("selectall", 
                                     label = "Select all books", 
                                     style = "inverse"),
                        p(),
                        p(),
                        #input for houses
                        checkboxGroupInput(inputId = "inputhouse",
                                           label = "Filter by House",
                                           choices = unique(potter$House),
                                           selected = unique(potter$House)),
                        # number of characters
                        sliderInput(inputId = "charnum",
                                    label = "Number of Characters to Display",
                                    min = 1, max = 8, value = 5),
                        # character tooltip
                        bsTooltip(id = "charnum",
                                  title = "Sorted by most aggressive characters in each book.",
                                  placement = "right", 
                                  trigger = "hover"),
                        width = 2),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("By Count",
                                     div(br(),
                                         br(),
                                       style = "position:relative",
                                       plotOutput("plot1a", 
                                                  width = 1250)
                                       )),
                            tabPanel("By Frequency",
                                    div(br(),
                                        br(),
                                    style = "position:relative",
                                    plotOutput("plot1", 
                                       width = 1280)))
                          ))
                      ),
             tabPanel("By Character",
                      br(),
                      sidebarPanel(
                    selectInput(inputId = "inputname",
                                  label = "Track a Character Through the Books",
                                  choice = unique(potter$Name), width = 300),
                    htmlOutput("character_text"),
                      width = 3),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("By Count",
                                   br(),
                                   br(),
                                   ggiraphOutput("plot2a"
                                                 #,
                                   #hover = hoverOpts("plot_hover")
                                   )
                                   #uiOutput("hover_plot2")
                                   ),
                          tabPanel("By Frequency",
                                   br(),
                                   br(),
                                   ggiraphOutput("plot2"))
                          

                               ))
                      )
             )),
              tabPanel("Mention Data",
                       tabsetPanel(
                         tabPanel("By Book",
                                  br(),
                                  sidebarPanel(
                                    # input for books
                                    checkboxGroupInput(inputId = "bookmentions", 
                                                       label = "Filter by Book",
                                                       choices = unique(potter$Book),
                                                       selected = unique(potter$Book)[1]),
                                    # select all for books
                                    actionButton("selectall1", 
                                                 label = "Select all books", 
                                                 style = "inverse"),
                                    p(),
                                    p(),
                                    #input for houses
                                    checkboxGroupInput(inputId = "housementions",
                                                       label = "Filter by House",
                                                       choices = unique(potter$House),
                                                       selected = unique(potter$House)),
                                    # number of characters
                                    sliderInput(inputId = "charmentions",
                                                label = "Number of Characters to Display",
                                                min = 1, max = 8, value = 5),
                                    # character tooltip
                                    bsTooltip(id = "charmentions",
                                              title = "Sorted by most popular characters in each book.",
                                              placement = "right", 
                                              trigger = "hover"),
                                    width = 2),
                                  mainPanel(
                                    div(br(),
                                        br(),
                                        style = "position:relative",
                                        plotOutput("bar_mentions", 
                                                   width = 1280
                                    )))),
                                  tabPanel("By Character",
                                           br(),
                                           sidebarPanel(
                                             selectInput(inputId = "namementions",
                                                         label = "Track a Character Through the Books",
                                                         choice = unique(potter$Name), width = 300),
                                             htmlOutput("character_text2"),
                                             br(),
                                             plotOutput("booklengths"),
                                             width = 3,
                                             bsTooltip("booklengths", "Quick reference, since mention count can be misleading in longer books", placement = "right")),
                                           mainPanel(
                                             tabsetPanel(
                                             tabPanel("By Count",
                                                      br(),
                                              ggiraphOutput("point_mentions")
                                              )
                                              ,
                                             tabPanel("By Frequency",
                                                      br(),
                                                      ggiraphOutput("point_freq")))
                                               
                                             )))
                                           ),
                      tabPanel("Summary by Book",
                               wellPanel(
                                 radioButtons(inputId = "radiobook",
                                              label = "Select a book to see its data summary",
                                              choice = unique(potter$BookNN),
                                              inline = TRUE
                                              
                                 )
                               ),
                               sidebarPanel(
                               wellPanel(
                                 
                                 htmlOutput("booksummary")), width = 2),
                               # so the graphs share space in the layout
                               splitLayout(cellWidths = c("20%", "22%", "19%", "22%"), 
                                           tableOutput("table1"), 
                                           tableOutput("table2"),
                                           tableOutput("table3"),
                                           tableOutput("table4"),
                                           width = 8),
                               # add some space between the divs
                               wellPanel(htmlOutput("wellpaneltext"),
                                         align = "center"),
                               wellPanel(
                                 fluidRow(
                                   column(2,
                                 plotOutput("salesplot")),
                                 column(2,
                                 plotOutput("lengthplot")),
                                 column(2,
                                        plotOutput("deathplot")),
                                 column(2,
                                        plotOutput("aggplot")),
                                 column(2,
                                        plotOutput("aggfplot"))
                               )
                               )
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(1),
                                 column(2,
                                        wellPanel(
                                        h4(strong("About the App"), 
                                           align = "center"),
                                        htmlOutput("appinfo",
                                                   align = "left")
                                        )),
                                 
                                 column(6,
                                        wellPanel(
                                        h4(strong("About the Data"),
                                           align = "center"),
                                        htmlOutput("datainfo",
                                                   align = "left")
                                        )),
                                 
                                 column(2,
                                        wellPanel(
                                          h4(strong("Data Sources"),
                                             align = "center"),
                                          htmlOutput("datasources",
                                                     align = "left")
                                        ))
                                 , align = "center"
                               )
                                           
                       ))
            

                 

# define server
server <- function(input, output, session) {
  # import csv
  potter <- read.csv('final_potter021817.csv')
  
  # define book factors
  potter$Book <- factor(potter$Book, levels = c("The Sorcerer's Stone (1997)", "The Chamber of Secrets (1998)", "The Prisoner of Azkaban (1999)", "The Goblet of Fire (2000)", "The Order of the Phoenix (2003)", "The Half-Blood Prince (2005)", "The Deathly Hallows (2007)"))
  # define house factors
  potter$House <- factor(potter$House, levels = c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin", "Unknown", "None (Non-Human)"))
  # define a colorway and assign them to the houses
  myColors <- c('#941B08', '#DAA520', '#071A80', '#154C07', '#383838', '#7F7F7F')
  names(myColors) <- levels(potter$House)
 

  # order aggression by book for easy selection
  potter_by_book <- potter[order(potter$Book, potter$Agg_freq, decreasing = TRUE),]
  
  # add whitespace to the names for better display in the plot
  #levels(potter_by_book$Name) <- sub(" ", "\n", levels(potter_by_book$Name))
  
  # copy original df for better manipulation
  names_potter <- potter

  # add whitespace to the book titles
  levels(names_potter$Book) <- gsub(" ", "\n", levels(names_potter$Book))
  
  
  # subset names based on the name selected
  names_sub <- reactive({subset(names_potter, Name %in% input$inputname)})
  # subset books based on the books selected
  book_sub <- reactive({subset(potter_by_book, Book %in% input$inputbook)})
  # subset house based on the houses selected
  book_house_sub <- reactive({subset(book_sub(), House %in% input$inputhouse)})
  # pull the top n Agg_freq values from the df
  book_plot <- reactive({
    book_house_sub()[ave(book_house_sub()$Agg_freq, book_house_sub()$Book, FUN = seq_along) <= input$charnum,]
  })


  
  
  # plot the first (set of) graph(s)

  output$plot1 <- renderPlot({
      ggplot(book_plot(), aes(x = Name, y = Agg_freq, fill = House, tooltip = Name, data_id = Name)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Book, scales = "free_x", shrink = FALSE, ncol = 4, nrow = 2) + 
      scale_fill_manual(values = myColors) +
      labs(title = paste(input$charnum, "Most Aggressive Characters by Book"), 
           x = "Character Name", 
           y = "Aggression Frequency") +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 14)) +
      theme(axis.text.y = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = .5, size = 18)) +
      theme(axis.title.x = element_text(size = 16)) +
      theme(axis.title.y = element_text(size = 16)) +
      theme(legend.title = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      theme(strip.text.x = element_text(size = 16)) + 
      scale_y_continuous(labels = percent)
    
  }, 
  # make height and width conditional on number of graphs
  height = function() {
    if (length(unique(book_plot()$Book)) > 4) 
    { 750
    } else {
        375
      }
  }
  , width = function() {
    if (length(unique(book_plot()$Book)) == 1)
    { 600
    } else if (length(unique(book_plot()$Book)) == 2)
      { 1000
    } else {
        "auto"
    }
  }
  )

output$plot2 <- renderggiraph({
  gg <- ggplot(names_sub(),
               aes(x = Book, y = Agg_freq,
                   group = 1, color = House))
  ggg <- gg +
    labs(title = paste(input$inputname, sep = "","'s Aggression Frequency by Book"),
         x = "Book Title",
         y = "Aggression Frequency") +
    scale_color_manual(values = myColors) +
    geom_line() +
    theme(axis.text.x = element_text(size = 8)) +
    theme(axis.text.y = element_text(size = 7)) +
    theme(plot.title = element_text(hjust = .5, size = 12)) +
    theme(axis.title.x = element_text(size = 10)) +
    theme(axis.title.y = element_text(size = 10)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 6))
  
  g <- ggg + geom_point_interactive(aes(
    tooltip = paste("Aggression Count: ", Aggressions,
                    br(),
                    "Mentions: ", Mentions,
                    br(),
                    "Aggression Frequency: ", percent(Agg_freq)
                    )
  ), size = 2.5, data_id = "Agg_freq") 
  
  ggiraph(code = {print(g)}, hover_css = "fill:yellow;", height = 3, width = .9)
})

  
  # total Mentions sorted by name
  mentions <- data.frame(aggregate(Mentions ~ Name, data = potter, FUN = "sum"))
  # total aggressions sorted by name
  aggs <- data.frame(aggregate(Aggressions ~ Name, data = potter, FUN = "sum"))
  # average aggression freq sorted by name
  agg_freqs <- data.frame(aggregate(Agg_freq ~ Name, data = potter, FUN = "mean"))
  # number of books character appears in
  # ended up not using this because of incomplete data gathering methods
  book_count <- data.frame(aggregate(Book ~ Name, data = potter, FUN = function(x){NROW(x)}))
  # merge the dfs
  totals <- merge(mentions, aggs, by= "Name")
  totals <- merge(totals, agg_freqs, by = "Name")
  totals <- merge(totals, book_count, by ="Name")
  # create empty rank columns
  totals$mentionrank <- NA
  totals$aggrank <- NA
  totals$agg_freqrank <- NA
  # rank the characters for the three variables of interest 
  totals$mentionrank[order(-totals$Mentions)] <- 1:nrow(totals)
  totals$aggrank[order(-totals$Aggressions)] <- 1:nrow(totals)
  totals$agg_freqrank[order(-totals$Agg_freq)] <- 1:nrow(totals)
  
  # reactive df of only the selected character
  total_name <- reactive({subset(totals, Name %in% input$inputname)})
  
  # dynamic text output for aggression
  output$character_text <- renderUI({ str1 <- paste(input$inputname, 
                                                    names_sub()$past,
                                             "described as exhibiting aggressive behavior",
                                              strong(total_name()$Aggressions),
                                              "times, making", 
                                              names_sub()$objective[1],
                                              "the",
                                              strong(toOrdinal(total_name()$aggrank)),
                                              "most aggressive character in the series by count.")
                                            
                                      str3 <- paste("However, this doesn't take into account how often", 
                                              names_sub()$subjective[1],
                                              names_sub()$present,
                                              "mentioned.", 
                                              capitalize(as.character(names_sub()$possessive[1])),
                                              "adjusted aggression frequency is",
                                              strong(percent(mean(total_name()$Agg_freq))),
                                              ", meaning", 
                                              names_sub()$subjective[1],
                                              names_sub()$present,
                                              "actually the",
                                              strong(toOrdinal(total_name()$agg_freqrank)),
                                              "most aggressive character.")
                                      
                                      str4 <- paste(capitalize(as.character(names_sub()$subjective[1])),
                                                    names_sub()$past,
                                              "most aggressive in",
                                              strong(names_sub()$Book[names_sub()$Agg_freq == max(names_sub()$Agg_freq)]),
                                              "with an aggression frequency of",
                                              strong(percent(names_sub()$Agg_freq[names_sub()$Agg_freq == max(names_sub()$Agg_freq)])),
                                              ".",
                                              capitalize(as.character(names_sub()$subjective[1])),
                                              names_sub()$past,
                                              "least aggressive in",
                                              strong(names_sub()$Book[names_sub()$Agg_freq == min(names_sub()$Agg_freq)]),
                                              ", with an aggression frequency of",
                                              strong(percent(names_sub()$Agg_freq[names_sub()$Agg_freq == min(names_sub()$Agg_freq)])),
                                              p(),
                                              em("Hover over points on the graph to view exact values.")
                                              
                                      )
                                      
                                      HTML(paste(str1[1], str3[1], str4[1], sep = '<p/>'))
  })
  
  
  # order aggression by book for easy selection
  potter_by_book1 <- potter[order(potter$Book, potter$Aggressions, decreasing = TRUE),]
  
  # subset books based on the books selected
  book_sub1 <- reactive({subset(potter_by_book1, Book %in% input$inputbook)})
  # subset house based on the houses selected
  book_house_sub1 <- reactive({subset(book_sub1(), House %in% input$inputhouse)})
  # pull the top n Agg_freq values from the df
  book_plot1 <- reactive({
    book_house_sub1()[ave(book_house_sub1()$Aggressions, book_house_sub1()$Book, FUN = seq_along) <= input$charnum,]
  })
  
  
  
  
  # plot aggression count data
  output$plot1a <- renderPlot({
    ggplot(book_plot1()) +
      geom_bar(aes(x = Name, y = Aggressions, fill = House), stat = "identity")+
      facet_wrap(~ Book, scales = "free_x", shrink = FALSE, ncol = 4, nrow = 2) + 
      scale_fill_manual(values = myColors) +
      labs(title = paste(input$charnum, "Most Aggressive Characters by Book"), 
           x = "Character Name", 
           y = "Aggressions (Count)") +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 14)) +
      theme(axis.text.y = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = .5, size = 18)) +
      theme(axis.title.x = element_text(size = 16)) +
      theme(axis.title.y = element_text(size = 16, margin = margin(0,10,0,0))) +
      theme(legend.title = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      theme(strip.text.x = element_text(size = 16))
  }, height = function() {
    if (length(unique(book_plot1()$Book)) > 4) 
    { 750
    } else {
      375
    }
  }
  , width = function() {
    if (length(unique(book_plot1()$Book)) == 1)
    { 550
    } else if (length(unique(book_plot1()$Book)) == 2)
    { 950
    } else {
      "auto"
    }
  }
  )
  
  # plot aggression count by character
  output$plot2a <- renderggiraph({
    gg <- ggplot(names_sub(), 
                 aes(x = Book, y = Aggressions, 
                     group = 1, color = House))
    ggg <- gg +
      labs(title = paste(input$inputname, sep = "","'s Aggression Count by Book"),
           x = "Book Title",
           y = "Aggression Count") +
      geom_line() +
      scale_color_manual(values = myColors) +
      theme(axis.text.x = element_text(size = 8)) +
      theme(axis.text.y = element_text(size = 7)) +
      theme(plot.title = element_text(hjust = .5, size = 12)) +
      theme(axis.title.x = element_text(size = 10)) +
      theme(axis.title.y = element_text(size = 10)) +
      theme(legend.title = element_text(size = 8)) +
      theme(legend.text = element_text(size = 6))
    
    g <- ggg + geom_point_interactive(aes(
      tooltip = paste("Aggression Count: ", Aggressions,
                      br(),
                      "Mentions: ", Mentions,
                      br(),
                      "Aggression Frequency: ", percent(Agg_freq)
      )), 
      size = 2.5, data_id = "Aggressions")
    
    ggiraph(code = {print(g)}, hover_css = "fill:yellow;", height = 3, width = .9)
  })
  

  # reactive df for selected character names
  names_sub_mentions <- reactive({subset(names_potter, Name %in% input$namementions)})
  
  # order mentions by book for easy selection
  potter_by_book2 <- potter[order(potter$Book, potter$Mentions, decreasing = TRUE),]
  
  # subset books based on the books selected
  book_sub2 <- reactive({subset(potter_by_book2, Book %in% input$bookmentions)})
  # subset house based on the houses selected
  book_house_sub2 <- reactive({subset(book_sub2(), House %in% input$housementions)})
  # pull the top n Agg_freq values from the df
  book_plot2 <- reactive({
    book_house_sub2()[ave(book_house_sub2()$Mentions, book_house_sub2()$Book, FUN = seq_along) <= input$charmentions,]
  })  
  
  # mention count by book
  output$bar_mentions <- renderPlot({
    ggplot(book_plot2()) +
      geom_bar(aes(x = Name, y = Mentions, fill = House), stat = "identity")+
      facet_wrap(~ Book, scales = "free_x", shrink = FALSE, ncol = 4, nrow = 2) + 
      scale_fill_manual(values = myColors) +
      labs(title = paste(input$charmentions, "Most Popular Characters by Book"), 
           x = "Character Name", 
           y = "Mentions (Count)") +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 14)) +
      theme(axis.text.y = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = .5, size = 18)) +
      theme(axis.title.x = element_text(size = 16)) +
      theme(axis.title.y = element_text(size = 16)) +
      theme(legend.title = element_text(size = 14)) +
      theme(legend.text = element_text(size = 12)) +
      theme(strip.text.x = element_text(size = 16))
  }, height = function() {
    if (length(unique(book_plot2()$Book)) > 4) 
    { 750
    } else {
      375
    }
  }
  , width = function() {
    if (length(unique(book_plot2()$Book)) == 1)
    { 600
    } else if (length(unique(book_plot2()$Book)) == 2)
    { 1000
    } else {
      "auto"
    }
  }
  )  
  
  # mention freq by character
  output$point_freq <- renderggiraph({
    gg <- ggplot(names_sub_mentions(), 
                 aes(x = Book, y = Mention_freq, 
                     group = 1, color = House))
    ggg <- gg +
      labs(title = paste(input$namementions, sep = "","'s Mention Frequency by Book"),
           x = "Book Title",
           y = "Mention Frequency") +
      geom_line() +
      scale_color_manual(values = myColors) +
      theme(axis.text.x = element_text(size = 8)) +
      theme(axis.text.y = element_text(size = 7)) +
      theme(plot.title = element_text(hjust = .5, size = 12)) +
      theme(axis.title.x = element_text(size = 10)) +
      theme(axis.title.y = element_text(size = 10)) +
      theme(legend.title = element_text(size = 8)) +
      theme(legend.text = element_text(size = 6))
    
    g <- ggg + geom_point_interactive(aes(
      tooltip = paste("Mentions: ", Mentions,
                      br(),
                      "Paragraphs: ", Paragraphs,
                      br(),
                      "Mention Frequency: ", percent(Mention_freq)
      )), 
      size = 2.5, data_id = "Mention_freq")
    
    ggiraph(code = {print(g)}, hover_css = "fill:yellow;", height = 3, width = .9)
  })
  
  # mention count by character
  output$point_mentions <- renderggiraph({
    gg <- ggplot(names_sub_mentions(), 
                 aes(x = Book, y = Mentions, 
                     group = 1, color = House))
    ggg <- gg +
      scale_color_manual(values = myColors) +
      labs(title = paste(input$namementions, sep = "","'s Mentions by Book"),
           x = "Book Title",
           y = "Mentions (Count)") +
      geom_line() +
      theme(axis.text.x = element_text(size = 8)) +
      theme(axis.text.y = element_text(size = 7)) +
      theme(plot.title = element_text(hjust = .5, size = 12)) +
      theme(axis.title.x = element_text(size = 10)) +
      theme(axis.title.y = element_text(size = 10)) +
      theme(legend.title = element_text(size = 8)) +
      theme(legend.text = element_text(size = 6))
    
    g <- ggg + geom_point_interactive(aes(
      tooltip = paste("Mentions: ", Mentions,
                      br(),
                      "Paragraphs: ", Paragraphs,
                      br(),
                      "Mention Frequency: ", percent(Mention_freq)
      )), 
      size = 2.5, data_id = "Mentions")
    
    ggiraph(code = {print(g)}, hover_css = "fill:yellow;", height = 3, width = .9)
  })
  
  # greyed out book length plot for sidebar
  output$booklengths <- renderPlot({
    ggplot(names_sub(), aes(x = Book, y = Paragraphs)) +
      geom_bar(stat = "identity") + 
      labs(title = "Paragraph Count per Book") +
      theme(plot.title = element_text(hjust = .5)) +
      theme(plot.background = element_rect(fill = alpha("grey", .2))) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_blank()) +
      #scale_x_continuous(breaks = pretty_breaks(n = 6)) +
      theme(legend.position = "none") 
      #scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C"))
  }, width = 325)
  
  # dupe of totals so the original data isn't edited
  totals2 <- totals
  #totals2 <- subset(totals2, Book == 7)
  total_name2 <- reactive({subset(totals2, Name %in% input$namementions)})
  
  # mentions dynamic text
  output$character_text2 <- renderUI({ str1 <- paste(input$namementions, 
                                                     names_sub_mentions()$past,
                                                     "mentioned a total of ", 
                                                    strong(sum(names_sub_mentions()$Mentions)), "times, or in ", 
                                                    strong(percent(mean(names_sub_mentions()$Mention_freq))),
                                                    "of the paragraphs in the series. This makes", 
                                                    names_sub_mentions()$objective[1],
                                                    "the",
                                                    strong(toOrdinal(total_name2()$mentionrank)),
                                                    "most mentioned character in the series.")
  
  str2 <- paste(capitalize(as.character(names_sub_mentions()$subjective[1])),
                names_sub_mentions()$past,
                "mentioned the most in",
                strong(names_sub_mentions()$Book[names_sub_mentions()$Mentions == max(names_sub_mentions()$Mentions)]),
                "- ",
                strong(names_sub_mentions()$Mentions[names_sub_mentions()$Mentions == max(names_sub_mentions()$Mentions)]),
                "times in ",
                strong(names_sub_mentions()$Paragraphs[names_sub_mentions()$Mentions == max(names_sub_mentions()$Mentions)]),
                "paragraphs, meaning",
                as.character(names_sub_mentions()$subjective[1]),
                names_sub_mentions()$past,
                "mentioned in",
                strong(percent(names_sub_mentions()$Mention_freq[names_sub_mentions()$Mentions == max(names_sub_mentions()$Mentions)])),
                "of all paragraphs in the book. ",
                capitalize(as.character(names_sub_mentions()$subjective[1])),
                names_sub_mentions()$past,
                "mentioned least in",
                strong(names_sub_mentions()$Book[names_sub_mentions()$Mentions == min(names_sub_mentions()$Mentions)]),
                "- ",
                strong(names_sub_mentions()$Mentions[names_sub_mentions()$Mentions == min(names_sub_mentions()$Mentions)]),
                "times in ",
                strong(names_sub_mentions()$Paragraphs[names_sub_mentions()$Mentions == min(names_sub_mentions()$Mentions)]),
                "paragraphs, meaning",
                as.character(names_sub_mentions()$subjective[1]),
                names_sub_mentions()$past,
                "mentioned in",
                strong(percent(names_sub_mentions()$Mention_freq[names_sub_mentions()$Mentions == min(names_sub_mentions()$Mentions)])),
                "of all paragraphs in the book. ",
                p(),
                em("Hover over points on the graph to view exact values.")
                
  )
  
  HTML(paste(str1[1], str2[1], sep = '<p/>'))
  })
  
  # reactive df for radio book input
book_df <- reactive({subset(potter, BookNN %in% input$radiobook)})

# find the totals as above but for name
book_aggression <- data.frame(aggregate(Aggressions ~ Book, data = potter, FUN=sum))
book_agg_freq <- data.frame(aggregate(Agg_freq ~ Book, data = potter, FUN=mean))
book_aggression <- merge(book_aggression, book_agg_freq, by = "Book")

# pull only the first mention of each book from the main df
book_info <- potter[!duplicated(potter$Book),]
# merge the dfs
book_info <- merge(book_info, book_aggression, by = "Book")
# create blank rank columns
book_info$salesrank <- NA
book_info$deathrank <- NA
book_info$ratingrank <- NA
book_info$barank <- NA
book_info$bafrank <- NA
book_info$lengthrank <- NA
# rank everything
book_info$salesrank[order(-book_info$CopiesSold)] <- 1:nrow(book_info)
book_info$deathrank[order(-book_info$Deaths)] <- 1:nrow(book_info)
book_info$ratingrank[order(-book_info$AvgRating)] <- 1:nrow(book_info)
book_info$barank[order(-book_info$Aggressions.y)] <- 1:nrow(book_info)
book_info$bafrank[order(-book_info$Agg_freq.y)] <- 1:nrow(book_info)
book_info$lengthrank[order(-book_info$Paragraphs)] <- 1:nrow(book_info)


# only book that was selected, plus the rank info
ind_book <- reactive({subset(book_info, BookNN %in% input$radiobook)})


  
# dynamic text for book/publisher data
output$booksummary <- renderUI({ str1 <- paste(strong("Title: "),
                                               book_df()$BookNoYear,
                                               p(),
                                               strong("Series Order: "),
                                               toOrdinal(ind_book()$Series))
                                               

str2 <- 
  if (as.character(ind_book()$RDBritish) == as.character(ind_book()$RDAmerican)){
  paste(strong("Publication Date: "),
              ind_book()$RDBritish)}
  else {
    paste(strong("Publication Date (UK): "),
          ind_book()$RDBritish,
          "(UK)",
          p(),
          strong("Publication Date (US): "),
          ind_book()$RDAmerican,
          "(US)")
  }
  

str3 <- paste(strong("Total Sales: "),
              book_df()$CopiesSold,
              "million",
              br(),
              em("(",
                 toOrdinal(ind_book()$salesrank),
                 "in the series )"),
              p(),
              strong("Length: "),
              book_df()$Paragraphs,
              "paragraphs",
              br(),
              em("(",
                 toOrdinal(ind_book()$lengthrank),
                 "in the series )"))




str4 <- paste(strong("Average Goodreads Rating: "),
              book_df()$AvgRating,
              br(),
              em("(",
              toOrdinal(ind_book()$ratingrank),
              "in the series )"))

str5 <- paste(strong("Total Deaths: "),
              book_df()$Deaths,
              br(),
              em("(",
              toOrdinal(ind_book()$deathrank),
              "in the series )"))

str6 <- paste(strong("Total Aggressions: "),
              ind_book()$Aggressions.y,
              br(),
              em("(",
              toOrdinal(ind_book()$barank),
              "in the series )"),
              p(),
              strong("Average Aggression Frequency: "),
              percent(ind_book()$Agg_freq.y),
              br(),
              em("(",
              toOrdinal(ind_book()$bafrank),
              "in the series )"))

HTML(paste(str1[1], str2[1], str3[1], str4[1], str5[1], str6[1], sep = '<p/>'))
})
  
# sort columns by book
agg_by_book <- potter[order(potter$Book, potter$Aggressions, decreasing = TRUE),]
names(agg_by_book)[5] <- "Aggression Count"
agg_freq_by_book <- potter[order(potter$Book, potter$Agg_freq, decreasing = TRUE),]
names(agg_freq_by_book)[6] <- "Aggression Frequency"
mentions_by_book <- potter[order(potter$Book, potter$Mentions, decreasing = TRUE),]
names(mentions_by_book)[10] <- "Mentions"
mention_freq_by_book <- potter[order(potter$Book, potter$Mention_freq, decreasing = TRUE),]
names(mention_freq_by_book)[8] <- "Mention Frequency"

# subset df based on the radiobook input
agg_reactive <- reactive({subset(agg_by_book, BookNN %in% input$radiobook)})
agg_freq_reactive <- reactive({subset(agg_freq_by_book, BookNN %in% input$radiobook)})
mentions_reactive <- reactive({subset(mentions_by_book, BookNN %in% input$radiobook)})
mention_freq_reactive <- reactive({subset(mention_freq_by_book, BookNN %in% input$radiobook)})

# create tables based off the dynamic dfs
output$table1 <- renderTable( head(agg_reactive()[c("Name", "Aggression Count")], 10), striped = TRUE, hover = TRUE, bordered = TRUE)
output$table2 <- renderTable( head(agg_freq_reactive()[c("Name", "Aggression Frequency")], 10), striped = TRUE, hover = TRUE, bordered = TRUE)
output$table3 <- renderTable( head(mentions_reactive()[c("Name", "Mentions")], 10), striped = TRUE, hover = TRUE, bordered = TRUE)
output$table4 <- renderTable( head(mention_freq_reactive()[c("Name", "Mention Frequency")], 10), striped = TRUE, hover = TRUE, bordered = TRUE)

# observe functions for the select all buttons
observe({
  if (input$selectall > 0) {
    if (input$selectall %% 2 == 0){
      updateCheckboxGroupInput(session=session, 
                               inputId="inputbook",
                               choices = unique(potter$Book),
                               selected = unique(potter$Book[1]))
      
    } else {
      updateCheckboxGroupInput(session=session, 
                               inputId="inputbook",
                               choices = unique(potter$Book),
                               selected = unique(potter$Book))
      
    }}
})

observe({
  if (input$selectall1 > 0) {
    if (input$selectall1 %% 2 == 0){
      updateCheckboxGroupInput(session=session, 
                               inputId="bookmentions",
                               choices = unique(potter$Book),
                               selected = unique(potter$Book[1]))
      
    } else {
      updateCheckboxGroupInput(session=session, 
                               inputId="bookmentions",
                               choices = unique(potter$Book),
                               selected = unique(potter$Book))
      
    }}
})

# text strings for the app info
output$appinfo <- renderUI({
  str1 <- paste(p(), "This app was developed by Emma Clarke using R/Shiny (with some data 
                wrangling help from Python) as a final project for Interactive 
                Information Visualization at the University of Washington's iSchool.")
  
  str2 <- paste("Although my original plan was to have mouseover tooltips for
                all of the graphs (not just the line/scatter plots), I couldn't
                find a library that allowed for the kind of flexibility I wanted. 
                To make up for the lack of tooltips on the bar graphs, I added
                the Summary by Book page, which shows stats for the top 10 characters
                in each category, as well as some additional publishing stats
                that I found interesting.")
  
  HTML(paste(str1[1], str2[1], sep = "<p/>"))
})

output$datainfo <- renderUI({
  str1 <- paste(p(), "'Aggressive' may not be the best way of describing the actions
  detailed in this data, but as that was the word used in the original dataset,
  I decided to keep it. They are defined in the dataset as 'attempts (successful or
                not) at causing physical harm to another living being'.")
  
  str2 <- paste(
    strong("Actions that ", em("are not"), "aggressive: "),
    br(),
    strong("-"),
    "Threats (pointing a wand or insulting someone)",
    br(),
    strong("-"),
    "Casting a mundane spell in a mundane context (e.g. summoning a dictionary)",
    br(),
    strong("-"),
    "Fred and George testing candies on volunteers"
  )
    
  str3 <- paste(
    strong("Actions that", em("are"), "aggressive: "),
    br(),
    strong("-"),
    "Third-party action (e.g. Umbridge gets blamed for her pen)",
    br(),
    strong("-"),
    "Spells in classes or DA meetings (e.g. Silencio, Stupefy)",
    br(),
    strong("-"),
    "Non-magical physical aggression",
    br(),
    strong("-"),
    "Failed spells or shooting sparks at another being",
    br(),
    strong("-"),
    "Actions both by and against boggarts",
    br(),
    strong("-"),
    "Mundane spells used aggressively (e.g. 'Accio Prophecy')"
    
  )
    
  str4 <- paste(
    "I thought it was interesting to focus on aggressions as well as the
    frequency of those aggressions (otherwise, poor Harry looks like a violent
    madman in every book). It only seemed natural to extend those visualizations 
    and show the count and frequency of character mentions as well.",
    p(),
    "It is important to note that this is not a comprehensive list of every 
    character in the books; some were in the dataset that I wasn't expecting,
    and some that I was expecting were missing (the Dursleys, for example). The 
    dataset contains 76 of the most aggressive (by count) characters in the series. 
    Some appear in only one or two books. This means that, for minor characters, their 
    'least mentioned' book may not even be in the dataset. In some books, there are so 
    few aggressions 
    that a character with 0 aggressive actions will show up on the chart. In these 
    cases, the graph defaults to the most mentioned character with no aggressions 
    (generally Dumbledore or Hagrid).",
    p(),
    "I added in a death count per book because I was curious to see if deaths
    correlated with aggressive actions (and sales). I only counted deaths that happened
    within the time period of the book - so ghosts don't count, and neither do Lily
    and James.",
    p(),
    "I used the American release date in the book title information for the graphs.",
    p(),
    "The data was collected manually, and as such, should be viewed with a grain of
    salt (and much appreciation for the work that went into its compilation)."
  )
  
  HTML(paste(str1[1], str2[1], str3[1], str4[1], sep = "<p/>"))
})


output$datasources <- renderUI({
  str1 <- paste("Original dataset:",
                br(),
    a("Google Doc", href = "https://docs.google.com/spreadsheets/d/1heSMqYzYnL5bS0xiZ2waReUMLKIxHce5MMsQxzhgaA8/edit#gid=1825799110", target = "_blank"),
    br(),
    "Explanation of data collection methods: ",
    br(),
    a("Reddit thread", href = "https://www.reddit.com/r/dataisbeautiful/comments/2zwjnw/aggressive_actions_within_harry_potter_oc/", target = "_blank"),
    br(),
    "From a helpful GitHub repository: ",
    br(),
    a(".csv file", href = "https://raw.githubusercontent.com/andrewheiss/Harry-Potter-aggression/master/harry_potter_aggression_full.csv", target = "_blank"),
    br(),
    "For reference on deaths and release dates: ",
    br(),
    a("Harry Potter Wiki", href = "http://harrypotter.wikia.com/wiki/Main_Page", target = "_blank"),
    br(),
    "For ratings information: ",
    br(),
    a("Harry Potter Goodreads page", href = "https://www.goodreads.com/series/45175-harry-potter", target = "_blank"),
    br(),
    "My edited dataset (data source for this app): ",
    br(),
    a("Google Doc", href = "https://drive.google.com/open?id=0B_BhIlhcbbB4ekp1MG1oWXlJa0E", target = "_blank")
  )

  HTML(paste(str1[1],  sep = "<p/>"))  
})

# create the small plots for the summary page
output$salesplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = CopiesSold, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Copies Sold (in millions)") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") 
}, height = 200, width = 200
)
output$lengthplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = Paragraphs, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Paragraphs") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") 
}, height = 200, width = 200
)
output$ratingsplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = AvgRating, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Average Rating") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") +
    ylim(0,5)
}, height = 200, width = 200
)
output$deathplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = Deaths, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Death Count") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") 
}, height = 200, width = 200
)
output$aggplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = Aggressions.y, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Aggressions (Count)") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") 
}, height = 200, width = 200
)
output$aggfplot <- renderPlot({
  ggplot(book_info, aes(x = Series, y = Agg_freq.y, fill = Book)) +
    geom_bar(stat = "identity") +
    labs(y = "Aggression Frequency") +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_fill_manual(values = c("#443E62","#B0344E","#363A3B","#B99366", "#113F60", "#3F4F45", "#Ef864C")) +
    theme(legend.position = "none") 
}, height = 200, width = 200
)
# explanation of small plots
output$wellpaneltext <- renderUI({
  str1 <- paste(em("Visualizations of the sidebar statistics (colors
                   are based on the US book covers):"))
  HTML(paste(str1[1]))
})

}
# publish
shinyApp(ui = ui, server = server)