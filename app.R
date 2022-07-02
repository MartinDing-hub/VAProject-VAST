##----------Load all the necessary packages----------##
packages <- c("shiny", "shinythemes", 
              "plotly", "tidyverse", "ggstatsplot", 
              "tools", "ggside", "lubridate",
              "hrbrthemes", "gt", "gtExtras",
              "reactable", "reactablefmtr", "htmltools",
              "ggiraph", "tmap", "png",
              "sf")

for (p in packages){
  library(p, character.only=T)
}
library("shinydashboard", lib.loc="D:\\R\\R-4.1.3\\library")


##----------Read all the data----------##
introduction <- scan("./data/introduction.txt", what="character", sep=" ")
df1 <- read_csv("./data/v1.csv")
df2 <- read_csv("./data/v2.csv")
df3 <- read_csv("./data/v3.csv")
df4 <- read_csv("./data/v4.csv")
df5 <- read_csv("./data/v5.csv")

health=read_csv('./data/health.csv')
jobs = read_csv('./data/Jobs.csv')
building=read_sf('./data/Buildings.csv',options='GEOM_POSSIBLE_NAMES=location') 
logs=read_sf('./data/log1.csv',options='GEOM_POSSIBLE_NAMES=currentLocation')

##----------Shiny UI----------##
ui <- navbarPage(
  title = "ShinyIDEA: Interactive Data Exploration and Analysis (Here, we need to change our own title!!!)",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction", 
           div(style="font-style:arial",
               div(style="font-size:30px",
                   div(style="text-align:justify",
                       verticalLayout(
                         imageOutput("ohio"),
                         div(style="padding-left:230px;padding-right:230px;padding-top:0px",
                             textOutput("intro_id")
                             )
                         )
                       )
                   )
              )
           ),

  tabPanel("Monthly Total Expense",
          sidebarLayout(
            sidebarPanel(width = 3,
                         selectInput(inputId = "category",
                           label = "Expense Type:",
                           choices = c("Education", "Food", "Recreation", "Shelter"),
                           selected = "Food"),
                         dateRangeInput(inputId = "date1", 
                                        label = "Date range:",
                                        start = "2022-04-01",
                                        end = "2023-05-01",
                                        min = "2022-03-01",
                                        max = "2023-05-31"),
                         textInput(inputId = "plotTitle1",
                                   label = "Plot title",
                                   placeholder = "Enter text to be used as plot title")
            ),
            mainPanel(width = 9,
                      box(plotOutput("Food_V1", 
                                     height = "400px",
                                     width = "1200px")))
          )
  ),
  
  tabPanel("Food Industry",
           sidebarLayout(
             sidebarPanel(width = 3,
                          dateRangeInput(inputId = "date2",
                                         label = "Date range:",
                                         start = "2022-04-01",
                                         end = "2023-05-01",
                                         min = "2022-03-01",
                                         max = "2023-05-31"),
                          textInput(inputId = "plotTitle2",
                                    label = "Plot title",
                                    placeholder = "Enter text to be used as plot title")
             ),
             mainPanel(width = 9,
                       box(plotOutput("Food_V2",
                                      height = "700px",
                                      width = "1200px")))
           )
  ),
  
  navbarMenu("Engel Index",
             tabPanel("Non-reactable",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     textInput(inputId = "min", 
                                               label = "Start ID:", 
                                               value = "0"),
                                     textInput(inputId = "max", 
                                               label = "End ID:", 
                                               value = "20"),
                                     dateRangeInput(inputId = "date3", 
                                                    label = "Date range:",
                                                    start = "2022-04-01",
                                                    end = "2023-05-01",
                                                    min = "2022-03-01",
                                                    max = "2023-05-31"),
                                     textInput(inputId = "plotTitle3",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title")
                                     ),
                        mainPanel(width = 9,
                                  box(gt_output("Engel1")))
                      )),
             tabPanel("Reactable", 
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     textInput(inputId = "pg_size", 
                                               label = "Page Size:", 
                                               value = "15"),
                                     dateRangeInput(inputId = "date4", 
                                                    label = "Date range:",
                                                    start = "2022-04-01",
                                                    end = "2023-05-01",
                                                    min = "2022-03-01",
                                                    max = "2023-05-31")
                                     ),
                        mainPanel(width = 9,
                                  box(reactableOutput("Engel2")))
                      ))),
  
  tabPanel("Living Expense Distribution", 
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput(inputId = "aspect",
                                      label = "Dimension:",
                                      choices = c("Education Level", "Happiness", "HaveKids"),
                                      selected = "Education Level"),
                          textInput(inputId = "plotTitle4",
                                    label = "Plot title",
                                    placeholder = "Enter text to be used as plot title"),
                          textInput(inputId = "subTitle1",
                                    label = "Subtitle",
                                    placeholder = "Enter text to be used as subtitle")),
             mainPanel(width = 9,
                       box(plotOutput("visualization4",
                                      height = "800px",
                                      width = "1000px")))
           )
  ),
  
  tabPanel("Savings", 
           sidebarLayout(
             sidebarPanel(width = 3,
                          sliderInput(inputId = "slider",
                                      label = "Number of Participants:",
                                      min = 40,
                                      max = 100,
                                      value = 60),
                          dateRangeInput(inputId = "date5", 
                                         label = "Date range:",
                                         start = "2022-04-01",
                                         end = "2023-05-01",
                                         min = "2022-03-01",
                                         max = "2023-05-31")
                          ),
             mainPanel(width = 9,
                       box(plotOutput("visualization5",
                                      height = "800px",
                                      width = "800px")))
           )
  ),
  
  navbarMenu("Employment",
             tabPanel("Wage of Employees",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "industry1",
                                                 label = "Industry Type:",
                                                 choices = c("All", "Pub", "Restaurant", "Other"),
                                                 selected = "All"),
                                     textInput(inputId = "title_N1_1",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title")
                        ),
                        mainPanel(width = 9,
                                  box(girafeOutput("N_1_1")))
                      )),
             tabPanel("Business Size",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput(inputId = "industry2",
                                                 label = "Industry Type:",
                                                 choices = c("All", "Pub", "Restaurant", "Other"),
                                                 selected = "All"),
                                     textInput(inputId = "title_N1_2",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title")
                        ),
                        mainPanel(width = 9,
                                  box(girafeOutput("N_1_2")))
                      )),
             tabPanel("Employment pattern",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     textInput(inputId = "title_N1_3",
                                               label = "Plot title",
                                               placeholder = "Enter text to be used as plot title")
                        ),
                        mainPanel(width = 9,
                                  box(girafeOutput("N_1_3")))
                      )
             )
  ),
  
  tabPanel("Regional Turnover",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput(inputId = "industry4",
                                     label = "Industry Type:",
                                     choices = c("Commercial", "Residental"),
                                     selected = "Commercial"),
                          textInput(inputId = "start_time", 
                                   label = "Start time (Hour):", 
                                   value = "8"),
                          textInput(inputId = "end_time", 
                                   label = "End time (Hour):", 
                                   value = "9"),
                          textInput(inputId = "title_N2_1",
                                   label = "Plot title",
                                   placeholder = "Enter text to be used as plot title")
                         ),
             mainPanel(width = 9,
                       box(tmapOutput("N_2_1"))))
  ),
  
  tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }
                   .datepicker { z-index:99999 !important; }")) # center text
)

##----------Shiny Server----------##
server <- function(input, output){
  ## Introduction text
  output$intro_id <- renderText({introduction})
  
  output$ohio <- renderImage({
    imgpng <- normalizePath(file.path("./image/Ohio.png"))
    list(src = imgpng,
         width = "100%",
         height = "175")
  }, deleteFile = FALSE)
  
  ## V1: Food
  output$Food_V1 <- renderPlot({
    title1 <- input$plotTitle1
    
    cat <- input$category
    
    if (title1 == ""){
      tit <- paste0("Monthly Total Expense of ", cat)
    } else {
      tit <- title1
    }
    
    start_date <- format(input$date1[1], "%Y-%m")
    end_date <- format(input$date1[2], "%Y-%m")
    
    df1[is.na(df1)] <- 0
    
    df1 <- df1 %>%
      group_by(category, date) %>%
      summarise(amt = sum(amount))
    df1$date <- format(df1$date, "%Y-%m")
    
    ggplot(filter(df1, category == cat & date >= start_date & date <= end_date), 
           aes(x=date, y=amt, group=1)) + 
      geom_line(color="gray50") +
      geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
      labs(title = tit,
           caption = "Data source: Financial Jornal",
           x = "", y = "") + 
      theme_light() +
      theme(
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")
      )
  })
  
  ## V2: Monthly Trend
  output$Food_V2 <- renderPlot({
    start_date <- format(input$date2[1], "%Y-%m")
    end_date <- format(input$date2[2], "%Y-%m")
    
    title2 <- input$plotTitle2
    
    if (title2 == ""){
      tit <- "Monthly Trend: Food Popularity vs Salary"
    } else {
      tit <- title2
    }
    
    df2$date <- format(df2$date, "%Y-%m")
    res <- df2 %>%
      filter(date <= end_date & date >= start_date) %>%
      group_by(date) %>%
      summarise(popularity=sum(hour), avg_wage=mean(amount))
    
    res1 <- res[,c(1, 2)] %>%
      mutate(variable = 'popularity') %>%
      rename(val=popularity)
    res2 <- res[,c(1, 3)] %>%
      mutate(variable = 'avg_salary') %>%
      rename(val=avg_wage)
    res <- rbind(res1, res2)
    
    ggplot(filter(res, date >= start_date & date <= end_date), 
           aes(x=date, y=val, group=1)) + 
      geom_line(color="steelblue") +
      geom_point(shape=21, color="gray80", fill="#3d5a80", size=3) + 
      facet_grid(variable ~.,scales = "free_y") + 
      labs(title = tit,
           caption = "Data source: Financial Jornal & Activity Logs",
           x = "", y = "") +
      theme_light() +
      theme(
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")
      )
  })
  
  ## V3: Non-reactable
  output$Engel1 <- render_gt({
    title3 <- input$plotTitle3
    
    if (title3 == ""){
      tit <- "The Engel Index of the participants"
    } else {
      tit <- title3
    }
    
    df3$date <- format(df3$date, "%Y-%m")
    min <- input$min
    max <- input$max
    min <- strtoi(min)
    max <- strtoi(max)
    cands = min:max
    
    start_date <- format(input$date3[1], "%Y-%m")
    end_date <- format(input$date3[2], "%Y-%m")
    
    spark <- df3 %>%
      filter(date >= start_date & date <= end_date & participantId %in% cands ) %>%
      group_by(participantId) %>%
      summarise('Engel Index' = list(engel_idx), .groups = "drop")
    e_index <- df3 %>% 
      filter(date >= start_date & date <= end_date & participantId %in% cands ) %>%
      group_by(participantId) %>% 
      summarise("Min" = min(engel_idx, na.rm = T),
                "Max" = max(engel_idx, na.rm = T),
                "Average" = mean(engel_idx, na.rm = T)
      )
    engel_index_data <- left_join(e_index, spark)
    engel_index_data %>%
      gt() %>%
      gt_plt_sparkline('Engel Index') %>%
      fmt_number(columns = 2:4,
                 decimals = 3) %>%
      tab_header(
        title = md(tit)
      )
  },
  height = px(1000),
  width = px(1000))
  
  ## V3: Reactable
  output$Engel2 <- renderReactable({
    df3$date <- format(df3$date, "%Y-%m")
    pagesize <- input$pg_size
    pagesize <- strtoi(pagesize)

    start_date <- format(input$date4[1], "%Y-%m")
    end_date <- format(input$date4[2], "%Y-%m")

    report <- df3 %>%
      filter(date >= start_date & date <= end_date) %>%
      group_by(participantId) %>%
      summarize(Engel_Index = list(engel_idx))
    reactable(
      report,
      defaultPageSize = pagesize,
      columns = list(participantId = colDef(maxWidth = 130),
                     Engel_Index = colDef(
                       maxWidth = 200,
                       cell = react_sparkline(
                         report, decimals = 3,
                         highlight_points = highlight_points(min = "red", max = "blue"),
                         statline = "mean",
                         bandline = "innerquartiles",
                         bandline_color = "green"))
      )
    )
  })
  
  ## V4: Living Expense Distribution
  output$visualization4 <- renderPlot({
    var <- input$aspect
    
    df4 <- df4 %>%
      group_by(participantId, educationLevel, mood_type, haveKids) %>%
      summarise(
        Education = mean(edu_ratio),
        Food = mean(food_ratio),
        Recreation = mean(recreation_ratio),
        Shelter = mean(shelter_ratio))
    
    if (var == "Education Level"){
      title4 <- input$plotTitle4
      subtit1 <- input$subTitle1
      
      if (title4 == ""){
        tit <- "Living Expense Distribution for Participants"
      } else {
        tit <- title4
      }
      
      if (subtit1 == ""){
        subtit <- "With different Education Level"
      } else {
        subtit <- subtit1
      }
      
      res <- rbind(
        df4[,c(1, 2, 5)] %>%
          group_by(educationLevel) %>%
          summarise(ratio = mean(Education)) %>%
          mutate(type = 'Education'),
        df4[,c(1, 2, 6)] %>%
          group_by(educationLevel) %>%
          summarise(ratio = mean(Food)) %>%
          mutate(type = 'Food'),
        df4[,c(1, 2, 7)] %>%
          group_by(educationLevel) %>%
          summarise(ratio = mean(Recreation)) %>%
          mutate(type = 'Recreation'),
        df4[,c(1, 2, 8)] %>%
          group_by(educationLevel) %>%
          summarise(ratio = mean(Shelter)) %>%
          mutate(type = 'Shelter')
      )
      ggplot(res,
             aes(x = ratio,
                 xend = 0,
                 y = type,
                 yend = type,
                 colour = type)) +
        facet_grid(educationLevel~., scales = "free_y", space = "free") +
        geom_segment() +
        geom_point(color="orange", size=3) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_y_discrete(limits = rev) +
        scale_colour_manual(breaks = c("Education", "Food", "Recreation",
                                       "Shelter"),
                            values = c("#DE3533", "#0047AB", "#006644",
                                       "#10C25B")) +
        labs(x = "",
             y = "",
             title = tit,
             subtitle = subtit,
             caption = "Data source: Financial Journal") +
        theme_light() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "off",
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")
        )
    }else if (var == "Happiness"){
      title4 <- input$plotTitle4
      subtit1 <- input$subTitle1
      
      if (title4 == ""){
        tit <- "Living Expense Distribution for Participants"
      } else {
        tit <- title4
      }
      
      if (subtit1 == ""){
        subtit <- "With different Mood Type"
      } else {
        subtit <- subtit1
      }
      
      res <- rbind(
        df4[,c(1, 3, 5)] %>%
          group_by(mood_type) %>%
          summarise(ratio = mean(Education)) %>%
          mutate(type = 'Education'),
        df4[,c(1, 3, 6)] %>%
          group_by(mood_type) %>%
          summarise(ratio = mean(Food)) %>%
          mutate(type = 'Food'),
        df4[,c(1, 3, 7)] %>%
          group_by(mood_type) %>%
          summarise(ratio = mean(Recreation)) %>%
          mutate(type = 'Recreation'),
        df4[,c(1, 3, 8)] %>%
          group_by(mood_type) %>%
          summarise(ratio = mean(Shelter)) %>%
          mutate(type = 'Shelter')
      )
      ggplot(res,
             aes(x = ratio,
                 xend = 0,
                 y = type,
                 yend = type,
                 colour = type)) +
        facet_grid(mood_type~., scales = "free_y", space = "free") +
        geom_segment() +
        geom_point(color="orange", size=3) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_y_discrete(limits = rev) +
        scale_colour_manual(breaks = c("Education", "Food", "Recreation",
                                       "Shelter"),
                            values = c("#DE3533", "#0047AB", "#006644",
                                       "#10C25B")) +
        labs(x = "",
             y = "",
             title = tit,
             subtitle = subtit,
             caption = "Data source: Financial Journal") +
        theme_light() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "off",
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")
        )
    }else if (var == "HaveKids"){
      title4 <- input$plotTitle4
      subtit1 <- input$subTitle1
      
      if (title4 == ""){
        tit <- "Living Expense Distribution for Participants"
      } else {
        tit <- title4
      }
      
      if (subtit1 == ""){
        subtit <- "Have/Not Have Child"
      } else {
        subtit <- subtit1
      }
      
      res <- rbind(
        df4[,c(1, 4, 5)] %>%
          group_by(haveKids) %>%
          summarise(ratio = mean(Education)) %>%
          mutate(type = 'Education'),
        df4[,c(1, 4, 6)] %>%
          group_by(haveKids) %>%
          summarise(ratio = mean(Food)) %>%
          mutate(type = 'Food'),
        df4[,c(1, 4, 7)] %>%
          group_by(haveKids) %>%
          summarise(ratio = mean(Recreation)) %>%
          mutate(type = 'Recreation'),
        df4[,c(1, 4, 8)] %>%
          group_by(haveKids) %>%
          summarise(ratio = mean(Shelter)) %>%
          mutate(type = 'Shelter')
      )
      ggplot(res,
             aes(x = ratio,
                 xend = 0,
                 y = type,
                 yend = type,
                 colour = type)) +
        facet_grid(haveKids~., scales = "free_y", space = "free") +
        geom_segment() +
        geom_point(color="orange", size=3) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_y_discrete(limits = rev) +
        scale_colour_manual(breaks = c("Education", "Food", "Recreation",
                                       "Shelter"),
                            values = c("#DE3533", "#0047AB", "#006644",
                                       "#10C25B")) +
        labs(x = "",
             y = "",
             title = tit,
             subtitle = subtit,
             caption = "Data source: Financial Journal") +
        theme_light() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "off",
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold")
        )
    }
  })
  
  ## V5: Savings
  output$visualization5 <- renderPlot({
    var <- input$slider
    
    start_date <- format(input$date5[1], "%Y-%m")
    end_date <- format(input$date5[2], "%Y-%m")
    
    df5$date <- format(df5$date, "%Y-%m")
    cands <- sample(c(0:max(df5$participantId)), size=var)
    data <- df5 %>%
      filter(date >= start_date & date <= end_date & participantId %in% cands) %>%
      group_by(participantId, educationLevel) %>%
      summarise(saving_rate = mean(saving_rate)) %>%
      ungroup() %>%
      rename(individual = participantId, group = educationLevel, value = saving_rate)
    data$individual <- paste0("pId ", data$individual)
    data$value <- data$value * 100
    
    empty_bar <- 2
    to_add <- data.frame( matrix(NA, empty_bar * length(unique(data$group)), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(unique(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) - empty_bar) %>% 
      rowwise() %>% 
      mutate(title=median(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-100,120) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE)
  })
  
  #N plot 1-1:
  output$N_1_1 <- renderGirafe({
    title_n1_1 <- input$title_N1_1
    
    ind <- input$industry1
    
    if (title_n1_1 == ""){
      tit <- "Hourly wage of employers"
    } else {
      tit <- title_n1_1
    }
    
    if (ind == "All"){
      industry=c("Pub", "Restaurant", "Other")
    } else if (ind == "Pub"){
      industry=c("Pub")
    } else if (ind == "Restaurant"){
      industry=c("Restaurant")
    } else if (ind == "Other"){
      industry=c("Other")
    }
    
    health_industry=health%>% filter(type %in% industry)
    
    p1 <- ggplot(data=health_industry, 
                 aes(x = avg_wage,
                     data_id=type)) +
      geom_dotplot_interactive(
        aes(tooltip = tooltip),
        stackgroups = TRUE,
        binwidth = 0.7,
        method = "histodot") +
      scale_y_continuous(NULL,               
                         breaks = NULL)+
      geom_vline(xintercept =mean(health_industry$avg_wage))+
      ggtitle(tit)
    
    girafe(
      ggobj = p1,
      width_svg = 8,
      height_svg = 8*0.618,
      options = list(                        
        opts_hover(css = "fill: #202020;"),  
        opts_hover_inv(css = "opacity:0.2;") 
      )     
    )
  })
  
  #N plot 1-2:
  output$N_1_2 <- renderGirafe({
    title_n1_2 <- input$title_N1_2
    
    ind <- input$industry2
    
    if (title_n1_2 == ""){
      tit <- "Size of employers"
    } else {
      tit <- title_n1_2
    }
    
    if (ind == "All"){
      industry=c("Pub", "Restaurant", "Other")
    } else if (ind == "Pub"){
      industry=c("Pub")
    } else if (ind == "Restaurant"){
      industry=c("Restaurant")
    } else if (ind == "Other"){
      industry=c("Other")
    }
    
    health_industry=health%>% filter(type %in% industry)
    
    p2 <- ggplot(data=health_industry, 
                 aes(x = no_jobs)) +
      geom_dotplot_interactive(
        aes(tooltip = tooltip,
            data_id=type),
        stackgroups = TRUE,
        binwidth = 0.1,
        method = "histodot") +
      scale_y_continuous(NULL,               
                         breaks = NULL)+
      geom_vline(xintercept = mean(health_industry$no_jobs))+
      ggtitle(tit)
    
    girafe(
      ggobj = p2,
      width_svg = 8,
      height_svg = 8*0.618,
      options = list(                        
        opts_hover(css = "fill: #202020;"),  
        opts_hover_inv(css = "opacity:0.2;") )
    )
  })
  
  #N plot 1-3:
  output$N_1_3 <- renderGirafe({
    title_n1_3 <- input$title_N1_3
    
    if (title_n1_3 == ""){
      tit <- "Wage vs Education"
    } else {
      tit <- title_n1_3
    }
    
    median_wage=median(jobs$hourlyRate)
    mean_wage=mean(jobs$hourlyRate)
    v3=jobs %>%
      ggplot(aes(x=educationRequirement,y=hourlyRate)) +
      geom_point() +
      scale_x_discrete(limits = c("Low", "HighSchoolOrCollege", "Bachelors", 
                                  "Graduate"),
                       labels = c("Low", "HighSchoolOrCollege", "Bachelors", 
                                  "Graduate"
                       ))+
      ggtitle(tit)+
      geom_hline(yintercept = median_wage) +
      geom_hline(yintercept = mean_wage)+
      geom_boxplot()
    
    girafe(
      ggobj = v3,
      width_svg = 8,
      height_svg = 8*0.618,
      options = list(                        
        opts_hover(css = "fill: #202020;"),  
        opts_hover_inv(css = "opacity:0.2;") )
    )
  })
  
  #N plot 2-1:
  output$N_2_1 <- renderTmap({
    title_n2_1 <- input$title_N2_1
    
    ind <- input$industry4
    
    if (title_n2_1 == ""){
      tit <- "Turnover within time for specifit type building"
    } else {
      tit <- title_n2_1
    }
    
    if (ind == "Commercial"){
      yourtype=c("Commercial")
    } else if (ind == "Residental"){
      yourtype=c("Residental")
    }
    
    start_time <- input$start_time
    end_time <- input$end_time
    start_time <- strtoi(start_time)
    end_time <- strtoi(end_time)
    
    yourtime=c(start_time, end_time)
    
    building_type=filter(building, buildingType %in% yourtype)
    logs_time=filter(logs, between(timestamp, yourtime[1], yourtime[2]))
    points_in_building <- st_join(logs_time, 
                                  building_type, 
                                  join=st_within) %>%
      st_set_geometry(NULL) %>%
      count(name='pointCount', buildingId)
    head(points_in_building)
    
    logs_combined <- building_type %>%
      left_join(points_in_building, 
                by = 'buildingId') %>%
      replace(is.na(.), 0)
    
    tm_shape(logs_combined %>%
               filter(pointCount > 0))+
      tm_fill("pointCount",
              n = 8,
              style = "quantile")+
      tm_borders(alpha = 0.1)+
      tm_layout(tit,
                legend.title.size = 1,
                legend.text.size = 0.3,
                legend.position = c("left","bottom"),
                legend.bg.color = "white",
                legend.bg.alpha = 1)
  })
  
  #V8
}

shinyApp(ui = ui, server = server)
