library(rsconnect)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(xlsx)
names(tags)
rsconnect::setAccountInfo(name='mdaheeeh',
                          token='2BD6C3609E0535EFBF5ACD5A912F1472',
                          secret='3V91S8lu2Akn5d6Y5BhQRzW3bBFo9tTgwIpLe5in')

ui <- fluidPage(
  tags$head(tags$link(rel = 'stylesheet', href = 'css.css')),
  wellPanel(fluidRow(tags$h1("Журнал успеваемости", id = "title")), id = 't'),
  navbarPage(
    tabPanel("Навигация по сайту", id = "f"),)
  navlistPanel(
    tabPanel
      ( "Файл", wellPanel(
          "Импорт файла",
          fileInput(
            label = '',
            inputId = 'fileimp',
            buttonLabel = 'Выбрать файл',
            placeholder = 'Файл не выбран',
            accept = '.csv, .xlsx'
            ),
          tags$p('Создание новой таблицы', id = 'Message'),
          actionButton(inputId = 'Create',label = 'Создать пустую таблицу')
        )
      ),
  )
    
    tabPanel("Таблица", wellPanel(id = 'Output', 
                                  DTOutput('table'), 
                                  actionButton('add_row', 'Добавить новую строку'),
                                  actionButton('remove_row', 'Удалить строку'),
                                  downloadButton("download", label = "Сохранить изменения")
    )),
    
    navbarMenu("Статистика",
      tabPanel("Классная статистика по предметам", wellPanel(id = 'Output',
                                                  DTOutput('stats_by_class_sub')), id = 'class_sub' ),
      
      tabPanel("Классная статистика по оценкам", wellPanel(id = 'Output',
                                                             DTOutput('stats_by_class_marks')                                           
      )),
      
      tabPanel("Общая статистика по предметам", wellPanel(DTOutput('stats_general_sub'))),
      
      tabPanel("Общая статистика по оценкам", wellPanel(DTOutput('stats_general_marks'))),
      
     
    ),
    
    navbarMenu("Статистика в графиках",
               
    tabPanel("График классной статистики по предметам", wellPanel(plotOutput('stats_graph_k_sub'))),
    tabPanel("График классной статистики по оценкам", wellPanel(plotOutput('stats_graph_k_mar'))),
    tabPanel("График общей статистики по предметам", wellPanel(plotOutput('stats_graph_g_sub'))),
    tabPanel("График общей статистики по оценкам", wellPanel(plotOutput('stats_graph_g_mar'))),
    
    ),
    
    
    tabPanel("Справка", wellPanel(textOutput('help1'),
                                  textOutput('help_file'),
                                  textOutput('help_table'),
                                  textOutput('help_stat'),
                                  textOutput('help_graphs'), id = 'help')),
    tabPanel("О разработчике", wellPanel(imageOutput('av'), textOutput('dev'), textOutput('contacts')),
    )))

  


server <- function(input, output)
{
  rv <- reactiveValues(data = NULL)
  
  observeEvent(input$Create,
    {
       data = data.frame(Имя = "Иванов Иван Иванович", Класс = "", Информатика = NA, Физика = NA, Математика = NA, Литература = NA, Музыка = NA) 
       rv$data <- data
    })
  
  file_import <- reactive(
    {
      req(input$fileimp$datapath)
      
      file <- input$fileimp$datapath
      data = as.data.frame(switch(tools::file_ext(file),
                                  csv = data.frame(read.csv2(file, sep = ';', encoding = 'utf-8')),
                                  xlsx = data.frame(read.xlsx2(file, sheetIndex = 1))      
      ))
      rv$data <- data
    }
  )
  
  observeEvent(file_import(), {
    rv$data <- file_import()
  })
  
  observeEvent(input$add_row,
               {
                 newrow <- data.frame(Имя = "", Класс = "", Информатика = NA, Физика = NA, Математика = NA, Литература = NA, Музыка = NA)
                 
                 rv$data <- rbind(rv$data, newrow)
               }
  )
  
  observeEvent(input$remove_row, {
    to_remove <- input$table_rows_selected
    if (length(to_remove) > 0) {
      rv$data <- rv$data[-to_remove, ]
    }
  })
  
  output$download <- downloadHandler(
    filename = "Журнал.csv",
    content = function(file) {
      write.csv2(rv$data, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    rv$data[row, col] <- value
  })
  
  rv_edited_data <- reactive({
    if (!is.null(rv$data)) {
      data.frame(rv$data)
    }
  })
  
  output$table <- renderDT({
    if (!is.null(input$fileimp$datapath)|!is.null(rv$data)){
      if (is.null(rv_edited_data())) {
        rv$data <- file_import()
      } else {
        rv$data <- rv_edited_data()
      }
      datatable(rv$data, editable = TRUE, list(pageLength = 100))
    }
  })
  
  stats_class_sub <- reactive(
    {
      if (!is.null(rv$data)){
        data <- as.data.frame(rv$data) %>%
          group_by(Класс) %>%
          summarize(
            "Среднее, информатика" = round(mean(as.numeric(Информатика), na.rm = TRUE),2),
            "Медиана, информатика" = median(as.numeric(Информатика), na.rm = TRUE),
            "Среднее, физика" = round(mean(as.numeric(Физика), na.rm = TRUE),2),
            "Медиана, физика" = median(as.numeric(Физика), na.rm = TRUE),
            "Среднее, математика" = round(mean(as.numeric(Математика), na.rm = TRUE),2),
            "Медиана, математика" = median(as.numeric(Математика), na.rm = TRUE),
            "Среднее, литература" = round(mean(as.numeric(Литература), na.rm = TRUE),2),
            "Медиана, литература" = median(as.numeric(Литература), na.rm = TRUE),
            "Среднее, музыка" = round(mean(as.numeric(Музыка), na.rm = TRUE),2),
            "Медиана, музыка" = median(as.numeric(Музыка), na.rm = TRUE),
          )
        data
      }
    }
  )
  output$stats_by_class_sub <- renderDT({
    datatable(stats_class_sub())
  })
  
  stats_class_marks <- reactive(
    {
      if (!is.null(rv$data)){
        data <- as.data.frame(rv$data) %>%
          group_by(Класс) %>%
          summarize(
            
            "Информатика 5, кол-во" = sum(as.numeric(Информатика) == 5),
            "Информатика 5, %" = round(sum(as.numeric(Информатика) == 5)/ n() * 100, 2), 
            "Информатика 4, кол-во" = sum(as.numeric(Информатика) == 4),
            "Информатика 4, %" = round(sum(as.numeric(Информатика) == 4)/ n() * 100, 2), 
            "Информатика 3, кол-во" = sum(as.numeric(Информатика) == 3),
            "Информатика 3, %" = round(sum(as.numeric(Информатика) == 3)/ n() * 100, 2), 
            "Информатика 2, кол-во" = sum(as.numeric(Информатика) == 2),
            "Информатика 2, %" = round(sum(as.numeric(Информатика) == 2)/ n() * 100, 2), 
            "Физика 5, кол-во" = sum(as.numeric(Информатика) == 5),
            "Физика 5, %" = round(sum(as.numeric(Физика) == 5)/ n() * 100, 2), 
            "Физика 4, кол-во" = sum(as.numeric(Информатика) == 4),
            "Физика 4, %" = round(sum(as.numeric(Физика) == 4)/ n() * 100, 2), 
            "Физика 3, кол-во" = sum(as.numeric(Информатика) == 3),
            "Физика 3, %" = round(sum(as.numeric(Физика) == 3)/ n() * 100, 2), 
            "Физика 2, кол-во" = sum(as.numeric(Информатика) == 2),
            "Физика 2, %" = round(sum(as.numeric(Физика) == 2)/ n() * 100, 2), 
            "Математика 5, кол-во" = sum(as.numeric(Информатика) == 5),
            "Математика 5, %" = round(sum(as.numeric(Математика) == 5)/ n() * 100, 2), 
            "Математика , кол-во" = sum(as.numeric(Информатика) == 4),
            "Математика 4, %" = round(sum(as.numeric(Математика) == 4)/ n() * 100, 2), 
            "Математика 3, кол-во" = sum(as.numeric(Информатика) == 3),
            "Математика 3, %" = round(sum(as.numeric(Математика) == 3)/ n() * 100, 2), 
            "Математика 2, кол-во" = sum(as.numeric(Информатика) == 2),
            "Математика 2, %" = round(sum(as.numeric(Математика) == 2)/ n() * 100, 2), 
            "Литература 5, кол-во" = sum(as.numeric(Информатика) == 5),
            "Литература 5, %" = round(sum(as.numeric(Литература) == 5)/ n() * 100, 2), 
            "Литература 4, кол-во" = sum(as.numeric(Информатика) == 4),
            "Литература 4, %" = round(sum(as.numeric(Литература) == 4)/ n() * 100, 2), 
            "Литература 3, кол-во" = sum(as.numeric(Информатика) == 3),
            "Литература 3, %" = round(sum(as.numeric(Литература) == 3)/ n() * 100, 2), 
            "Литература 2, кол-во" = sum(as.numeric(Информатика) == 2),
            "Литература 2, %" = round(sum(as.numeric(Литература) == 2)/ n() * 100, 2), 
            "Музыка 5, кол-во" = sum(as.numeric(Информатика) == 5),
            "Музыка 5, %" = round(sum(as.numeric(Музыка) == 5)/ n() * 100, 2), 
            "Музыка 4, кол-во" = sum(as.numeric(Информатика) == 4),
            "Музыка 4, %" = round(sum(as.numeric(Музыка) == 4)/ n() * 100, 2), 
            "Музыка 3, кол-во" = sum(as.numeric(Информатика) == 3),
            "Музыка 3, %" = round(sum(as.numeric(Музыка) == 3)/ n() * 100, 2), 
            "Музыка 2, кол-во" = sum(as.numeric(Информатика) == 2),
            "Музыка 2, %" = round(sum(as.numeric(Музыка) == 2)/ n() * 100, 2), 
          )
        data
      }
    }
  )
  output$stats_by_class_marks <- renderDT({
    datatable(stats_class_marks())
  })
  
  
 stats_marks <- reactive({
   
   data <- as.data.frame(rv$data)
   n <- nrow(data)
   
   infn<-list()
   infp<-list()
   for(i in 5:2)
   {
     b<- sum(data$Информатика == i)
     infn<-append(infn,b)
     infp<-append(infp,round(b/n * 100, 2))
   }   
   
   fizn<-list()
   fizp<-list()
   for(i in 5:2)
   {
     b<- sum(data$Физика == i)
     fizn<-append(fizn,b)
     fizp<-append(fizp,round(b/n * 100, 2))
   } 
   
   mathn<-list()
   mathp<-list()
   for(i in 5:2)
   {
     b<- sum(data$Математика == i)
     mathn<-append(mathn,b)
     mathp<-append(mathp,round(b/n * 100, 2))
   } 

   litn<-list()
   litp<-list()
   for(i in 5:2)
   {
     b<- sum(data$Литература == i)
     litn<-append(litn,b)
     litp<-append(litp,round(b/n * 100, 2))
   } 

   musn<-list()
   musp<-list()
   for(i in 5:2)
   {
     b<- sum(data$Музыка == i)
     musn<-append(musn,b)
     musp<-append(musp,round(b/n * 100, 2))
   } 
   
   data1 <- data.frame(
     "Оценка" = c(5, 4, 3, 2),
     "Информатика,кол" = unlist(infn),
     "Информатика,проц" = unlist(infp),
     "Физика,кол" = unlist(fizn),
     "Физика,проц" = unlist(fizp),
     "Математика,кол" = unlist(mathn),
     "Математика,проц" = unlist(mathp),
     "Литература,кол" = unlist(litn),
     "Литература,проц" = unlist(litp),
     "Музыка,кол" = unlist(musn),
     "Музыка,проц" = unlist(musp))
 })
 
 output$stats_general_marks <- renderDT(stats_marks())
 
 stats_sub <- reactive(
   {
     if (!is.null(rv$data)){
       data <- as.data.frame(rv$data) %>%
         summarize(
           "Среднее, информатика" = round(mean(as.numeric(Информатика), na.rm = TRUE),2),
           "Медиана, информатика" = median(as.numeric(Информатика), na.rm = TRUE),
           "Среднее, физика" = round(mean(as.numeric(Физика), na.rm = TRUE),2),
           "Медиана, физика" = median(as.numeric(Физика), na.rm = TRUE),
           "Среднее, математика" = round(mean(as.numeric(Математика), na.rm = TRUE),2),
           "Медиана, математика" = median(as.numeric(Математика), na.rm = TRUE),
           "Среднее, литература" = round(mean(as.numeric(Литература), na.rm = TRUE),2),
           "Медиана, литература" = median(as.numeric(Литература), na.rm = TRUE),
           "Среднее, музыка" = round(mean(as.numeric(Музыка), na.rm = TRUE),2),
           "Медиана, музыка" = median(as.numeric(Музыка), na.rm = TRUE),
         )
       data
     }
   }
 )
 output$stats_general_sub <- renderDT({
   datatable(stats_sub())
 })
 
 output$stats_graph_k_sub <- renderPlot({
   if (!is.null(stats_class_sub())){
   data <- stats_class_sub()
   data_long <- tidyr::pivot_longer(data, cols = -c("Класс"), names_to = "name", values_to = "value")
   ggplot(data_long, aes(x = name, y = value, fill = Класс)) +
     geom_col(position = "dodge") +
     labs(title = "Средняя успеваемость по классам", x = "Дисциплина", y = "Значение") +
     theme_minimal()}
 })
 
 output$stats_graph_k_mar <- renderPlot({
   if (!is.null(stats_class_marks())){
     data <- stats_class_marks()
     data_long <- tidyr::pivot_longer(data, cols = -c("Класс"), names_to = "name", values_to = "value")
     ggplot(data_long, aes(x = name, y = value, fill = Класс)) +
       geom_col(position = "dodge") +
       labs(title = "Распределение оценок по классам", x = "Дисциплина", y = "Значение") +
       theme_minimal()}
 })
 
 output$stats_graph_g_sub <- renderPlot({
   if (!is.null(stats_sub())){
     data <- stats_sub()
     data_long <- tidyr::pivot_longer(data, cols, names_to = "name", values_to = "value")
     ggplot(data_long, aes(x = name, y = value, fill )) +
       geom_col(position = "dodge") +
       labs(title = "Средняя успеваемость", x = "Дисциплина", y = "Значение") +
       theme_minimal()}
 })
 
 output$stats_graph_g_mar <- renderPlot({
   if (!is.null(stats_marks())){
     data <- stats_marks()
     data_long <- tidyr::pivot_longer(data, cols, names_to = "name", values_to = "value")
     ggplot(data_long, aes(x = name, y = value, fill = )) +
       geom_col(position = "dodge") +
       labs(title = "Распределение оценок", x = "Дисциплина", y = "Значение") +
       theme_minimal()}
 })
 
 output$help1 <- renderText("Помощь в использовании программы.")
 output$help_file <- renderText("Вкладка 'Файл' предназначена для формирования исходных данных. Предусмотрена возможность импорта файлов формата .csv и .xlsx, а также возможность генерации пустой таблицы.")
 output$help_table <- renderText(" Вкладка 'Таблица' позволяет просмотреть данные в формате таблицы. Кнопки внизу позволяют добавить пустую строку или удалить выбранные строки. Двойное нажатие по ячейке таблицы позволит редактировать её значение. Кнопка 'Сохранить изменения' позволитяет скачать файл в виде таблицы Excel.")
 output$help_stat <- renderText("Вкладка 'Статистика' позволит увидеть успеваемость каждого класса и учеников в общем; отображает параметры: средняя и медианная оценка по предметам, количество и процент учеников, имеющих определенную оценку по каждому предмету")
 output$help_graphs <- renderText(" Вкладка 'Статистика в графиках' отобразит визуальную версию вкладки 'Статистика'.")
 
 output$av <- renderImage(
   {
     list(src = pic, width = '300px')
   },
   deleteFile = FALSE)
 
 output$dev <- renderText("Выполнил: студент гр. АБ-110, Минаев Тиммофей Дмитриевич")
 output$contacts <- renderText("Telegram: @AlstorRedivile, VK: @redivile")
 pic <- "ava.jpg"
}

shinyApp(ui, server)
