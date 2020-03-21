library(rwhatsapp)
library(dplyr)
library("ggplot2")
library("tidytext")
library("ggimage")
library("tidyr")
library("plotly")
library("wordcloud2")
library("data.table")
library("lubridate")
library(shinyWidgets)


shinyServer(function(input, output) {
    
    getData <- reactive({
        req(input$file)
        
        # tryCatch(
        #     {
        #         chat <- rwa_read(input$file$datapath)%>%
        #             filter(!is.na(author)) # remove messages without author
        #     },
        #     error = function(e) {
        #         # return a safeError if a parsing error occurs
        #         stop(safeError(e))
        #     }
        # )
        
        
        ext <- tools::file_ext(input$file$name)
        
        if(ext=="txt" | ext=="csv"){
            chat <- rwa_read(input$file$datapath)%>%
                        filter(!is.na(author)) # remove messages without author
            return(chat)
            
        }else{
            validate("Invalid file: Please upload a .csv or .txt file")
        }
        
    })
    
    
    output$usersBox <- shinydashboard::renderValueBox({
        req(file)

        val= levels(getData()$author) %>% length()
        # levels(chat$author) %>% length()
        shinydashboard::valueBox(val,"Members", icon = icon("users"),
            color = "purple"
        )
    })
    
    
    
    
    
    output$messagesBox <- shinydashboard::renderValueBox({
        req(file)

        shinydashboard::valueBox(nrow(getData()), "Messages", icon = icon("comments"), color = "yellow" )
    })
    
    output$includeMediasBox<-shinydashboard::renderValueBox({
        req(file)
        val=getData()$text %>% grepl("Media", .) %>% sum
        shinydashboard::valueBox(val,"Medias", icon = icon("volume-up"))
    })
    
    

    output$monthly_chats<-renderPlot({
        req(input$file)
        messages_by_month(getData(),title = "")
    })
    
    output$chats_per_user<-renderPlot({
        messages_by_user(getData(),show_members = 20,title = "")
    })
    
    output$top_emojis_id<-renderPlot({
        
        validate(
            need(!is.null(input$file),"Please upload a chat file in format .txt or .csv"),
            need(!is.null(input$members_list),'Check at least one members!'),
            need((input$n_members2show>1 & !is.null(input$n_members2show)), "# Top members must be greater than 1!"),
            need(is.integer(input$n_emojis),"# Top words to display must be integer!")
        )
        
        m=input$members_list[1:input$n_members2show]
        
        top_emojis(getData(),top=input$n_emojis,show_members=m, title="")
        
    })
    
    
    output$top_word_id<-renderPlot({
        
        validate(
            need(!is.null(input$file),"Please upload a chat file in format .txt or .csv"),
            need(!is.null(input$members_list),'Check at least one members!'),
            need((input$n_members2show>1 & !is.null(input$n_members2show)), '# Top members must be greater than 1!'),
            need((is.integer(input$n_words)), "# Top words to display must be integer!")
        )
        
        m=input$members_list[1:input$n_members2show]
        
        top_words(getData(),top=input$n_words,show_members = m,lang=input$lang, title="")
        
    })
    
    
    output$wcloud<-renderWordcloud2({
        
        # req(input$file)
        
        
        wcloud(getData(),top=100, lang=input$lang, rm_words=strsplit(input$wcInput, ","))
        
    })
    
    
    # output$wcloud2<-renderWordcloud2({
    #     
    #     req(input$file)
    #     wcloud(getData(),top=100, lang=input$lang, rm_words=c("c'est","c est","c est"))
    #     
    # })
    
    
    
    
    output$members_list_ui <- renderUI({
        
        
        members=getData() %>%count(author) %>% arrange(.data=.,desc(n))
        
        pickerInput(
            inputId = "members_list",
            label = "Select members to display:",
            choices = levels(members$author)[members$author], #levels(members$author),
            selected = levels(members$author),
            multiple = TRUE,
            options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "No one",
                `select-all-text` = "All",
                `none-selected-text` = "No one"
            )
        )
        
    })
    
    
    output$wcOut<-renderText({
        # input$wcInput %>% st
        
        strsplit(input$wcInput, ",") %>% paste()
    })
    

})






