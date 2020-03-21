library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
# source("D:/Documents/Code R/Chat_Analysis/Utils.R")

#  ------------------------------------------- Header  -------------------------------------------

header <- dashboardHeader(
  title = "CHATBOARD", #HTML("<font size=6><strong>CHATBOARD</strong></font>"),
  
  dropdownMenu( headerText = "Would you like to contact the author?",
                type = "notifications", 
                icon = icon("envelope"), 
                badgeStatus = "primary",
                notificationItemPlus(
                  text = "seypro223@gmail.com",
                  icon("google-plus"),
                  href="mailto:seypro223@gmail.com",
                  target = "_blank"
                ),
                notificationItemPlus(
                  text = "Seydou KEITA",
                  icon("linkedin"),
                  href = "https://www.linkedin.com/in/k-seydou/",
                  target = "_blank"
                ),
                notificationItemPlus(
                  text = "Source Code",
                  icon("github"),
                  href="https://github.com/AveugleVisionnaire/ShinyWhatsApp",
                  target = "_blank"
                
                )
                ),
  
  
  
  dropdownMenu( headerText = "Love it? Share it!",
                type = "notifications", 
                icon = icon("share-alt"), 
                badgeStatus = "primary",
                notificationItemPlus(
                  text = "WharsApp",
                  icon("whatsapp"),
                  href="https://wa.me/?text=An%20App%20for%20WhatsApp%20chat%20analysis%3A%20https%3A%2F%2Fskeita.shinyapps.io%2FShinyWhatsApp%2F",
                  target = "_blank"
                ),
                notificationItemPlus(
                 text = "Twitter",
                 icon("twitter"),
                 href = "https://twitter.com/intent/tweet?url=https://skeita.shinyapps.io/ShinyWhatsApp/",
                 target = "_blank"
               ),
               notificationItemPlus(
                 text = "Facebook",
                 icon("facebook"),
                 href = "https://www.facebook.com/sharer/sharer.php?u=https://skeita.shinyapps.io/ShinyWhatsApp/",
                 target = "_blank"
               )
            
               
  )
  
)

#  ------------------------------------------- Sidebar  -------------------------------------------

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Global View", tabName = "global", icon = icon("globe")),
    menuItem("Funny Stats", icon = icon("chart-bar"), tabName = "stats"),
    menuItem("Help", icon = icon("question-circle"), tabName = "help",selected = T)
  ),
  
  shiny::hr(),
  
  fileInput("file", 'Choose chat File in format ".txt"',
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
  ),
  
  uiOutput("members_list_ui")
  ,
  
  
  numericInput("n_members2show", "# Top members to display:", value=5, min=2, max=20),
  
  pickerInput(
    inputId = "lang",
    label = "Chat language:",
    choices = list("French" = "fr", "English" ="en", "Arabic" = "ar"),
    selected = "fr",
    multiple = F,
  )
  
  
  
)

#  ------------------------------------------- Body  -------------------------------------------
body <- dashboardBody(
  
  tags$head(tags$style(HTML("
    .btn-group{
  background-color: red;
    }
        
    .main-header .logo {
    font-size: 24px;
    font-weight: bold;
  }
        .row{
  align: center;
        }
        
        
        div.box-header {text-align: center;}"))),
  
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  
  tabItems(
    
    #  ------------------------------- Item global-------------------------------
    
    tabItem(tabName = "global",
            
            
            
            
            fluidRow(
              # column(4,valueBoxOutput("usersBox")),
              # column(4,infoBoxOutput("messagesBox"))
              
              valueBoxOutput("usersBox",width = 6),
              valueBoxOutput("messagesBox",width = 6)
              # valueBoxOutput("includeMediasBox")
            ),
            
            
            boxPlus(
              title = "Wordcloud",
              closable = F,
              width = NULL,
              solidHeader = T,
              collapsible = T,
              enable_dropdown = TRUE,
              dropdown_icon = "gear",
              dropdown_menu = dropdownItemList(status = "danger",
                                               textInput("wcInput", "Word to remove", value = "", placeholder = "word1, word2"),
              ),
              wordcloud2Output("wcloud")%>% withSpinner(color="#0000FF")
            )
            
    ),
    
    
    #  ------------------------------- Item Stats-------------------------------
    
    
    tabItem(tabName = "stats",
            fluidRow(
              column(6,
                     #Number of messages per month
                     # plotOutput("monthly_chats")%>% withSpinner(color="#0000FF")
                     
                     boxPlus(title = "Number of messages per month", closable = F, width = NULL, solidHeader = T, collapsible = T,
                             plotOutput("monthly_chats")%>% withSpinner(color="#0000FF")
                     )
                     
              ),
              column(6,
                     #Number of message per user
                     # plotOutput("chats_per_user")%>% withSpinner(color="#0000FF")
                     
                     boxPlus(title = "Number of messages per user", closable = F, width = NULL, solidHeader = T, collapsible = T,
                             plotOutput("chats_per_user")%>% withSpinner(color="#0000FF")
                     )
                     
                     
              )
            ),
            
            fluidRow(
              column(12,
                     boxPlus(
                       title = "Most often used words",closable = F, width = NULL, solidHeader = T, collapsible = T,  enable_dropdown = TRUE, dropdown_icon = "gear",
                       dropdown_menu = dropdownItemList(status = "danger",
                                                        numericInput("n_words", "# Top words to display", value = 3, min = 1, max=5)
                       ),
                       plotOutput("top_word_id") %>% withSpinner(color="#0000FF")
                     )
              )
              
            ),
            
            
            fluidRow(
              column(12,
                     
                     boxPlus(title = "Most often used emojis", closable = F, width = NULL, solidHeader = T, collapsible = T, enable_dropdown = TRUE,
                             dropdown_icon = "gear",
                             dropdown_menu = dropdownItemList(status = "danger",
                                                              numericInput("n_emojis", "# Top emojis to display", value = 3, min = 1, max=5)
                             ),
                             plotOutput("top_emojis_id")%>% withSpinner(color="#0000FF")
                     )
                     
              )
            )
    ),
    
    
    #  ------------------------------- Item Help-------------------------------
    
    tabItem(tabName = "help",
            
            fluidRow(
              column(12,
              HTML("<font size='6'>
                      This App is intended to perform a quick analysis of your WhatsApp chats as easy as possible: reading your chat history and summarizing keys information.
                      <br>
                      For that, follow theses steps:
                   </font>"),
              br(),
             
              HTML("<font size='6'>
                        <ol>
                          <li>Export the chat you desire to analyse from WhatsApp (<i>See screenshots below <i class=\"fa fa-hand-point-down\"></i></i>)</li>
                          <li>Upload the chat file into App using <b><strong>Browse...</strong></b> button in left side bar menus</li>
                          <li>Enjoy App by switching between menus in sidebar.</li>
                        </ol>
                   </font>")
              
              )
            ),
            
            fluidRow(
              column(3,
                     
                     boxPlus(title =HTML("<b>Step 1</b>"), closable = F, width = NULL, solidHeader = T, collapsible = F, enable_dropdown = F,
                             img(src='export1.jfif', align = "right", height="1000%", width="100%")
                             
                     )
              ),
              
              column(3,
                     
                     boxPlus(title = HTML("<b>Step 2</b>"), closable = F, width = NULL, solidHeader = T, collapsible = F, enable_dropdown = F,
                             img(src='export2.jfif', align = "right",height="100%", width="100%")
                             
                     )
              ),
              
              column(3,
                     
                     boxPlus(title = HTML("<b>Step 3</b>"), closable = F, width = NULL, solidHeader = T, collapsible = F, enable_dropdown = F,
                             img(src='export3.jfif', align = "right",height="100%", width="100%")
                             
                     )
                     
              ),
              
              
              column(3,
                     
                     boxPlus(title = HTML("<b>Step 4</b>"), closable = F, width = NULL, solidHeader = T, collapsible = F, enable_dropdown = F,
                             img(src='browse.png', align = "right",height="100%", width="100%")
                             
                     )
                     
              )
              
              
              
            ),
            
            fluidRow(
              
            column(12,
                   
                   boxPlus(title = HTML("<b>Demo</b>"), closable = F, width = NULL, solidHeader = T, collapsible = F, enable_dropdown = F,
                           tags$iframe(style="height:600px; width:100%", src="demo.pdf")
                   )
                   
                   )
            )
    )
    
    
    
    
  )
  
  
)



#  ------------------------------------------- dashboardPage call  -------------------------------------------
dashboardPage(header, sidebar, body,skin="green")
