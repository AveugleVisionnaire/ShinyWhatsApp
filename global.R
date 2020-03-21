library("rwhatsapp")
library("dplyr")
library("ggplot2")
library("tidytext")
library("ggimage")
library("tidyr")
library("lubridate")
library("stopwords")
library("wordcloud2")

theme_set(theme_minimal())
theme_update(plot.title = element_text(hjust = 0.5)) 


messages_by_month<-function(chat,title="Number of messages per month"){

  
  chat %>%
    mutate(m = floor_date(time, "month"), month = paste0(year(time), "-", lubridate::month(time,label =T))) %>%
    count(month,m) %>% 
    ggplot(aes(x = reorder(month,m),
               y = n,
               fill = month)) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(expand = (mult = c(0, 0, 0, 50))) +
    geom_text(aes(label = n), vjust = -0.1) +
    # scale_color_brewer(palette = "Accent")+
    # scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(14))+
    ylab("") +
    xlab("") +
    ggtitle(title) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
    # coord_flip()
}

# messages_by_month(chat)

messages_by_user<-function(chat,show_members=NULL, title="Number of messages per member"){
  
  members=chat %>%count(author) %>% arrange(.data=.,desc(n))
  
  if(is.numeric(show_members)){
    show_list=members$author[1:show_members]
  }else if(is.character(show_members)){
    show_list=show_members
  }else{
    show_list=members$author
  }
  
  chat %>%  
    filter(author %in% show_list) %>%
    count(author) %>%
    ggplot(aes(x = reorder(author, n),
               y = n,
               fill = author)) +
    geom_col(show.legend = FALSE) +
    # scale_y_continuous(expand = (mult = c(0, 0, 0, 20))) +
    geom_text(aes(label = scales::comma(n)), hjust = -0.1) +
    ylab("") +
    xlab("") +
    ggtitle(title) +
    coord_flip()
}

# messages_by_user(chat,show_members=20)



top_emojis<-function(chat, top=5, show_members=10,title="Most often used emojis"){
  
  library("ggimage")
  emoji_data <- rwhatsapp::emojis %>% # data built into package
    mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
    mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                              tolower(hex_runes1), ".png"))
  
  
  members=chat %>%count(author) %>% arrange(.data=.,desc(n))
  if(is.numeric(show_members)){
    show_list=members$author[1:show_members]
  }else{show_list=show_members}
  
  
  
  chat %>%
    filter(author %in% show_list) %>%
    unnest(emoji) %>%
    count(author, emoji, sort = TRUE) %>%
    arrange(author,desc(n)) %>% 
    setDT() %>%
    .[, head(.SD, top), by=.(author)] %>%
    left_join(emoji_data, by = "emoji") %>% 
    ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
    geom_col(show.legend = FALSE) +
    ylab("") +
    xlab("") +
    coord_flip() +
    geom_image(aes(y = n + 20, image = emoji_url)) +
    facet_wrap(~author, ncol = 2, scales = "free_y") +
    ggtitle(title) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
}


top_words <- function(chat, top=5, show_members=10, lang="fr", rm_words=NULL, title="Most often used words"){
  
  
  members=chat %>%count(author) %>% arrange(.data=.,desc(n))
  if(is.numeric(show_members)){
    show_list=members$author[1:show_members]
  }else{show_list=show_members}
  
  whatsapp_infos_tags=c("media",  "omitted",  "ref", "https")
  to_remove <- c(get_stopwords(language = lang,"stopwords-iso")$word, rm_words, whatsapp_infos_tags)
  
  
  
  chat %>%
    filter(author %in% show_list) %>%
    unnest_tokens(input = text,
                  output = word) %>%
    setDT %>%
    .[!(word %in% to_remove ),.N,.(author, word)] %>%
    setorder(.,author, -N, word) %>% 
    .[, head(.SD, top), by=.(author)] %>%
    ggplot(aes(x = reorder_within(word, N, author), y = N, fill = author)) +
    geom_col(show.legend = FALSE) +
    ylab("") +
    xlab("") +
    coord_flip() +
    facet_wrap(~author, ncol = 2, scales = "free_y") +
    scale_x_reordered() +
    ggtitle(title)
  
  
  
}

# top_words(chat,show_members = members$author[1:3])


wcloud<-function(chat,top=100, lang="fr", rm_words=NULL){
  
  whatsapp_infos_tags=c("media",  "omitted",  "ref", "https")
  # to_remove <- c(get_stopwords(language = lang,"stopwords-iso")$word, rm_words, whatsapp_infos_tags)
  to_remove <- c(stopwords(language = lang), rm_words, whatsapp_infos_tags)
  
  chat %>%
    unnest_tokens(input = text, output = word) %>%
    setDT %>% 
    .[!(word %in% to_remove),.N,by=word]%>% 
    setorder(., -N, word) %>% 
    # filter(!(word %in% to_remove )) %>%
    # count(word, sort=TRUE) %>% 
    # top_n(top) %>% 
    wordcloud2(data=., size=1.6, color='random-dark',shape = 'heart')
}




notificationItemPlus<- function(text, icon = shiny::icon("warning"), status = "success", href = NULL, ...) {
  if (is.null(href)) 
    href <- "#"
  icon <- tagAppendAttributes(icon, class = paste0("text-", 
                                                   status))
  tags$li(a(href = href, icon, text, ...))
}