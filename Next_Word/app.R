library(shiny)
library(data.table)
library(stringr)
library(quanteda)
library(readr)

tsf_ng6 <- fread("./data/ng6_processed.train.csv"); setattr(tsf_ng6,"sorted",c("ngram","dis.frequency"))
tsf_ng5 <- fread("./data/ng5_processed.train.csv"); setattr(tsf_ng5,"sorted",c("ngram","dis.frequency"))
tsf_ng4 <- fread("./data/ng4_processed.train.csv"); setattr(tsf_ng4,"sorted",c("ngram","dis.frequency"))
tsf_ng3 <- fread("./data/ng3_processed.train.csv"); setattr(tsf_ng3,"sorted",c("ngram","dis.frequency"))
tsf_ng2 <- fread("./data/ng2_processed.train.csv"); setattr(tsf_ng2,"sorted",c("ngram","dis.frequency"))
tsf_ng1 <- fread("./data/ng1_processed.train.csv"); setattr(tsf_ng1,"sorted",c("ngram","dis.frequency"))

tsf.list <- list(tsf_ng1,tsf_ng2,tsf_ng3,tsf_ng4,tsf_ng5,tsf_ng6)
rm(tsf_ng1,tsf_ng2,tsf_ng3,tsf_ng4,tsf_ng5,tsf_ng6)

make_ngram <- function(input,order){
    
    if(length(input)> order) input <- input[(length(input)-order+1):length(input)]
    paste0(input,collapse = "_")
}

find_output <- function(tsf, in_ngram = NULL, leave_out = NULL, return_all = F){
    
    tsf_temp <- if(is.null(in_ngram)){
        tsf[!(output %in% leave_out)][order(dis.frequency)]
    }else{ 
        tsf[.(in_ngram)][!(output %in% leave_out)]
    }
    
    tsf_temp2 <- if(is.null(in_ngram)){
        tsf[(output %in% leave_out)]
    }else{
        tsf[.(in_ngram)][(output %in% leave_out)]
    }
    
    get_rows <- 
        if(!return_all){
            if(nrow(tsf_temp)>=2){
                seq(nrow(tsf_temp)-2, nrow(tsf_temp))
            }else if (nrow(tsf_temp) != 0){
                seq(1, nrow(tsf_temp))
            }else{
                0
            }
        }else{
            seq(1, nrow(tsf_temp))
        }
    
    output <- tsf_temp[get_rows,.(output,dis.frequency)]
    if(nrow(output)==0) output <- list(output = NA, dis.frequency = NA)
    leave_out_freqs <- tsf_temp2[,.(total_dis.freq = sum(dis.frequency), total_freq = sum(frequency))]
    total_freqs <- tsf_temp[,.(total_dis.freq = sum(dis.frequency), total_freq = sum(frequency))]
    leave_out <- if(!all(is.na(output$output))) c(leave_out, tsf_temp[,output]) else leave_out
    list(output = output$output, 
         dis.freq = output$dis.frequency, 
         leave_out_dis.freq = leave_out_freqs$total_dis.freq,
         total_dis.freq = total_freqs$total_dis.freq,
         total_freq = total_freqs$total_freq+leave_out_freqs$total_freq,
         leave_out = leave_out)
}

next_word <- function(input, tsf.list, max_order){
    
    input <- str_remove_all(input,"[^A-Za-z\\- ']")
    input <- unlist(strsplit(input," "))
    #input <- input[!(tolower(input) %in% stopwords('en'))]
    order <- length(input)
    if(order > max_order) order <- max_order
    
    out.df <- data.frame(output = character(), prob = numeric(),
                         alpha = numeric(), prob_leave_out = numeric())
    alpha <- 1
    leave_out <- NULL
    for(i in 1:(order+1)){
        if(i <= order){
            ngram <- make_ngram(input, order-(i-1))
            output <- find_output(tsf.list[[order-(i-2)]],ngram, leave_out)
            if(all(is.na(output$output))) next
        }else{
            output <- find_output(tsf.list[[1]], leave_out = leave_out)
        }
        prob <- output$dis.freq/output$total_freq
        prob_leave_out <- 1 - output$leave_out_dis.freq/output$total_freq
        alpha <- (1-(output$total_dis.freq+output$leave_out_dis.freq)/output$total_freq)
        out.df <- rbind(out.df,data.frame(output = output$output,prob = prob,
                                          alpha = alpha, prob_leave_out = prob_leave_out,
                                          phase = order - (i-2),
                                          stringsAsFactors = F))
        leave_out <- output$leave_out
        
    }
    prob_r <- NULL
    out.df <- data.table(out.df)
    alpha <- out.df[,.(alpha = max(alpha)),by="phase"]
    prob_leave_out <- out.df[,.(prob_leave_out = max(prob_leave_out)), by="phase"]
    for(i in 1:nrow(out.df)){
        phase_r = out.df[i,phase]
        alpha_r <- prod(alpha[phase > phase_r,alpha])
        prob_leave_out_r <- prod(prob_leave_out[phase >= phase_r, prob_leave_out])
        prob_r <- c(prob_r,out.df[i,prob]*alpha_r/prob_leave_out_r)
    }
    
    
    out.df <- cbind(out.df, data.frame(prob_real = prob_r))
    out.df[order(-prob_real), output][1:3]
}

ui <- fluidPage(
    
    fluidRow(
        h3("Next word predictor"),
        p("This is a next word predictor"),
        p(paste0("Input partial sentences and the app will try to predict the next word based on",
          " the previous inputs. It outputs the 3 most probable words, based on a set of texts from",
          " Blogs, News and Twitter.")),
        p("The algorithm accounts for the previous N words input, which you can choose."),
        p(paste0("For example, choosing '4 words', the algorithm will consider the most probable next words",
        " for the serial combination of the 4 previous words. That way, providing 'I will predict the",
        " future' as input will make the algorithm only consider 'will predict the future'")),
        p("It only works for the english language."),
        column(12,
               textInput("sentence", "Input partial sentence")
        ),
        column(12,
               selectInput("ngram", "Select ngram",
                           choices = list("5 last words"=5, "4 last words"=4,"3 last words"=3,
                                          "2 last words"=2,"1 last word"=1)))
    ),
    fluidRow(
        column(12,
               verbatimTextOutput("w1"))#,
        #column(4,
        #       verbatimTextOutput("w2")),
        #column(4,
        #       verbatimTextOutput("w3"))
        )
)

server <- function(input,output){
    
    run <- reactiveValues()
    
    observeEvent(input$sentence,{
        run$r1 <- runif(1,0,10000)
    })
    observeEvent(input$ngram,{
        run$r1 <- runif(1,0,10000)
    })
    
    observeEvent(run$r1,{
        inputs <- input$sentence
        order <- as.numeric(input$ngram)
        
        #browser()
        words <- if(inputs != "") next_word(inputs,tsf.list, order) else c("","","")
    
        output$w1 <- renderText({words[1]})
        output$w2 <- renderText({words[2]})
        output$w3 <- renderText({words[3]})
    })
}

shinyApp(ui = ui, server = server)
