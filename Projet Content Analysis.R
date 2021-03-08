### 1. EXTRACTION AND CLEANING PART ###
install.packages('htmltab')
install.packages('rvest')
install.packages('xml2')
install.packages("stringr")
install.packages('dplyr')
install.packages('tidytext')
library(dplyr)
library(tidytext)
library(rvest)
library(htmltab)
library(xml2)
library(stringr)

### 2. CHOOSE YOUR URL ###
URL = 'https://www.boutique-box-internet.fr/demenagement/resiliation-box/'

### 3. META ANALYSIS ###
#PART 3.0 : PAGE LENGTH
{HTML <- read_html(URL)
  BRUT_CODE <- as.character(HTML)
  LENGTH_CODE <- nchar(BRUT_CODE)
  LENGTH_CODE
}

#PART 3.1 : META ANALYSIS
{
  head_Location_start <- str_locate(BRUT_CODE,'<head>')
  head_Location_start <- as.data.frame(head_Location_start)
  head_Location_end <- str_locate(BRUT_CODE,'</head>')
  head_Location_end <- as.data.frame(head_Location_end)
  HEAD_CONTENT <- substring(BRUT_CODE,head_Location_start[,1],head_Location_end[,1])
  LENGTH_HEAD <- nchar(HEAD_CONTENT)
  
  meta_description_Location_start <- str_locate(HEAD_CONTENT,'<meta name="description" content="')
  Tampon <- substring(HEAD_CONTENT,meta_description_Location_start[,2],LENGTH_HEAD)
  meta_description_Location_end <- str_locate(Tampon,'>')
  meta_description_Location_end <- as.data.frame(meta_description_Location_end)
  M.DESCRIPTION_CONTENT <- substring(Tampon,1,meta_description_Location_end[,1])
  M.DESCRIPTION_CONTENT <- gsub("\"","",M.DESCRIPTION_CONTENT)
  M.DESCRIPTION_CONTENT <- gsub('>',"",M.DESCRIPTION_CONTENT)
  M.DESCRIPTION_CONTENT <- gsub('<',"",M.DESCRIPTION_CONTENT)
  M.DESCRIPTION_CONTENT
  
  title_Location_start <- str_locate(HEAD_CONTENT,'<title>')
  title_Location_start <- as.data.frame(title_Location_start)
  title_Location_end <- str_locate(HEAD_CONTENT,'</title>')
  title_Location_end <- as.data.frame(title_Location_end)
  M.TITLE_CONTENT <- substring(HEAD_CONTENT,title_Location_start[,2],title_Location_end[,1])
  M.TITLE_CONTENT <- gsub("\"","",M.TITLE_CONTENT)
  M.TITLE_CONTENT <- gsub('>',"",M.TITLE_CONTENT)
  M.TITLE_CONTENT <- gsub('<',"",M.TITLE_CONTENT)
  M.TITLE_CONTENT
  
  META <- paste(M.DESCRIPTION_CONTENT,M.TITLE_CONTENT)
} 

#PART 3.2 : HEAD ANALYSIS
{
  body_Location_start <- str_locate(BRUT_CODE,'<body')
  body_Location_start <- as.data.frame(body_Location_start)
  body_Location_end <- str_locate(BRUT_CODE,'</body>')
  body_Location_end <- as.data.frame(body_Location_end)
  BODY_CONTENT <- substring(BRUT_CODE,body_Location_start[,1],body_Location_end[,1])
  
  count_h1 <- str_count(BODY_CONTENT,"<h1")
  count_h1
  count_h2 <- str_count(BODY_CONTENT,"<h2")
  count_h2
  count_h3 <- str_count(BODY_CONTENT,"<h3")
  count_h3
  count_h4 <- str_count(BODY_CONTENT,"<h4")
  count_h4
  count_h5 <- str_count(BODY_CONTENT,"<h5")
  count_h5
  count_h6 <- str_count(BODY_CONTENT,"<h6")
  count_h6
  
  h1_Location_start <- str_locate(BODY_CONTENT,'<h1')
  h1_Location_start <- as.data.frame(h1_Location_start)
  h1_Location_end <- str_locate(BODY_CONTENT,'</h1>')
  h1_Location_end <- as.data.frame(h1_Location_end)
  H1_CONTENT <- substring(BODY_CONTENT,h1_Location_start[,2],h1_Location_end[,1])
  H1_CONTENT
  
  h1_Location_start <- str_locate(H1_CONTENT,'>')
  h1_Location_start <- as.data.frame(h1_Location_start)
  h1_Location_end <- str_locate(H1_CONTENT,'<')
  h1_Location_end <- as.data.frame(h1_Location_end)
  H1_CONTENT_CLEAN <- substring(H1_CONTENT,h1_Location_start[,2],h1_Location_end[,1])
  H1_CONTENT_CLEAN <- gsub('<','',H1_CONTENT_CLEAN)
  H1_CONTENT_CLEAN <- gsub('>','',H1_CONTENT_CLEAN)
  H1_CONTENT_CLEAN
  
  #Body Introduction (Between <body> and <h1>)
  Body_Introduction <- substring(BODY_CONTENT,1,h1_Location_end[,2])
  LENGTH_Body_Introduction <- nchar(Body_Introduction)
  Body_Introduction
  
  #First H2 - Part Title
  h2_Location_start <- str_locate(BODY_CONTENT,'<h2')
  h2_Location_start <- as.data.frame(h2_Location_start)
  h2_Location_end <- str_locate(BODY_CONTENT,'</h2>')
  h2_Location_end <- as.data.frame(h2_Location_end)
  H2_CONTENT <- substring(BODY_CONTENT,h2_Location_start[,1],h2_Location_end[,1])
  H2_CONTENT
  
}

  
### 4. INTRODUCTION ARTICLE ###
{ 
  article_introduction <- substring(BODY_CONTENT,h1_Location_end[,2],h2_Location_start[,1])
  article_introduction <- gsub('<path','',article_introduction)
  LENGTH_article_introduction <- nchar(article_introduction)
  LENGTH_article_introduction
  
  ARTICLE_INTRODUCTION_DF <- as.data.frame("")
  
  count_srtp_introduction <- str_count(article_introduction,"<p")
  count_endp_introduction <- str_count(article_introduction,"</p>")
  
  
  if (count_srtp_introduction == count_endp_introduction) {
    print(1)
    counter = count_endp_introduction
    print(paste('Nombre de paragraphe :',counter))
  } else {
    print(0)
    print('différence de nb <p> et </p>')
  }
  
  for (i in 1:counter){
    #DETECT AND EXTRACT <P> tag
    p_Location_start <- str_locate(article_introduction,'<p')
    p_Location_start <- as.data.frame(p_Location_start)
    p_Location_end <- str_locate(article_introduction,'</p>')
    p_Location_end <- as.data.frame(p_Location_end)
    Introduction_P <- substring(article_introduction,p_Location_start[,1],p_Location_end[,2])
    ARTICLE_INTRODUCTION_DF <- rbind(ARTICLE_INTRODUCTION_DF,Introduction_P)
    
    #MAKE DISTINCTION BETWEEN Class AND Text INTO EACH <p>
    class_text_Location_start <- str_locate(ARTICLE_INTRODUCTION_DF[i,1],'<p')
    class_text_Location_start <- as.data.frame(class_text_Location_start)
    class_text_Location_inter <- str_locate(ARTICLE_INTRODUCTION_DF[i,1],'>')
    class_text_Location_inter <- as.data.frame(class_text_Location_inter)
    class_text_Location_end <- str_locate(ARTICLE_INTRODUCTION_DF[i,1],'</p>')
    class_text_Location_end <- as.data.frame(class_text_Location_end)
    ARTICLE_INTRODUCTION_DF[i,'Class_p'] <- substring(ARTICLE_INTRODUCTION_DF[i,1],class_text_Location_start[,1],class_text_Location_inter[,2])
    ARTICLE_INTRODUCTION_DF[i,'Text_p'] <- substring(ARTICLE_INTRODUCTION_DF[i,1],class_text_Location_inter[,2],class_text_Location_end[,2])
    
    
    #BOUCLE IF (SPAN)
    SPAN_CONTENT <- ARTICLE_INTRODUCTION_DF[i,'Text_p']
    
    #IF NA
    if (is.na(SPAN_CONTENT)) {
      SPAN_CONTENT <- 'NA'
    } else {
      SPAN_CONTENT <- SPAN_CONTENT
    }
    
    if (str_detect(SPAN_CONTENT,'<span')) {
      print(paste("Ligne",i,"'Span' Présent donc boucle for #2"))
      count_span_introduction <- str_count(ARTICLE_INTRODUCTION_DF[i,'Text_p'],"<span")
      print(paste('il y a',count_span_introduction,'span'))
      
      for (j in 1:count_span_introduction){
        print(j)
        print(i)
        
        span_col_name <- paste('span',as.character(j))
        print(span_col_name)
        
        span_Location_start <- str_locate(SPAN_CONTENT,'<span')
        span_Location_start <- as.data.frame(span_Location_start)
        span_Location_start
        
        span_Location_end <- str_locate(SPAN_CONTENT,'</span>')
        span_Location_end <- as.data.frame(span_Location_end)
        span_Location_end
        
        ARTICLE_INTRODUCTION_DF[i,span_col_name] <- substring(SPAN_CONTENT,span_Location_start[,1],span_Location_end[,2])
        print(ARTICLE_INTRODUCTION_DF[i,span_col_name])
        
        LENGTH_SPAN_CONTENT <- nchar(SPAN_CONTENT)
        print(SPAN_CONTENT)
        
        SPAN_CONTENT <- substring(SPAN_CONTENT,span_Location_end[,2],LENGTH_SPAN_CONTENT)
        LENGTH_SPAN_CONTENT <- nchar(SPAN_CONTENT)
        
        
        j = j+1
      }
      
    } else {
      print("Fin de Boucle Normal")
    }
    
    
    
    #RESIZE SAMPLE TO NEXT LAP
    article_introduction <- substring(article_introduction,p_Location_end[,2],LENGTH_article_introduction)
    LENGTH_article_introduction <- nchar(article_introduction)
    
    i = i+1
  }
  
  View(ARTICLE_INTRODUCTION_DF)
 
}

### LENGTH ARTICLE ###
{
  Article_content_start <- str_locate(BODY_CONTENT,'<h1')
  Article_content_start <- as.data.frame(Article_content_start)
  Article_content_start
  
  LENGTH_BODY_CONTENT <- nchar(BODY_CONTENT)
  LENGTH_BODY_CONTENT
  
  ARTICLE_CONTENT <- substring(BODY_CONTENT,Article_content_start[,1],LENGTH_BODY_CONTENT)
  #ARTICLE_CONTENT
  
  LENGTH_ARTICLE_CONTENT <- nchar(ARTICLE_CONTENT)
  LENGTH_ARTICLE_CONTENT
}

### CONTENT ARTICLE EXTRACTION ###
{
  STRUCTURE_DF <- as.data.frame("")
  CONTENT_BUFFER <- ARTICLE_CONTENT
  
  for (z in 1:2){
    z=1
    for (k in 1:count_h2){
      
      if (k == count_h2) {
        print('Last Lap')
        
      } else {
        print('Not Last Lap')
        
        #STEP 1 - FIRST H2 LOCATION
        h2_n0_Location_start <- str_locate(CONTENT_BUFFER,'<h2')
        h2_n0_Location_start <- as.data.frame(h2_n0_Location_start)
        h2_n0_Location_start
        
        h2_n0_Location_end <- str_locate(CONTENT_BUFFER,'</h2>')
        h2_n0_Location_end <- as.data.frame(h2_n0_Location_end)
        h2_n0_Location_end
        
        Buffer_n0 <- substring(CONTENT_BUFFER,h2_n0_Location_start[,1],h2_n0_Location_end[,2])
        print(Buffer_n0)
        
        h2_Buffer_Location_start <- str_locate(Buffer_n0,'>')
        h2_Buffer_Location_start <- as.data.frame(h2_Buffer_Location_start)
        h2_Buffer_Location_start
        
        h2_Buffer_Location_end <- str_locate(Buffer_n0,'</h2>')
        h2_Buffer_Location_end <- as.data.frame(h2_Buffer_Location_end)
        h2_Buffer_Location_end
        
        Buffer_n0 <- substring(Buffer_n0,h2_Buffer_Location_start[,2],h2_Buffer_Location_end[,1])
        Buffer_n0 <- as.character(Buffer_n0)
        print(Buffer_n0)
        
        Buffer_n0 <- gsub("<","",Buffer_n0)
        Buffer_n0 <- gsub(">","",Buffer_n0)
        print(Buffer_n0)
        
        H2_n0 <- str_locate(ARTICLE_CONTENT,Buffer_n0)
        H2_n0 <- as.data.frame(H2_n0)
        H2_n0
        

        #STEP 2 - RESIZE SAMPLE TO BYPASS FIRST H2 LOCATION#

        
        LENGTH_CONTENT_BUFFER <- nchar(CONTENT_BUFFER)
        LENGTH_CONTENT_BUFFER
        
        CONTENT_BUFFER <- substring(CONTENT_BUFFER,h2_n0_Location_end[,2],LENGTH_ARTICLE_CONTENT)
        LENGTH_CONTENT_BUFFER <- nchar(CONTENT_BUFFER)
        LENGTH_CONTENT_BUFFER
        

        #STEP 3 - SECOND H2 LOCATION#

        
        h2_n1_Location_start <- str_locate(CONTENT_BUFFER,'<h2')
        h2_n1_Location_start <- as.data.frame(h2_n1_Location_start)
        h2_n1_Location_start
        
        h2_n1_Location_end <- str_locate(CONTENT_BUFFER,'</h2>')
        h2_n1_Location_end <- as.data.frame(h2_n1_Location_end)
        h2_n1_Location_end
        
        Buffer_n1 <- substring(CONTENT_BUFFER,h2_n1_Location_start[,1],h2_n1_Location_end[,2])
        print(Buffer_n1)
        
        h2_Buffer_n1_Location_start <- str_locate(Buffer_n1,'>')
        h2_Buffer_n1_Location_start <- as.data.frame(h2_Buffer_n1_Location_start)
        h2_Buffer_n1_Location_start
        
        h2_Buffer_n1_Location_end <- str_locate(Buffer_n1,'</h2>')
        h2_Buffer_n1_Location_end <- as.data.frame(h2_Buffer_n1_Location_end)
        h2_Buffer_n1_Location_end
        
        Buffer_n1 <- substring(Buffer_n1,h2_Buffer_n1_Location_start[,2],h2_Buffer_n1_Location_end[,1])
        Buffer_n1 <- as.character(Buffer_n1)
        print(Buffer_n1)
        
        Buffer_n1 <- gsub("<","",Buffer_n1)
        Buffer_n1 <- gsub(">","",Buffer_n1)
        print(Buffer_n1)
        
        H2_n1 <- str_locate(ARTICLE_CONTENT,Buffer_n1)
        H2_n1 <- as.data.frame(H2_n1)
        H2_n1
        
        

        #STEP 4 - EXTRACT THE WHOLE PART BETWEEN FRIST H2 AND SECOND H2#
        
        Part <- substring(ARTICLE_CONTENT,H2_n0[,1],H2_n1[,1])
        print(Part)
        
        Length_Part <- nchar(Part)
        STRUCTURE_DF[k,'Content_Part_Length'] <- Length_Part
        
        h2_Part_Location_end <- str_locate(Part,'<h2')
        h2_Part_Location_end <- as.data.frame(h2_Part_Location_end)
        h2_Part_Location_end
        
        Part <- substring(Part,1,h2_Part_Location_end[,1])
        Length_Part <- nchar(Part)
        
        h2_Part_Location_end <- str_locate(Part,'</h2>')
        h2_Part_Location_end <- as.data.frame(h2_Part_Location_end)
        h2_Part_Location_end
        
        Title_Part <- substring(Part,1,h2_Part_Location_end[,2])
        
        Part <- substring(Part,h2_Part_Location_end[,2],Length_Part)
        
        Length_Part <- nchar(Part)
        
        STRUCTURE_DF[k,'Part number'] <- paste('Part Number',k)
        STRUCTURE_DF[k,'Part_content'] <- Part
        STRUCTURE_DF[k,'Title_Part'] <- Title_Part
        
        Part_buffer <- Part
        
        if (str_detect(Part_buffer,'<h3')){
          count_h3 <- str_count(Part_buffer,"<h3")
          
          print(paste('There are',count_h3,'<h3> in',STRUCTURE_DF[k,'Part number']))
          STRUCTURE_DF[k,'Subtitle'] <- count_h3
          
          p_searcher <- str_locate(Part_buffer,'<p')
          h3_searcher <- str_locate(Part_buffer,'<h3')
          
          if (p_searcher[,1]<h3_searcher[,1]){
            print('<p>')
            
            STRUCTURE_DF[k,'Part_Start_by'] <- 'p'
            
            count_p <- str_count(Part_buffer,"<p")
            STRUCTURE_DF[k,'count_p'] <- count_p
          } else {
            print('<h3>')
            
            STRUCTURE_DF[k,'Part_Start_by'] <- 'h3'
            
            count_p <- str_count(Part_buffer,"<p")
            STRUCTURE_DF[k,'count_p'] <- count_p
          }
          
        } else {
          print(paste("There is no <h3> in",STRUCTURE_DF[k,'Part number']))
          
          STRUCTURE_DF[k,'Subtitle'] <- 0
          STRUCTURE_DF[k,'Part_Start_by'] <- 'p'
          
          count_p <- str_count(Part_buffer,"<p")
          STRUCTURE_DF[k,'count_p'] <- count_p
        }
      }
    }
    z=2
    CONTENT_BUFFER <- ARTICLE_CONTENT
    print('Content Buffer Reset')
    
  }
}
  
View(STRUCTURE_DF) 
STRUCTURE_DF <- STRUCTURE_DF[2:8]

### CLEANING CONTENT ARTICLE ###

#PART 1 : Cleaner before <p> into parts (<h2>)
{
  for (k in 1:count_h2){
    Content_buffer <- STRUCTURE_DF[k,'Part_content']
    LENGTH_Content_buffer <- nchar(Content_buffer)
    LENGTH_Content_buffer
    
    STRUCTURE_DF[k,'Part_content'] <- substring(Content_buffer,2,LENGTH_Content_buffer)
  }
}
  
#PART 2 : CONCATENATE PARAGRAPHS AND EXTRACT SUBTITLES 
{
  STRUCTURE_DF[k,'Paragraph'] <- ""
  STRUCTURE_DF[k,'subtitle_content'] <- ""
  for (k in 1:count_h2){
    print(paste('Loop on part nb',k))
    Content_buffer <- STRUCTURE_DF[k,'Part_content']
    count_p <- STRUCTURE_DF[k,'count_p']
    count_p
    for (l in 1:count_p){
      
      if ( str_detect(Content_buffer,'<h3')) {
        print('There are subtitles')
        {p_searcher <- str_locate(Content_buffer,'<p')
          h3_searcher <- str_locate(Content_buffer,'<h3')
          
          if (p_searcher[,1] > h3_searcher[,1]) {
            print('subtitle before paragraph - so we need to extract it before')
            #h3 extraction
            h3_Location_start <- str_locate(Content_buffer,'<h3')
            h3_Location_start <- as.data.frame(h3_Location_start)
            h3_Location_start
            
            h3_Location_inter <- str_locate(Content_buffer,'>')
            h3_Location_inter <- as.data.frame(h3_Location_inter)
            h3_Location_inter
            
            if (h3_Location_inter[,1] < h3_Location_start[,1]) {
              print(1)
              Content_buffer <- substring(Content_buffer,h3_Location_start[,1],LENGTH_Content_buffer)
              
              h3_Location_start <- str_locate(Content_buffer,'<h3')
              h3_Location_start <- as.data.frame(h3_Location_start)
              h3_Location_start
              
              h3_Location_inter <- str_locate(Content_buffer,'>')
              h3_Location_inter <- as.data.frame(h3_Location_inter)
              h3_Location_inter
              
            } else {
              print(0)
            }
            
            h3_Location_end <- str_locate(Content_buffer,'</h3>')
            h3_Location_end <- as.data.frame(h3_Location_end)
            h3_Location_end
            
            h3 <- substring(Content_buffer,h3_Location_inter[,1],h3_Location_end[,1])
            h3
            
            h3 <- gsub(">","",h3)
            h3 <- gsub("<","",h3)
            h3
            
            STRUCTURE_DF[k,'subtitle_content'] <- paste(STRUCTURE_DF[k,'subtitle_content'],h3)
            
            LENGTH_Content_buffer <- nchar(Content_buffer)
            LENGTH_Content_buffer
            Content_buffer <- substring(Content_buffer,h3_Location_end[,2],LENGTH_Content_buffer)
            
            #paragraph extraction
            {
              p_Location_start <- str_locate(Content_buffer,'<p')
              p_Location_start <- as.data.frame(p_Location_start)
              p_Location_start
              
              p_Location_inter <- str_locate(Content_buffer,'>')
              p_Location_inter <- as.data.frame(p_Location_inter)
              p_Location_inter
              
              if (p_Location_inter[,1] < p_Location_start[,1]) {
                print(1)
                Content_buffer <- substring(Content_buffer,p_Location_start[,1],LENGTH_Content_buffer)
                
                p_Location_start <- str_locate(Content_buffer,'<p')
                p_Location_start <- as.data.frame(p_Location_start)
                p_Location_start
                
                p_Location_inter <- str_locate(Content_buffer,'>')
                p_Location_inter <- as.data.frame(p_Location_inter)
                p_Location_inter
                
              } else {
                print(0)
              }
              
              p_Location_end <- str_locate(Content_buffer,'</p>')
              p_Location_end <- as.data.frame(p_Location_end)
              p_Location_end
              
              p <- substring(Content_buffer,p_Location_inter[,1],p_Location_end[,1])
              p
              
              p <- gsub(">","",p)
              p <- gsub("<","",p)
              p
              
              STRUCTURE_DF[k,'Paragraph'] <- paste(STRUCTURE_DF[k,'Paragraph'],p)
              
              LENGTH_Content_buffer <- nchar(Content_buffer)
              LENGTH_Content_buffer
              Content_buffer <- substring(Content_buffer,p_Location_end[,2],LENGTH_Content_buffer)}      
          } else {
            print('no subtitle - paragraph extraction directly')
            #paragraph extraction
            {
              p_Location_start <- str_locate(Content_buffer,'<p')
              p_Location_start <- as.data.frame(p_Location_start)
              p_Location_start
              
              p_Location_inter <- str_locate(Content_buffer,'>')
              p_Location_inter <- as.data.frame(p_Location_inter)
              p_Location_inter
              
              if (p_Location_inter[,1] < p_Location_start[,1]) {
                print(1)
                Content_buffer <- substring(Content_buffer,p_Location_start[,1],LENGTH_Content_buffer)
                
                p_Location_start <- str_locate(Content_buffer,'<p')
                p_Location_start <- as.data.frame(p_Location_start)
                p_Location_start
                
                p_Location_inter <- str_locate(Content_buffer,'>')
                p_Location_inter <- as.data.frame(p_Location_inter)
                p_Location_inter
                
              } else {
                print(0)
              }
              
              p_Location_end <- str_locate(Content_buffer,'</p>')
              p_Location_end <- as.data.frame(p_Location_end)
              p_Location_end
              
              p <- substring(Content_buffer,p_Location_inter[,1],p_Location_end[,1])
              p
              
              p <- gsub(">","",p)
              p <- gsub("<","",p)
              p
              
              STRUCTURE_DF[k,'Paragraph'] <- paste(STRUCTURE_DF[k,'Paragraph'],p)
              
              LENGTH_Content_buffer <- nchar(Content_buffer)
              LENGTH_Content_buffer
              Content_buffer <- substring(Content_buffer,p_Location_end[,2],LENGTH_Content_buffer)}
          }}
      } 
      
      
      
      else {
        print('There is no subtitle')
        #paragraph extraction
        {
          p_Location_start <- str_locate(Content_buffer,'<p')
          p_Location_start <- as.data.frame(p_Location_start)
          p_Location_start
          
          p_Location_inter <- str_locate(Content_buffer,'>')
          p_Location_inter <- as.data.frame(p_Location_inter)
          p_Location_inter
          
          if (p_Location_inter[,1] < p_Location_start[,1]) {
            print(1)
            Content_buffer <- substring(Content_buffer,p_Location_start[,1],LENGTH_Content_buffer)
            
            p_Location_start <- str_locate(Content_buffer,'<p')
            p_Location_start <- as.data.frame(p_Location_start)
            p_Location_start
            
            p_Location_inter <- str_locate(Content_buffer,'>')
            p_Location_inter <- as.data.frame(p_Location_inter)
            p_Location_inter
            
          } else {
            print(0)
          }
          
          p_Location_end <- str_locate(Content_buffer,'</p>')
          p_Location_end <- as.data.frame(p_Location_end)
          p_Location_end
          
          p <- substring(Content_buffer,p_Location_inter[,1],p_Location_end[,1])
          p
          
          p <- gsub(">","",p)
          p <- gsub("<","",p)
          p
          
          STRUCTURE_DF[k,'Paragraph'] <- paste(STRUCTURE_DF[k,'Paragraph'],p)
          
          LENGTH_Content_buffer <- nchar(Content_buffer)
          LENGTH_Content_buffer
          Content_buffer <- substring(Content_buffer,p_Location_end[,2],LENGTH_Content_buffer)}
      }
      
      
      
      
    }
  }
} 

#PART 3 : CLEANING HREF
{
  for (k in 1:count_h2){
    Content_buffer <- STRUCTURE_DF[k,'Paragraph']
    Content_buffer <- gsub("NA", "", Content_buffer)
    if ( str_detect(Content_buffer,'href')) {
      LENGTH_Content_Buffer <- nchar(Content_buffer)
      
      h3_Location_start <- str_locate(Content_buffer,'href=')
      h3_Location_start <- as.data.frame(h3_Location_start)
      h3_Location_start
      
      Clear_Content_n0 <- substring(Content_buffer,0,h3_Location_start[,1]-4)
      
      href_start <- substring(Content_buffer,h3_Location_start[,2]+2,LENGTH_Content_buffer)
      
      h3_Location_end <- str_locate(href_start,'"')
      h3_Location_end <- as.data.frame(h3_Location_end)
      h3_Location_end
      
      href_end <-  h3_Location_start[,2]+h3_Location_end[,1]
      
      Clear_Content_n1 <- substring(Content_buffer,href_end,LENGTH_Content_Buffer)
      
      Clear_Content <- paste(Clear_Content_n0,Clear_Content_n1)
      Clear_Content <- gsub("/a","",Clear_Content)
      Clear_Content <- gsub('"',"",Clear_Content)
      Clear_Content <- gsub("/", "", Clear_Content)
      
      STRUCTURE_DF[k,'Paragraph'] <- Clear_Content
    }
  }
}
  
#PART 4 : CLEANING NA
{
  for (k in 1:count_h2){
    
    if ( str_detect(STRUCTURE_DF[k,'Paragraph'],'NA')) {
      
      print('NA Detected')
      Clear_Content <- gsub("NA","",STRUCTURE_DF[k,'Paragraph'])
      STRUCTURE_DF[k,'Paragraph'] <- Clear_Content
    }
    
    if (is.na(STRUCTURE_DF[k,'subtitle_content'])) {
    } else {
      if (str_detect(STRUCTURE_DF[k,'subtitle_content'],"NA")) {
        
        print('NA Detected')
        Clear_Content <- gsub("NA","",STRUCTURE_DF[k,'subtitle_content'])
        STRUCTURE_DF[k,'subtitle_content'] <- Clear_Content
      }
    }
    
  }
  } 
  
#PART 5 : CONTENT GATHERING
{
  TITLE_CONTENT_c <- ""
  
  for (k in 1:count_h2){
    STRUCTURE_DF[k,'Title_Part'] <- gsub('</h2>','',STRUCTURE_DF[k,'Title_Part'])
    TITLE_CONTENT_c <- paste(TITLE_CONTENT_c,STRUCTURE_DF[k,'Title_Part'])
  }
}
  
  
### TEXT ANALYSIS ###

#PART 1 
{
  text_content <- c(STRUCTURE_DF[1,'Paragraph'],STRUCTURE_DF[2,'Paragraph'],STRUCTURE_DF[3,'Paragraph'],STRUCTURE_DF[4,'Paragraph'],STRUCTURE_DF[5,'Paragraph'],STRUCTURE_DF[6,'Paragraph'],STRUCTURE_DF[7,'Paragraph'])
  text_df <- tibble(line = 1:1, text = text_content)
  text_df %>% unnest_tokens(word, text)
  
  content_text_df <- text_df %>%
    mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stopwords_iso$fr) %>%
    count(word, sort = TRUE)
  
  content_text_df_head <- head(content_text_df,10)
  content_text_df_head %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 1) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)
}  
  
#PART 2 : TEXT ANALYSIS META
{
  text_meta <- c(META)
  text_df <- tibble(line = 1:1, text = text_meta)
  text_df %>% unnest_tokens(word, text)
  
  meta_text_df <- text_df %>%
    mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stopwords_iso$fr) %>%
    count(word, sort = TRUE)
  
  meta_text_df %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 0) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)
}
  
#PART 3 : TEXT ANALYSIS TITLE
{
  text_title <- c(TITLE_CONTENT_c)
  text_df <- tibble(line = 1:1, text = text_title)
  text_df %>% unnest_tokens(word, text)
  
  title_text_df <- text_df %>%
    mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
    unnest_tokens(word, text) %>%
    filter(!word %in% stopwords_iso$fr) %>%
    count(word, sort = TRUE)
  
  title_text_df %>%
    mutate(word = reorder(word, n)) %>%
    filter(n > 1) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)

}
  
#PART 4 : COMBINE ANALYSIS META x CONTENT
{
  join <- merge(x = content_text_df, y = meta_text_df, by = "word", all = TRUE)
  join_clean <- join %>% mutate_all(funs(replace_na(.,0)))
  
  x <- as.vector(join_clean$n.x)
  y <- as.vector(join_clean$n.y)
  # Plot with main and axis titles
  # Change point shape (pch = 19) and remove frame.
  plot(x, y, main = "Main title",
       xlab = "From Content", ylab = "From Meta",
       pch = 19, frame = FALSE)
  # Add regression line
  plot(x, y, main = "Main title",
       xlab = "X axis title", ylab = "Y axis title",
       pch = 19, frame = FALSE)
  abline(lm(y ~ x, data = mtcars), col = "blue")
  
  LENGTH_JOIN <- length(t(join_clean))
  m=1
  for(m in 1:LENGTH_JOIN){
    print(m)
    if (join_clean[m,'n.y']==0) {
      print(1)
      join_clean[m,'Keyword_type'] <- 'None'
    } else {
      if (join_clean[m,'n.y']==1) {
        print(2)
        join_clean[m,'Keyword_type'] <- 'Secondary'
      } else {
        if (join_clean[m,'n.y']>1) {
          print(3)
          join_clean[m,'Keyword_type'] <- 'Primary'
        }
      }
    }
    
  }
} 

#PART 5 : KEYWORD ANALYSIS
{
nb_of_keyword <- sum(join_clean[,'n.x'])
  
  nb_primary_keyword <- 0
  nb_secondary_keyword <- 0
  for(m in 1:LENGTH_JOIN){
    if (join_clean[m,'Keyword_type']=='Primary'){
      nb_primary_keyword <- nb_primary_keyword + join_clean[m,'n.y']
    } else {
      if (join_clean[m,'Keyword_type']=='Secondary'){
        nb_secondary_keyword <- nb_secondary_keyword + join_clean[m,'n.y']
      }
      
    }
    
  }
}
  
#PART 6 : KEYWORD SCORE 
target_keyword <- sum(nb_primary_keyword,nb_secondary_keyword)
KEYWORD_STUFFING_SCORE <- target_keyword/nb_of_keyword
  

#RANKING LOOPS
{
  RANK = 0
  #Critère 1 : Si la page contient une descritption et un titre (2)
  if (is.na(M.DESCRIPTION_CONTENT)) {
    print(0)
  } else {
    print(1)
    RANK = RANK+1
  }
  
  if (is.na(M.TITLE_CONTENT)) {
    print(0)
  } else {
    print(1)
    RANK = RANK+1
  }
  
  #Critère 2 ! Si la descritption meta est entre 150 & 320 caractères (1)
  
  if (150<nchar(M.DESCRIPTION_CONTENT)) {
    print(0)
  } else {
    if (nchar(M.DESCRIPTION_CONTENT)<320) {
      print(1)
      RANK = RANK+1
    }
  }
  
  #Critère 3 : Si la page contient un unique h1 et au moins 3 h2 (2)
  if (is.na(count_h1)) {
    print(0)
  } else {
    print(1)
    RANK = RANK+1
  }
  
  if (count_h2<3) {
    print(0)
  } else {
    print(1)
    RANK = RANK+1
  }
  
  #Critère 4 : Si le contenu du h1 ne dépace pas 70 caractères (1)
  nchar(H1_CONTENT_CLEAN)
  if (15<nchar(H1_CONTENT_CLEAN)) {
    print(0)
  } else {
    if (nchar(H1_CONTENT_CLEAN)<70) {
      print(1)
      RANK = RANK+1
    }
  }
  #Critère 5 : le toal du contenu doit être entre 2250 et 2750 caractères
  length_content <- nchar(text_content)
  length_content <- sum(length_content)
  
  if (2250<length_content) {
    print(0)
  } else {
    if (length_content<2750) {
      print(1)
      RANK = RANK+1
    }
  }
  
  #Critère 6 : Keyword_Stuffing_Score
  if (KEYWORD_STUFFING_SCORE<0.02) {
    print(0)
  } else {
    if (KEYWORD_STUFFING_SCORE>0.02) {
      print('Good Keyword Score')
      RANK = RANK + 3
    } else {
      if (KEYWORD_STUFFING_SCORE<0.0305) {
        print('Keyword Stuffing!')
        RANK = RANK-3
      }
    }
  }
}

print(paste('The grade of the article is around',round(RANK,2)))

