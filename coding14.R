library(dplyr)
library(tm)
library(stopwords)
library(katadasaR)
library(stringi)
library(qdapRegex)
library(networkD3)
library(readxl)

df <- read.csv("covid19_tweet.csv")

# kata normalisasi
old <- c(" ni "," tu "," org "," yg "," ga ","ambik"," klo "," ngga ","tdk", " nggak ", " ya ","kualalumpur","corona","pedemik")
new <- c(" ini "," itu "," orang "," yang "," tidak ","ambil"," kalau "," tidak "," tidak ", " tidak "," iya ","KualaLumpur","covid","pandemi")

#butuh dihapus
hapus <- c("covid-19","vaksin","covid", "pandemi","lock down")

fungsi_proses <- function(a){
  a %>%
    casefold() %>%
    rm_url(pattern = pastex("@rm_twitter_url","@rm_url")) %>%
    gsub('#\\S+','',.) %>%
    gsub('@\\S+','',.) %>%
    gsub('[^[:alpha:] ]','',.) %>%
    stri_trans_general("latin-ascii") %>%
    stri_replace_all_fixed(old,new,vectorize_all = FALSE) %>%
    removeWords(stopwords(language = "id", source = "nltk")) %>%
    removeWords(hapus)
}

#Untuk tugas akhir, filter berdarkan tema
#Tema : "pemerintah|menteri"
library(stringr)
dffilter <- filter(df,str_detect(text,"pemerintah|menteri"))

dfnew <- mutate(dffilter,komentar_bersih=sapply(text,fungsi_proses))

library(tidytext)
dftoken <- dfnew %>%
  select(komentar_bersih) %>%
  unnest_tokens(kata,komentar_bersih) %>%
  count(kata) %>%
  top_n(30)

# visualisasi sederhana
library(ggplot2)
dftoken %>%
  top_n(25,n) %>%
  ggplot(aes(x=reorder(kata,n),y=n)) + geom_col() + coord_flip()

library(wordcloud2)
dftoken %>%
  top_n(100,n) %>%
  wordcloud2()

#B.Persiapan Data Network###

#B.1. ambil Kolom user dan teks
df1 <- dfnew %>% select(screen_name,text)

#B.2. tambah @ ke username
df1$screen_name <- paste0('@',df1$screen_name)

#B.3. gabungkan 2 kolom
library(tidyr)
df2 <- df1 %>%
  unite(teks,screen_name,text,sep=" ")

#B.4.Ambil hanya user(@.....)
df3 <- str_extract_all(df2$teks,"(@[[:alnum:]_]*)")
df3 <- sapply(df3,paste,collapse=" ")
df3 <- data.frame(df3)

#B.5. Menghapus jumlah user dalam satu baris
df3$count <- str_count(df3$df3,"\\S+")

#B.6. Menghapus baris yang berisi hanya satu baris
df4 <- df3 %>% filter(count > 1)

#B.7 Membuat pasangan source target
library(tidytext)
df5 <- df4 %>%
  select(df3) %>%
  unnest_tokens(username,df3,token = "ngrams",n=2)

#B.8. Memisahkan ke dalam 2 kolom
df6 <- df5 %>%
  separate(username,into=c("source","target"),sep=" ")

#B.9. tambah #@ depan username
df6$source <- paste0('@',df6$source)
df6$target <- paste0('@',df6$target)

#Contoh Visualisasi
library(igraph)
ig <- graph_from_data_frame(df6,directed=FALSE)

#export file
write_graph(ig,"covid19.graphml",format="graphml")

