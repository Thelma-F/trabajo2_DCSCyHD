library (tidyverse)
library(tidytext)


corpus <- read.csv("C:/Beatriz/Documentos/2023/Diplomatura/TPFinal/M5_corpus_medios.csv")

stop_words <- read_csv('https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt', col_names=FALSE) %>%
  rename(word = X1) %>%
  mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))

corpus_limpio <- corpus %>%
  mutate(texto = tolower(gsub("[[:punct:]0-9]", "", texto))) %>%
  mutate(texto = str_squish(texto))%>%
 unnest_tokens(output = word, input = text)

library(dplyr)
library(readr)
library(stringr)
library(tidytext)

# Cargar el corpus y las stop words
corpus <- read.csv("C:/Beatriz/Documentos/2023/Diplomatura/TPFinal/M5_corpus_medios.csv")
stop_words <- read_csv('https://raw.githubusercontent.com/Alir3z4/stop-words/master/spanish.txt', col_names=FALSE) %>%
  rename(word = X1) %>%
  mutate(word = stringi::stri_trans_general(word, "Latin-ASCII"))

# PREPROCESAMIENTO Limpieza Y normalización del texto: conversión a minúsculas


corpus_limpio <- corpus %>%
  mutate(texto_limpio = tolower(gsub("[[:punct:]0-9]", "", texto))) %>%
  mutate(texto_limpio = str_squish(texto_limpio)) %>%
  unnest_tokens(output = word, input = texto_limpio)


#Remover palabras comunes que no aportan 

palabras_vacias <- c("a", "aún", "ante", "bajo", "con", "contra", "de", "desde", "durante","día", "días",
                     "en", "entre", "hacia", "hasta", "mediante", "para", "por", 
                     "según", "sin", "sobre", "también", "más", "año", "años", "está", "están", 
                     "había", "después", "qué", "que", "además", "así", "embed", 'leé', 
                     "cómo", "como", "sólo", "través", "tenía", "sí", "si", "podría", "será", "allí", "ahí", 
                     'cronica.com.ar', 'minutouno.com', 'comentar', 'jpg', 'loading', 
                     'pristupluk', 'minutouno.com', 'guardar', 'páginai12', '01', 
                     'l.l', 'loading', 'jpe', 'más', 'también', 'está', 'había', 'qué', 
                     'así', 'están', 'además', 'día', '1', 'gusta', 'twitter', 'fuente', 
                     'whatsapp', 'compartir', 'facebook', 'mail', 'él', "cronicacomar", "minutounocom", "email", "páginai", "nacion")

stop_words1 <- tibble(word = palabras_vacias)

# Remover palabras vacías directamente del corpus limpio
corpus_limpio <- anti_join(corpus_limpio, stop_words1, by = "word")


corpus_limpio <- corpus_limpio %>%
  anti_join(stop_words, by  = "word")

#exploración del texto

corpus_cuenta <- corpus_limpio %>%
  count(word, sort=TRUE)


corpus_limpio %>% 
  group_by(word) %>%  
  summarize(n = n()) %>%  
  arrange(desc(n))


corpus_tf_idf <- corpus_limpio  %>%
  count(medio, word, sort = TRUE) %>%
  bind_tf_idf (word, medio, n) %>% 
  arrange(desc(tf)) %>% 
  print()



top_words_by_medio <- corpus_tf_idf %>%
  group_by(word,medio,  tf, tf_idf) %>%
  top_n(40, wt = tf_idf) %>%
  arrange(desc(tf_idf)) %>% 
  print()



top_words_by_medio <- corpus_limpio %>%
  group_by(medio, word) %>%
  summarise(tf = n()) %>%
  arrange(medio, desc(tf)) %>%
  group_by(medio) %>%
  top_n(10, wt = tf) %>% 
  print()



#clarin

top_words_by_clarin <- corpus_tf_idf %>%
  filter(medio == "clarin") %>% 
  group_by(medio) %>%
  top_n(20, wt = tf) %>%
  arrange(desc(n)) %>% 
  print()


top_words_by_medio <- corpus_tf_idf %>%
  group_by(medio) %>%
  top_n(10, wt = tf)

#Paso la primera letra de los medios en mayúscula

top_words_by_medio$medio <- str_to_title(top_words_by_medio$medio)


# Crear el gráfico
ggplot(top_words_by_medio, aes(x = tf, y = reorder(word, tf), fill = medio)) +
  geom_col() +
  scale_fill_discrete(name = "Medio") +
  labs(x = "TF-IDF", y = "Palabra") +
  facet_wrap(~medio, scales = "free_y") +
  theme_minimal()


top_words_by_medio <- corpus_tf_idf %>%
  group_by(medio) %>%
  top_n(10, wt = tf_idf)


#Paso la primera letra de los medios en mayúscula

top_words_by_medio$medio <- str_to_title(top_words_by_medio$medio)

# Crear el gráfico
ggplot(top_words_by_medio, aes(x = tf_idf, y = reorder(word, tf_idf), fill = medio)) +
  geom_col() +
  scale_fill_discrete(name = "Medio") +
  labs(x = "TF-IDF", y = "Palabra") +
  facet_wrap(~medio, scales = "free_y") +
  theme_minimal()



library(topicmodels)
library(tictoc)
library(stm)


word_counts <- corpus_limpio %>%
  group_by(word, medio) %>%
  summarise(n=n()) %>%
  ungroup()


#Dfm
corpus_dfm <- word_counts %>%
  cast_dfm(medio, word, n)

#Dtm
corpus_dtm <- word_counts %>%
  cast_dtm(medio, word, n)


lda_4 <- LDA(corpus_dtm, k=4, control = list(seed = 1234))

ap_topics <- tidy(lda_4, matrix = "beta") # Si esta línea les tira algún error, hagan install.packages("reshape2")

ap_topics %>%
  mutate(beta = round(100*beta,6))

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms


grafo2 <-  ggplot(ap_top_terms, aes(reorder(term, beta), beta, fill = beta)) +
  geom_col() +
  scale_fill_viridis_c() +
  facet_wrap(~ topic, scales = "free") +
  labs(title = "Términos más relevantes por grupo de tópicos", y = "Valor de Beta") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar etiquetas del eje x en 90 grados
        legend.position = "none",  # Eliminar la leyenda
        text = element_text(size = 8))  

ggsave("Grafo2.png", plot = grafo2, width = 10, height = 6, dpi = 300, bg = "white")



beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic3 > .002 | topic4 > .002) %>%
  mutate(log_ratio3_4 = log2(topic4 / topic2))

beta_wide


beta_wide %>%
  ggplot(aes(x=reorder(term,log_ratio3_4) , y=log_ratio3_4)) +
  geom_col() +
  coord_flip() +
  labs(x='Término',
       y='Log2 ratio topic4/topic3') +
  theme_minimal()
ggsave("Grafo2.png", plot = grafo2, width = 10, height = 6, dpi = 300)




