library(tm)
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

source("cleaning.R")

# Tokenization

discurso_df <- tibble(line = 1, text = discurso_texto)

words <- discurso_df |>
  unnest_tokens(word, text) |>
  anti_join(tibble(word = tm::stopwords("es")))

words |>
  count(word, sort = TRUE) |>
  mutate(word = reorder(word, n)) |>
  filter(n > 5) |>
  ggplot(aes(word, n)) +
  geom_col(fill = "#0077b6") +
  coord_flip() +
  labs(
    y = "# veces que aparece",
    x = "Palabra",
    title = "Discurso presidencial Gabriel Boric. 19 Dic 2021\nPalabras más repetidas (> 5 veces)",
    subtitle = "\n",
    caption = "\nAutor: Paulo Villarroel"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 18))


# Bigrams

words_bigram <- discurso_df |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- words_bigram |> 
  separate(bigram, c("word1", "word2"), sep = " ")

stop_words_es <- tm::stopwords("es")

bigrams_filtered <- bigrams_separated |> 
  filter(!word1 %in% stop_words_es) |> 
  filter(!word2 %in% stop_words_es)

bigram_counts <- bigrams_filtered |> 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered |> 
  unite(bigram, word1, word2, sep = " ")

bigrams_united |> 
  count(bigram, sort = TRUE) |> 
  filter(n > 1) |> 
  mutate(bigram = reorder(bigram, n)) |> 
  ggplot(aes(n, bigram)) +
  geom_col(fill = "deeppink") +
  labs(
    y = "Bigrama",
    x = "# veces que aparece",
    title = "Discurso presidencial Gabriel Boric. 19 Dic 2021\nBigramas más repetidas",
    caption = "\nAutor: Paulo Villarroel"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 18))


# Plot bigrams

bigram_graph <- bigram_counts |> 
  #filter(n > 1) |> 
  graph_from_data_frame()
bigram_graph

set.seed(1234)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE)
