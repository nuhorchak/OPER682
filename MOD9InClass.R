library(harrypotter)
library(stringr)
library(tidyverse)
library(tidytext)
library(readr)

#find Mr. and Mrs.
str_count(philosophers_stone, "M(r|rs)") %>%  sum()

#extract all ements in dealthly hallows that start with harry
deathly_hallows[str_detect(deathly_hallows, "^Harry")]
str_extract(philosophers_stone, "Harry.")

#harry folowed by work with vowel
str_count(philosophers_stone, "Harry.[aeiou]") %>%  sum()

#25 character before and after harry
str_extract(philosophers_stone, ".{25}Harry.{25}")

#tidy text
text_tb <- tibble(
  chapter = seq_along(philosophers_stone),
  text= philosophers_stone
)

text_tb %>%  unnest_tokens(words, text) 
text_tb %>% unnest_tokens(words, text, token="sentences")
text_tb %>%  unnest_tokens(words, text, token="ngrams", n=10)

#filter original text, dont include stop words, get count of words
text_ps <- tibble(
  chapter = seq_along(philosophers_stone),
  text= philosophers_stone
) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

#frequency analysis of words in chamber of secrets
text_cs <- tibble(
  chapter = seq_along(chamber_of_secrets),
  text = chamber_of_secrets
) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)


text_diff <- text_cs %>%
  rbind(text_ps) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(book, word) %>%
  spread(book, n)
  