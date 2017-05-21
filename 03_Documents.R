# TF-IDF Concept

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(janeaustenr)

# Processing words from Jane Austen novels
book_words = austen_books() %>% 
        unnest_tokens(output = word, input = text) %>% 
        count(book, word, sort = T) %>% 
        ungroup()

book_words

total_words = book_words %>% 
        group_by(book) %>% 
        summarise(total = sum(n))

total_words

# tidy text format
book_words = left_join(x = book_words, y = total_words, by = "book")
book_words

## Visualizing term frequency - the tails represent most common words
'Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.'
book_words %>% ggplot(aes(n/total, fill = book)) +
        geom_histogram(show.legend = FALSE, alpha = 0.5) +
        facet_wrap(~book, ncol = 2, scales = "free_y") +
        xlim(NA, 0.0006) +
        theme_minimal()


freq_rank = book_words %>%
        arrange(desc(n)) %>% 
        group_by(book) %>%
        mutate(rank = row_number(), 
               term_freq = n/total)


freq_rank %>% ggplot(aes(x = rank, y = term_freq, color = book)) +
        geom_line(size = 1.3, show.legend = F) +
        scale_x_log10() +
        scale_y_log10() +
        theme_minimal() +
        facet_wrap(~book)
        

## TF-IDF
book_words = book_words %>% 
        bind_tf_idf(term_col = word, document_col = book, n_col = n) 

book_words # IDF and TF-IDF os zero for common words

book_words %>% 
        select(-total) %>% 
        arrange(desc(tf_idf)) # tf_idf is higher for uncommon words


book_words = book_words %>% arrange(desc(tf_idf))
book_words$word = factor(book_words$word, levels = unique(book_words$word))

book_words %>%
        group_by(book) %>% 
        top_n(10) %>%
        ungroup() %>% 
        ggplot(aes(x = word, y = tf_idf, fill = book)) +
        facet_wrap(~book, scales = "free") +
        geom_col(show.legend = F) +
        coord_flip() +
        theme_minimal()





