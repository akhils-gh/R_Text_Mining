library(tidyverse)
library(tidytext)
library(stringr)

# Loading text
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

df_text = data_frame(line = 1:4, text)
df_text

# Converting text to tidy format (tokenizing)
df_text %>%
        unnest_tokens(output = word, input = text)


#-------- Analyzing Jane Austen Works using Tidy text

install.packages("janeaustenr")
library(janeaustenr)

# creating row and chapter number
original_books = austen_books() %>% 
        group_by(book) %>% 
        mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = T)))) %>% 
        ungroup()

original_books

# Tokeninzing
tidy_books = original_books %>% 
        unnest_tokens(output = word, input = text)

tidy_books

# removing stop words
data("stop_words") # in tidy text package

tidy_books = tidy_books %>% 
        anti_join(y = stop_words) # default join by words

tidy_books %>% 
        count(word, sort = T) # same as groupby and n

# Function to plot word count
plot_wc = function(tidy_df, wc = 650){
                gg_obj = tidy_df %>% 
                        count(word, sort = T) %>% 
                        filter(n > wc) %>% 
                        mutate(word = reorder(word, n)) %>% 
                        ggplot(aes(x = word, y = n)) +
                        geom_col(fill = "deepskyblue3") +
                        coord_flip() +
                        theme_minimal()
                
                return(gg_obj)
}

plot_wc(tidy_df = tidy_books)

#--------- Comparing works by authors

install.packages("gutenbergr") # package to download works of authors
library(gutenbergr)

# HG Wells works
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells = hgwells %>% 
        unnest_tokens(word, text) %>% 
        anti_join(stop_words)

plot_wc(tidy_hgwells, wc = 200)

# Bronte Sister Works
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte = bronte %>% 
        unnest_tokens(word, text) %>% 
        anti_join(stop_words)

plot_wc(tidy_bronte, wc = 550)

## Lets compare word frequencies
all_works = bind_rows(mutate(tidy_books, author = "Jane Austen"),
                      mutate(tidy_hgwells, author = "HG Wells"),
                      mutate(tidy_bronte, author = "Bronte Sister"))

all_works$author %>% table()

freq = all_works %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>% 
        count(author, word) %>% 
        group_by(author) %>% 
        mutate(prop = n/sum(n)) %>% 
        select(-n) %>%
        spread(key = author, value = prop) %>% 
        gather(key = author, value = prop, `Bronte Sister`:`HG Wells`)
        
ggplot(freq, aes(x = prop, y = `Jane Austen`, color = abs(`Jane Austen` - prop))) +
        geom_jitter(alpha = 0.1) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        geom_abline(color = "blue", lty = 2) +
        facet_wrap(~author) +
        scale_x_log10() +
        scale_y_log10() +
        theme_light() +
        labs(x= NULL, y = "Jane Austen") +
        scale_color_gradient(low = "darkgreen", high = "red", guide = F) 
        
