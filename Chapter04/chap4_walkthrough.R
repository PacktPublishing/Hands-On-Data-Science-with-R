## Drawven Name
if(!require(httr)){install.packages('httr')}
library(httr)

form_dt <- list(
  nametype = 2,
  numnames = 1,
  gender = 1,
  surnametype = 2,
  namegenraceid = 1
)

url <- 'http://www.rdinn.com/generators/1/dwarven_name_generator.php'
post <- POST(url, body = form_dt, verbose())
page <- content(post, as = 'text')

pattern_1 <- '<td width=\"25%\">'
pattern_2 <- '</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;'

dwarven_name <- substr(page,
                     start = regexpr(pattern_1,page) + nchar(pattern_1),
                     stop = regexpr(pattern_2, page) - 1)
dwarven_name


## Retrieving text from the web
if(!require(rvest)){install.packages('rvest')}
library(rvest)

url <- 'https://cran.r-project.org/web/packages/available_packages_by_name.html'
xpath <- '/html/body/table//a'
dt <- read_html(url) %>%
  html_nodes(xpath = xpath)

head(dt)

dt <- dt %>% 
  gsub(pattern = '<(.[^>]*)>',
       replacement = '')

head(dt)

r_list <- available.packages()[,'Package']
setdiff(r_list,dt)
setdiff(dt,r_list)


if(!require(rtweet)){install.packages('rtweet')}
library(rtweet)

my_token <- create_token(
  app = '*YOUR* APP NAME',
  consumer_key = '*YOUR* API KEY',
  consumer_secret = '*YOUR* API SECRET')

if(!require(beepr)){install.packages('beepr')}
library(beepr)

tweets_dt <- search_tweets2(q = '#rstats', 
                            n = 20000, 
                            include_rts = T,
                            tweet_mode = 'extended',
                            retryonratelimit = T,
                            token = my_token)

for(i in 0:2){beep(5); Sys.sleep(3)}

#or
#tweets_dt <- read_twitter_csv(url('http://bit.do/rstats4life'))

dim(tweets_dt)
names(tweets_dt)

## Cleaning and transforming data
quotes <- tweets_dt$is_quote
rts <- tweets_dt$is_retweet

dt <- c(tweets_dt$quoted_text[quotes],
        tweets_dt$retweet_text[rts],
        tweets_dt$text[!rts])

dt <- data.frame(tweets = dt, 
                 stringsAsFactors = F)

if(!require(dplyr)){install.packages('dplyr')}
if(!require(tidytext)){install.packages('tidytext')}

library(dplyr) ;library(tidytext)

pkgs <- row.names(available.packages())

clean_dt <- dt %>%
  unnest_tokens(word, tweets, 
                to_lower = F) %>%
  filter(word %in% pkgs) %>%
  count(word, sort = T)

head(clean_dt, 10)

clean_dt <- clean_dt[-(2:8),]
head(clean_dt, 10)


## Looking for patterns
dim(clean_dt)
tail(clean_dt, 10)
summary(clean_dt)

# Visualizing data

if(!require(ggplot2)){install.packages('ggplot2')}

clean_dt$word <- factor(clean_dt$word,
                        levels = rev(clean_dt$word))

library(ggplot2)

ggplot(data = clean_dt[1:10,], aes(x = n, y = word)) +
  geom_segment(linetype = 'dashed',
               size = .1,
               aes(yend = word, 
                   x = min(n) - 50, 
                   xend = n)) +
  geom_point(size = 17, color = '#e66101') +
  geom_text(aes(label = n), size = 6) +
  coord_cartesian(xlim = c(120, 500))

if(!require(wordcloud)){install.packages('wordcloud')}
if(!require(tm)){install.packages('tm')}

#par(mar = rep(0,4))
library(wordcloud)
set.seed(10)
wordcloud(word = clean_dt$word, freq = clean_dt$n, random.order = F, 
          rot.per = .35, scale = c(5,.5), max.words = 50, 
          colors = c('#e66101','#fdb863','#b2abd2','#5e3c99'))


# clean_dt$col <- '#b2abd2'
# clean_dt$col[clean_dt$word %in% installed.packages()] <- '#e66101'
# wordcloud(word = clean_dt$word[1:50], freq = clean_dt$n[1:50], 
#           random.order = F, rot.per = .35, scale = c(5,.5),
#           colors = clean_dt$col[1:50], ordered.colors = T)


tw_clust <- hclust(dist(clean_dt$n[1:25]))
plot(tw_clust, labels = clean_dt$word[1:25])

if(!require(dendextend)){ install.packages('dendextend')}
library(dendextend)

tw_clust %>% as.dendrogram() %>%
  set('labels', as.character(clean_dt$word[1:25])) %>%
  set('labels_col', k = 3, 
      value = c('#e66101','#fdb863','#b2abd2')) %>%
  set('branches_k_color', k = 3,
      value = c('#e66101','#fdb863','#b2abd2')) %>%
  set('labels_cex', 1.1) %>%
  plot()

cutree(tw_clust, k = 3)
cutree(tw_clust, h = 250)