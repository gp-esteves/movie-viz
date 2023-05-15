library(jsonlite); library(httr); library(tidyverse); library(stringr)
library(ggtext); library(ggimage); library(viridis); library(patchwork)
library(ggside); library(showtext); library(ggridges); library(rms);
library(ggpmisc)

options(scipen = 9999)

# api_key <- "YourKeyHere" # Make sure to get and insert your own key here!
#movie_ids <- read.csv("movie_ids.csv")
load(file="horror_df.RData")
font_add_google("Nosifer", family = "special")
font_add_google("Black Han Sans", family = "blackHans")
showtext_auto()

theme_dark <- function() {
  theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray17"),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(fill = 'gray12', color = 'gray12'),
          plot.background = element_rect(fill = 'gray12', color = 'gray12'),
          axis.title = element_text(colour = "gray90"),
          axis.text = element_text(colour = "gray90"),
          plot.title = element_text(colour = "gray90", family="special", hjust=0.5, 
                                    size=15),
          plot.subtitle = element_text(colour = "gray90"),
          plot.caption = element_text(colour = "gray90"),
          legend.text = element_text(colour = "gray90"),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.background = element_blank())
  
}

# Cleaning and unnesting df

horror_nested <- horror_df |>
  select(adult, backdrop_path, budget, genres, id, imdb_id, original_language, 
         original_title, popularity, production_companies, production_countries, 
         revenue, runtime, spoken_languages, title, vote_average, vote_count) |> 
  unnest_wider(c(genres, production_companies,
                 spoken_languages, production_countries), 
               names_repair = "unique") |> 
  select(-c(adult, id...4, id...6, id...11, origin_country, iso_639_1,
            name...16, english_name, logo_path)) |> 
  rename(genres = name...5, produc_comp = name...13, 
         origin_country = iso_3166_1, spoken_lang = name...21) |> 
  mutate(across(.cols = c(budget, popularity, revenue, runtime, 
                          vote_average, vote_count),
                .fns = as.numeric)) 

## Unnesting

horror <- horror_df |> 
  unnest(backdrop_path) |> 
  unnest(genres) |> pivot_wider(names_from=genres, values_from=genres) |> 
  unnest(imdb_id) |> 
  unnest(original_language) |> 
  unnest(original_title) |> 
  unnest(produc_comp) |> pivot_wider(names_from=produc_comp, values_from=produc_comp, names_repair="unique") |> 
  unnest(origin_country) |> pivot_wider(names_from=origin_country, values_from=origin_country, names_repair="unique")

#save(horror, file="horror_df_clean.RData")
load(file="horror_df_clean.RData")

## Top 50 popularity plot ##

top_5 <- movie_ids |> 
  slice_max(popularity, n=5) |> 
  mutate(original_title = fct_reorder(original_title, -popularity))

titles <- str_replace_all(str_replace(top_5$original_title, ":", ""), " ", "_")

poster_destinations <- paste0("Posters/", titles, ".png")

popular <- movie_ids |> 
  slice_max(popularity, n=50) |> 
  mutate(original_title = fct_reorder(original_title, popularity)) |> 
  mutate(pop_new = case_when(popularity > 9000 ~ 3500,
                             TRUE ~ as.numeric(popularity)))

images <- data.frame(path = paste0("Posters/", titles, ".png")) |>  
  mutate(popularity = top_5$popularity,
         y = c(40, 35, 30, 25, 16.5),
         original_title = top_5$original_title,
         pop_tag = case_when(popularity > 1750 ~ as.integer(popularity)),
         pop_tag2 = case_when(popularity < 1750 ~ as.integer(popularity)))

p1 <- ggplot(popular, aes(y=original_title, x=popularity, fill=pop_new)) +
  geom_bar(stat="identity", color="black", width=1, alpha=.95) +
  scale_fill_gradientn(colours = viridis(256, option = "A", begin = 0.25, end = 0.75)) +
  labs(y=NULL, x="Popularity Score", title="Top 50 popular movies as of today",
       subtitle="Source: The Movie Database (TMBD) API") +
  guides(fill="none") +
  scale_x_continuous(expand=c(0, 0.05)) +
  theme_dark() +
  theme(axis.text = element_text(colour = "gray90", size=6.5))

#ggsave("Plots/popular_titles.png", units="in", width=8, height=8, dpi=600, type="cairo-png")

transparent <- function(img) {
  magick::image_fx(img, expression = "0.1*a", channel = "alpha")
}

p2 <- ggplot(top_5, aes(y=original_title, x=popularity)) +
  geom_ribbon(aes(xmin=min(popularity), xmax=popularity, y=original_title, group=1), 
              alpha=.35, fill="grey75") +
  geom_line(aes(group=1), color="white", size=1, alpha=.75) +
  geom_point(color="grey75", size=3, shape=18) +
  scale_fill_gradientn(colours = viridis(256, option = "A", begin = 0.25, end = 0.75)) +
  geom_image(data = images, aes(image=path, y=original_title, x=popularity + 1050),
             size = .1185, image_fun=transparent) +
  scale_x_continuous(expand=expansion(mult = c(0.01, 0.15))) +
  scale_y_discrete(expand=expansion(mult = c(0.125, 0.125))) +
  theme_dark() +
  labs(x=NULL, y=NULL, title="Top 5 movies",
       subtitle="Source: The Movie Database (TMBD) API") +
  theme(panel.grid.major.x=element_line(color="grey25"),
        panel.grid.major.y=element_blank(),
        panel.background = element_rect(fill = 'gray18', colour = 'black'),
        plot.background = element_rect(fill = 'gray15', colour = 'black'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(colour = "gray90"),
        plot.subtitle = element_text(colour = "gray90"))

p1 + p2 +
  plot_layout(widths = c(.75, 1))

ggsave("Plots/top5popular_titles.png", units="in", width=12, height=6, dpi=600, type="cairo-png")

#ggsave("Plots/horror_budget_popularity.png", units="in", width=8, height=8, dpi=600, type="cairo-png")

## Best grossing horror movies ##

base_url <- "https://image.tmdb.org/t/p/"
file_size <- "w500"

horror <- horror |> 
  mutate(profit = revenue - budget) 

horror_profit <- horror |> 
  slice_max(profit, n=10) |> 
  select(backdrop_path, original_title, profit, budget, revenue) |> 
  rename(Profit = profit, Budget = budget, Revenue = revenue) |> 
  mutate(across(Profit:Revenue, ~ . / 1000000),
         original_title = fct_reorder(original_title, Profit)) |> 
  pivot_longer(cols=Profit:Revenue) |> 
  mutate(name = fct_relevel(name,  "Budget", "Revenue",  "Profit"),
         label_p = case_when(name == "Profit" ~ as.numeric(format(round(value, 0), nsmall=0))))

horror_path <- horror |> 
  slice_max(profit, n=10)

poster_paths <- paste0(base_url, file_size, horror_path$backdrop_path)

poster_destinations <- paste0("C:\\Users\\Gabriel\\Desktop\\movie-viz\\Posters\\top_grossing\\", 
                              horror_path$original_title, ".jpg")


colors <- c("Profit" = "#ff6912", "Budget" = "#FFFF30", "Revenue" = "#C50909")



images <- data.frame(path = paste0("Posters/top_grossing/", horror_path$original_title, ".jpg")) |>  
  mutate(original_title = horror_path$original_title)


ggplot(horror_profit, aes(y=original_title)) +
  geom_bar(aes(x=value, fill=name), stat="identity", color="black", width=.6,
           position = position_dodge(width=.59), linewidth=0.85) +
  geom_text(aes(label=original_title, x=0, y=original_title), hjust=0,
            position=position_nudge(y=.45), color="gray90", size=3) +
  geom_text(aes(label=paste0("$", label_p, "m"), x=label_p, y=original_title), hjust=0,
            position=position_nudge(y=0.2265, x=3.5), color="gray90", size=3.75) +
  geom_image(data = images, aes(image=path, y=original_title, x=-50),
             size = .1) +
  scale_fill_manual(values=colors) +
  scale_x_continuous(breaks=seq(0, 600, 100)) +
  labs(x="USD (Millions)", y=NULL, 
       title="Horror movies with scary high profits",
       caption="Source: The Movie Database (TMDB) \n Graph inspired by @ Tanya Shapiro") +
  theme_dark() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.y = element_blank(),
        legend.position = c(0.925, 0.5)) 

showtext_opts(dpi=100) 
ggsave("Plots/horror_profits.png", units="in", width=7, height=10, dpi=600, type="cairo-png")

## Looking at production companies in horror movies

prodComp <- horror_nested |> 
  filter(vote_count > 100) |>
  select(original_title, vote_average, produc_comp) |> 
  unnest(original_title) |> 
  unnest(produc_comp) |> pivot_wider(names_from=produc_comp, values_from=produc_comp, values_fill="No", names_repair="unique") |> 
  mutate(across(3:198, ~ ifelse(.x == "No", "No", "Yes"))) |> 
  pivot_longer(cols=c(-1, -2)) |> filter(value=="Yes") |> select(-value)

mostCommonProd <- prodComp |> 
  count(name) |> 
  arrange(-n) |> 
  slice_max(n, n=20)

ggplot(mostCommonProd, aes(y=fct_reorder(name, n), x=n)) +
  geom_bar(aes(fill=n), stat="identity", width=.95, color="black", linewidth=.5) +
  geom_point(color="white", size=2) +
  geom_line(aes(group=1), linewidth=1, color="white") +
  geom_label(aes(label=n, x=n), color="grey95", fill="black",
            position=position_nudge(x=8.5)) +
  scale_fill_gradientn(colours = viridis(256, option = "A", begin = 0.125, end = 0.85)) +
  guides(fill="none") +
  labs(x="Frequency", y=NULL, title="Most frequent Horror film production companies",
       caption="Source: The Movie Database (TMDB)") +
  theme_dark() +
  theme(plot.title = element_text(colour = "gray90", family="blackHans", hjust=0.5, 
                          size=12))

showtext_opts(dpi=100) 
ggsave("Plots/horror_prodComp_freq.png", units="in", width=7, height=10, dpi=600, type="cairo-png")

## Companies and distributions

mostCommon_50 <- prodComp |> 
  count(name) |> 
  arrange(-n) |> 
  slice_max(n, n=10)

prodComp |> filter(name %in% mostCommon_50$name) |> 
  ggplot(aes(x=vote_average, fill=name, y=name, color=name)) +
  geom_density_ridges(alpha=.90, size=1, quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_d(option = "A") +
  scale_color_manual(values=c("grey30", "grey20", rep("black", 8))) +
  guides(fill="none", color="none") +
  labs(x="Movie score distribution", y=NULL, 
       caption="Source: The Movie Database (TMDB)", title="Movie votes distribution across Production Companies") +
  theme_dark() +
  theme(plot.title = element_text(colour = "gray90", family="blackHans", hjust=0.5, 
                                  size=12))

showtext_opts(dpi=800) 
ggsave("Plots/horror_prodComp_dist_score.png", units="in", width=14, height=10, dpi=600, type="cairo-png")

## Modelling budget ~ vote_average

model_linear <- lm(vote_average ~ budget, data=horror)
summary(model_linear)

ggplot(horror, aes(x=budget, y=vote_average)) +
  geom_point(alpha=.25) +
  geom_smooth(method="lm") 

horror_spline <- horror |> select(budget, vote_average)

dd <- datadist(horror_spline)
options(datadist="dd")

model_spline <- ols(vote_average ~ rcs(budget, 3), data=horror_spline)
summary(model_spline)
model_knots <- tibble(knots=as.numeric(unlist(stringr::str_split(specs(model_spline)$how.modeled[2], " "))[2:4]),
                      y=c(10, 9.5, 9.5))

horror_spline$fit_spline = predict(model_spline)
horror_spline$conf.high_spline = predict(model_spline, newdata=horror_spline, conf.int=0.95)$upper
horror_spline$conf.low_spline = predict(model_spline, newdata=horror_spline, conf.int=0.95)$lower
horror_spline$fit_lm = predict(model_linear, interval="confidence", level = 0.95)[,1]
horror_spline$conf.high_lm = predict(model_linear, interval="confidence", level = 0.95)[,2]
horror_spline$conf.low_lm = predict(model_linear, interval="confidence", level = 0.95)[,3]

horror_model_long <- horror_spline |> 
  mutate(row = row_number()) |> 
  pivot_longer(cols=c(fit_spline:conf.low_lm),
               names_to = c("stat", "model"),
               names_sep = "_") |> 
  pivot_wider(names_from=stat, values_from=value)

model_comp <- performance::compare_performance(model_linear, model_spline) |> 
  select(Model, AIC, BIC, R2, RMSE) |> 
  mutate(Model = case_when(Model == "lm" ~ "Linear model",
                           TRUE ~ "Spline model"))

ggplot(horror_model_long, aes(x=budget, y=vote_average)) +
  geom_point(mapping = aes(budget, vote_average), alpha=.25, color="grey70") +
  geom_line(mapping = aes(budget, fit, color=model, group=model), linewidth=1) +
  geom_ribbon(aes(y=fit, ymin=conf.low, ymax=conf.high, fill=model), 
              alpha=0.2) +
  geom_vline(xintercept=model_knots$knots, linetype="dashed", color="purple") +
  geom_text(data=model_knots, aes(y=y, x=knots, 
                                  label=ifelse(knots < 50000, paste0("$",scales::comma(knots)), paste0("$",knots*1e-6,"m"))), 
            color="grey90", hjust=-0.1) +
  annotate(geom="table", x=1e+8, y=0, label=model_comp, size=6) +
  labs(x="Movie budget", y="Average vote score", title="Association between movie budget and average score",
       caption="Source: The Movie Database (TMDB) 
       Knot position shown by purple dashed lines
       x axis truncated at $100m") +
  scale_color_manual(values=c("darkred", "orange"), labels=c("Linear model", "Spline model")) +
  scale_fill_manual(values=c("darkred", "orange"), labels=c("Linear model", "Spline model")) +
  scale_x_continuous(breaks=seq(0, 200000000, 25000000), limits=c(0, 100000000), 
                     labels = scales::label_number(prefix = "$", suffix = "m", scale = 1e-6, big.mark = ",")) +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  theme_dark() +
  theme(plot.title = element_text(family="blackHans", hjust=0.5),
        legend.position = c(0.875, 0.25),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=14))

showtext_opts(dpi=600) 
ggsave("Plots/horror_budget_vote_models.png", units="in", width=14, height=10, dpi=600, type="cairo-png")

