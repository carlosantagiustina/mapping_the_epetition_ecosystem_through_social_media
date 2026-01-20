#Author: Carlo Santagiustina (carlo.santagiustina@sciencespo.fr & carlo.santagiustina@inria.fr)
#Date (last update): 20/01/2026
#Description: this R script contains code for reproducing some of the figures and tables of the paper by C. Santagiustina and P. Ramaciotti, titled “Mapping the e‑petition ecosystem through social media: mobilization in the EU across issues and ideologies”, published in Journal of Quantitative Description: Digital Media (2026). DOI: 10.51685/jqd.2026.001.

#### Libraries ####

#All the following libraries must be installed before running the code
require(tidyverse)
require(quanteda)
require(ggplot2)
require(igraph)
require(visNetwork)
require(tidygraph)
require(ggraph)
require(ggrepel)
require(scales)
require(psych)
require(ComplexHeatmap)
require(circlize)
require(dendsort)

#the following RDS files must be in the same folder of the R script
# 1. perplexity_results_lang.RDS
# 2. users_by_domain_binary.RDS
# 3. x_count_urlmentions_by_domain.RDS
# 4. x_users_cat.RDS
# 5. top_topics.RDS
# 6. x_data_classified_agg_wide.RDS


####Figure 6 ####
#Figure 6  :  Scatter plot of the Perplexity of the distribution of URLs’ counts (log), considering all URLs referring to a specific domain, as a function of the number of distinct URLs referring to that domain.  Circle colour represents the domain’s language. Circle size represents the number of posts containing an URL referring to that domain.


  perplexity_results_lang=readRDS("perplexity_results_lang.RDS")



  q= ggplot(perplexity_results_lang, aes(x = log(n_urls), y = log(perplexity),colour = lang,fill=lang)) +
    # geom_smooth(data =perplexity_results_lang,  aes(x = log(n_urls), y = log(perplexity)), method='lm', formula= y~x,alpha = 0.5,colour = "gray") +
    geom_abline(slope=1, intercept = 0,alpha = 0.5) +
    geom_point(alpha = 0.7,aes(x = log(n_urls), y = log(perplexity), size = n_domain, color = lang)) +
    theme_minimal() +
    ggrepel::geom_label_repel(      aes(label = domain),
                                    colour="black",
                                    size=3,
                                    data = subset(perplexity_results_lang, n_domain > 10),
                                    force_pull = 0.01,
                                    box.padding = 0.01,
                                    max.overlaps = 50,
                                    point.padding = 0.3,
                                    min.segment.length=0,
                                    #arrow= arrow(length = unit(0.15, "inches"),  ends = "last", type = "open"),
                                    force = 20,
                                    alpha = 0.5,
                                    label.size = 0.25,
                                    label.r = unit(0.2, "lines"),
                                    direction = "y",
                                    angle = 0,
                                    max.time = 20,
                                    max.iter = 1000000,
                                    hjust = "center",          # left-align labels
                                    segment.alpha = 0.5,
                                    nudge_x = 0,
                                    nudge_y = 0,
    ) +
    #theme(legend.position = "lower")+
    labs(
      #title = "Scatterplot: Perplexity vs Number of URLs",
      x = "(log) N. distinct URLs",
      y = "(log) Perplexity of URLs' counts distribution",
      color = "Domain\n language:",
      size = "N. posts\n by domain:"
    ) +
    xlim(0, 11)+
    ylim(0, 11) +
    guides(fill = "none")  # Remove the fill legend

  ggsave(
    filename = "Fig6.png",     # File name
    plot = q,                  # The plot object (replace 'p' with your plot)
    width = 10,                 # Width of the image in inches
    height = 9,                # Height of the image in inches
    dpi = 300,                 # Resolution in DPI (300 is standard for publication)
    units = "in"               # Units for width and height
  )


### Multiplatform  activity - Figure 5###
  #Figure 5: Number of users by platform(s) community. A user is considered part of a petitioning platform(s)’ community if they have published at least one post containing a URL linking to that platform(s)’ domain(s). The vertical histogram on the left shows community size by platform, for platforms with at least 100 users, while the horizontal histogram on the top represents platforms’ community intersections of at least 100 users.
library(UpSetR)
library(ComplexUpset)
library(ComplexHeatmap)


users_by_domain_binary=readRDS("users_by_domain_binary.RDS")

data_for_upset=users_by_domain_binary[,(colnames(users_by_domain_binary) %in% {which(colSums(users_by_domain_binary)>=100) %>% names()})] %>% as.data.frame()
permutation_list= c(combn(colnames(data_for_upset), 2, simplify = FALSE),combn(colnames(data_for_upset), 3, simplify = FALSE))

# Generate combinations of length 3
comb = ComplexHeatmap::make_comb_mat(data_for_upset,min_set_size = 100, mode = "intersect")
comb=comb[ComplexHeatmap::comb_degree(comb) >= 2]
comb=comb[ComplexHeatmap::comb_size(comb) >= 100]

png(  filename = "Fig5.png",     # File name
      width = 10,                 # Width of the image in inches
      height = 10,                # Height of the image in inches
      res = 200,                 # Resolution in DPI (300 is standard for publication)
      units = "in"  )

ComplexHeatmap::UpSet(comb,comb_order = rev(order(ComplexHeatmap::comb_size(comb))),
                      top_annotation = ComplexHeatmap::upset_top_annotation(comb, add_numbers = TRUE),
                      right_annotation = ComplexHeatmap::upset_right_annotation(comb, add_numbers = TRUE),row_names_side = "left")

dev.off ()
#### Figure 4 ####
#Figure 4: Hierarchical circle plot of the number of posts in the X dataset referring to a specific petitioning platform, by language. Circle width equal to log(1+N), where N represents the number of posts referring to the platform (i.e., contain one or more URLs whose domain is that of the petitioning platform).
require(tidyverse)
require(igraph)
require(tidygraph)
require(ggraph)

count_urlmentions_by_domain=  readRDS("x_count_urlmentions_by_domain.RDS")


  #visualize as circle plot
  hierarchy_data <- count_urlmentions_by_domain %>%
    mutate(id = domain) %>%
    bind_rows(
      count_urlmentions_by_domain  %>%
        select(lang) %>%
        distinct() %>%
        mutate(id = lang, domain = NA, n_unique_tweets = NA) # Add parent nodes
    ) %>%
    bind_rows(data.frame(id = "root", domain = NA, n_unique_tweets = NA, lang = NA)) %>%
    mutate(parent = case_when(
      !is.na(lang) & is.na(domain) ~ "root",  # Categories connect to root
      !is.na(domain) ~ lang,  # Domains connect to their category
      TRUE ~ NA_character_
    ))

  hierarchy_data=hierarchy_data  %>%  mutate(size = hierarchy_data$n_unique_tweets)
  #hierarchy_data=hierarchy_data[hierarchy_data$id!="spanish",]
# create a dataframe with the vertices' attributes

  vertices <-hierarchy_data[,c("id","size")]  %>%
    distinct(id, size)
  vertices$counts= vertices$size
  vertices$size[is.na(vertices$size)]=0
  vertices$size=log(as.numeric(vertices$size+1))
  vertices$label=vertices$id
  vertices$label[vertices$size==0]=""
  vertices$label_cat=vertices$id
  vertices$label_cat[vertices$size!=0]=""
 #vertices$label_cat[vertices$size!=0 | !(vertices$id %in% c("france","italy","germany","poland","netherlands","spain","belgium","romania","multiple EU lang"))]=NA
  graph <- graph_from_data_frame(hierarchy_data[-nrow(hierarchy_data) ,c("parent","id")], vertices = vertices,directed = T)


c=  ggraph(graph, layout = "circlepack", weight = size) +
    geom_node_circle(aes(fill =depth)) +
    geom_text(aes(x = x, y = y, label = ifelse(is.na(counts)|label_cat=="root" ,"", paste0(label,"\n",counts)),size=(size/5)),size.unit = "pt") +
    scale_size(range = c(5, 15)) +
   ggrepel::geom_text_repel(aes(x = x, y = y, label =   ifelse(label_cat=="root" |label_cat=="","",label_cat),size=10),colour="white",alpha=0.5) +
  coord_fixed() +
    theme_void() +
    theme(legend.position = "none")

  ggsave(
    filename = "Fig4.png",     # File name
    plot = c,                  # The plot object (replace 'p' with your plot)
    width = 10,                 # Width of the image in inches
    height = 10,                # Height of the image in inches
    dpi = 300,                 # Resolution in DPI (300 is standard for publication)
    units = "in"               # Units for width and height
  )

####Table 2 ####
#  Table 2: Number and share of users by user category and language. To distinguish between Hit-and-run activists and New lobbyists, we define a threshold of one year between the first and last posts on a specific issue: users who exceed this threshold are categorized as New lobbyists. The Activism consumer category includes all users that published at least two posts and whose dominant (i.e., more probable) posts’ issue is not always the same. All other criteria are specified in Table 1.
users_cat=readRDS("x_users_cat.RDS")
table(users_cat$lang)
categories=table(users_cat$lang,users_cat$category ) %>% t()
categories
col_sums <- colSums(categories)

# Divide each element by its corresponding column sum
normalized_mat <- sweep(categories, 2, col_sums, FUN = "/")
print(normalized_mat)

users_cat=users_cat %>% mutate(long_activity=time_frame>0)

#Figure S.3 in supplement

s=  ggplot(users_cat, aes(x = n_url, y = top_topic_n)) +
  # geom_smooth(data =perplexity_results_lang,  aes(x = log(n_urls), y = log(perplexity)), method='lm', formula= y~x,alpha = 0.5,colour = "gray") +
    geom_jitter(alpha = 0.2,aes(color = time_frame),width = 0.14,height = 0.14) +
    scale_size_continuous(range = c(0.05, 0.05)) +
    scale_x_log10() +
    scale_y_log10() +
  theme_minimal() +
  #theme(legend.position = "lower")+
  labs(
    #title = "Scatterplot: Perplexity vs Number of URLs",
    x = " N. distinct petitions (i.e. unique URLs) mentioned by user",
    y = "N. distinct topics discussed by user",
    color = "User activity range (in days):",
  )+theme(legend.position = "bottom")


  ggsave(
    filename = "FigS3.png",     # File name
    plot = s,                  # The plot object (replace 'p' with your plot)
    width = 10,                 # Width of the image in inches
    height = 6,                # Height of the image in inches
    dpi = 300,                 # Resolution in DPI (300 is standard for publication)
    units = "in"               # Units for width and height
  )

####Figure 8 ####
 # Figure 8: Total reach (x-axis) and engagement (y-axis) for petition-related posts’ most probable issue, for the top 10 issues by reach for each language. Only issues for which the average political orientation of MP followers posting the content (represented by the color scale) can be computed are included. Size of issues is a monotonic nonlinear transformation of the number of occurrences of the issue as the most probable issue for a post, normalized by the frequency of the most probable issue for the specified language.  Spanish and Romanian were omitted due to an insufficient number of posts.

  top_topics= readRDS("top_topics.RDS")

  p <-  top_topics[!is.nan(top_topics$lrgen_avg) & !(top_topics$lang %in% c("spanish","polish")),]  %>% group_by(lang) %>%
    slice_max(order_by = total_reach, n = 10, with_ties = FALSE)  %>%
    ggplot(., aes(y = total_engagement+1, x= total_reach+1,  fill = lrgen_avg)) +
    geom_point(alpha = 0.5)+
    ggrepel::geom_label_repel( aes(label = top_topic_name,size=top_topic_n/max_n_occurrences),
                               force_pull = 0.01,
                               box.padding = 0.01,
                               max.overlaps = 50,
                               point.padding = 0.3,
                               min.segment.length=0,
                               #arrow= arrow(length = unit(0.15, "inches"),  ends = "last", type = "open"),
                               force = 20,
                               alpha = 0.5,
                               #label.size = 0.25,
                               label.r = unit(0.2, "lines"),
                               #size=2.25,
                               direction = "y",
                               angle = 0,
                               max.time = 20,
                               max.iter = 1000000,
                               hjust = "center",          # left-align labels
                               segment.alpha = 0.5,
                               nudge_x = 0,
                               nudge_y = 100
    )+
    scale_size_continuous(range = c(2, 3.5)) +
    scale_y_continuous(labels =  scales::unit_format(unit = "K", scale = 1e-3),
                       breaks = function(x) unique(c(10000, 50000,250000,500000,1000000, pretty(x))))+
    scale_x_continuous(labels =  scales::unit_format(unit = "K", scale = 1e-3),
                       breaks = function(x) unique(c(1000,2500, 5000, 10000, 25000, 50000,250000, 500000,1000000, pretty(x))))+
    scale_alpha_continuous(range = c(0.75, 1)) +
    guides(size = "none", alpha = "none") +
    coord_trans(y = "log")+
    coord_trans(x = "log")+
    scale_fill_gradientn(colors = c("red2", "gray40", "blue2"), values = c(0, 0.5, 1),
                         name = "Political Orientation:\n",  # Rename legend
                         breaks = c(0.5,5 ,9.5),  # Tick positions,
                         limits = c(0, 10),
                         labels = c("Far Left","Center","Far Right") )+
    #geom_hline(yintercept = 1, color = "black") +
    theme_bw()  +
    theme( axis.text.y = element_text(size = 6,angle = 45),axis.text.x = element_text(size = 6,angle = 45, hjust = 1),
           legend.position = "bottom")+
    labs(
      title = "",
      y = "Engagement",
      x = "Reach"
    ) +   facet_wrap(c("lang"),scales = "free",ncol=1,shrink=F) +
    guides(alpha = guide_legend(override.aes = list(alpha = 0.5)))

  ggsave(
    filename = "Fig8.png",     # File name
    plot = p,                  # The plot object (replace 'p' with your plot)
    width = 8,                 # Width of the image in inches
    height = 11,                # Height of the image in inches
    dpi = 300,                 # Resolution in DPI (300 is standard for publication)
    units = "in"               # Units for width and height
  )


####  Figure 7 ###
#Figure 7: This heatmap represents correlations among the propensities of issues discussed in posts related to petitioning. Propensities were inferred via the ManifestoBERTa model. Issues with an average propensity below 1% were excluded from the plot. Issues ordered using the Hclust algorithm, clustering results are shown in the subplot on the top of the figure. The circles subplot on the right side of the figure shows average propensities of issues (topic p.); circle size is proportional to the square root of average issue propensities across all posts in our dataset. Values on the diagonal of the correlation matrix (all equal to 1) were omitted from the plot.
#plot correlation matrix (with no threshold)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(dendsort)

set.seed(123)

doc_feature_matrix=readRDS("x_data_classified_agg_wide.RDS")
# Generate random counts and then normalize to relative frequencies

# Compute the average relative frequency for each feature across all documents.
feature_avg <- colMeans(doc_feature_matrix)
#feature_avg %>% sort(d=T)

# Set frequency threshold (features below this threshold will be removed)
threshold_freq <- 1 # equivalent to 1% (0.01)

# Identify features meeting the threshold
selected_features <- names(feature_avg)[feature_avg >= threshold_freq]

# Filter the document-feature matrix to retain only these features.
filtered_matrix <- doc_feature_matrix[, selected_features, drop = FALSE]

# --- Correlation Computation ---

# Compute the correlation matrix among the (remaining) features.
# Since columns represent features, 'cor' computes correlations across documents.
cor_matrix <- cor(filtered_matrix)
diag(cor_matrix)=0

#cor_mat_sig= psych::corr.test(x = doc_feature_matrix[, selected_features, drop = FALSE])
col_fun = colorRamp2(c(-max(abs(cor_matrix)), 0, max(abs(cor_matrix))), c("blueviolet","white","forestgreen" ))
ha1 = HeatmapAnnotation(foo1 = feature_avg[selected_features],
                        annotation_name_side = "left"
)
annotation <-rowAnnotation(
  "topic p."= anno_points(
    x=feature_avg[selected_features],
    pch = 21,  # Circle shape
    axis = T,
    axis_param = list(at = c(0, 10, 20), labels = c(" 0%", " 10%", " 20%"), labels_rot = 0),
    gp = gpar(fill = "black"),  # Circle fill color
    size = unit( sqrt(feature_avg[selected_features]), "mm"),
    border = F
  ),
  width = unit(1.7, "cm"),
  annotation_name_gp = gpar(fontsize = 8, fontface = "italic"),# Adjust annotation width
  #annotation_name_offset = unit(2, "mm"),  # Add space between annotation name and heatmap
  gap = unit(5, "mm")  # Add extra space between heatmap and annotation

)

row_dend = dendsort(hclust(dist(cor_matrix)))
col_dend = dendsort(hclust(dist(t(cor_matrix))))

png(  filename = "Fig7.png",     # File name
      width = 10,                 # Width of the image in inches
      height = 10,                # Height of the image in inches
      res = 200,                 # Resolution in DPI (300 is standard for publication)
      units = "in"  )
Heatmap(matrix = cor_matrix,col = col_fun, name = "Correlation:",
        column_names_gp = gpar(fontsize = 8),  # Column text size
        row_names_gp = gpar(fontsize = 8),  # Row text size
        column_dend_height = unit(2, "cm"),
        row_dend_width = unit(2, "cm"),
        heatmap_width = unit(18, "cm"),
        heatmap_height = unit(18, "cm"),
        row_names_side = "left",
        cluster_rows = row_dend,
        cluster_columns = col_dend,
        right_annotation = annotation ,
        show_row_dend = FALSE)
dev.off()



####  Figure S.5. ###
#Figure S.5: This plot shows the number of New Lobbyists on X by petitioning platform language as a function of the petitioning activity time-span threshold criteria
### Compute number of New lobbyist by threshold in days
x_users_cat <- readRDS("x_users_cat.RDS")
x_users_cat=x_users_cat[x_users_cat$top_topic_n==1 & x_users_cat$n >1,]

x_users_cat[x_users_cat$time_frame>365,] %>% nrow()
x_users_cat[x_users_cat$time_frame>21,] %>% nrow()

df <- x_users_cat %>%
  mutate(time_frame = as.integer(time_frame)) %>%
  filter(!is.na(time_frame) & time_frame >= 0)

# maximum time_frame to complete to (at least 365)
max_tf <- max(max(df$time_frame, na.rm = TRUE), 365L)

# 1) counts per lang x time_frame
counts_lang_tf <- df %>%
  group_by(lang, time_frame) %>%
  summarise(n = n(), .groups = "drop")

# 2) complete the grid per language (0..max_tf), then compute cumulative counts for time_frame >= t
counts_thresholds <- counts_lang_tf %>%
  group_by(lang) %>%
  complete(time_frame = 0:max_tf, fill = list(n = 0)) %>%    # ensure all time frames appear
  arrange(lang, desc(time_frame)) %>%                        # descending so cumsum gives >= t
  mutate(count_ge = cumsum(n)) %>%                           # count of rows with time_frame >= this value
  ungroup() %>%
  filter(time_frame >= 21 & time_frame <= 365) %>%            # keep requested thresholds
  rename(threshold = time_frame) %>%
  select(lang, threshold, count = count_ge)

# counts_thresholds now has one row per (lang, threshold) with count = #rows where time_frame >= threshold

# 3) Plot: one line per language (colour) — change to facet_wrap(~lang, scales = "free_y") if preferred
p <- ggplot(counts_thresholds, aes(x = threshold, y = count, colour = lang, group = lang)) +
  geom_line() +
  geom_point(size = 0.6, alpha = 0.6) +
  scale_x_continuous(breaks = c(21, 30, 60, 90, 180, 365)) +
  labs(x = "User activity time frame threshold (in days)", y = "Number of New lobbyists",
       title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "FigS5.png",     # File name
  plot = p,                  # The plot object (replace 'p' with your plot)
  width = 12,                 # Width of the image in inches
  height = 8,                # Height of the image in inches
  dpi = 300,                 # Resolution in DPI (300 is standard for publication)
  units = "in"               # Units for width and height
)

