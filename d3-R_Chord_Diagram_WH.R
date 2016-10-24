# Mehaz: http://data-steve.github.io/d3-r-chord-diagram-of-white-house-petitions-data/
# The OpenData movement has the White House producing and releasing some novel datasets. One of them is the We The People petition site. I learned about this from Proofreader.com’s interesting python work using that data. From the petition site, you can see an interesting gallery of work done in different language and for different media/platform. One such example is yoni’s r pkg. In the spirit of the open data goodness, I thought I’d play around as well with this d3 chord diagram.

# Downloading data from github repo

# if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("mattflor/chorddiag")
pacman::p_load(dplyr, magrittr, ggplot2, tidyr, curl)

curl::curl_download(
  "https://github.com/yoni/r_we_the_people/blob/master/data/petitions.RData?raw=true"
  , destfile="~/R/devdat/d3-R_Chord_Diagram_White-House/petitions.RData")
load("petitions.RData")

# Cleaning / Set up
# recover tag names and ids
p <- petitions   # save some typing
ids_names <- rbind(    
  p[, c("issues1.id", "issues1.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues2.id", "issues2.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues3.id", "issues3.name")]%>% setNames(c("ids", "names"))
) %>%
  unique() %>% na.omit()


# get only petitions with multi-tags
tag_count <- p %>%              
  select(id, issues1.id, issues2.id, issues3.id) %>%
  tidyr::gather(order, cats, -id) %>%
  filter(!is.na(cats)) %>%
  mutate(order = tidyr::extract_numeric(order)) %>%
  left_join(ids_names, by=c("cats"="ids"))

xtab_tag <- tag_count %>%
  count(names) %>%
  arrange(desc(n))

# Adjacency Matrix
#Now we build the matrix by registering whether a tag shows up for a specific petition and then creating the adjacency matrix to represent co-occurences of tags, which is what we need for the chord diagram.

# list of tags
tags <- sort(unique(ids_names$names))

# matrix to hold counts
mat <- matrix(0,nrow=nrow(tag_count),ncol=length(tags))
colnames(mat) <- tags

# Chord Diagram and choices

# I’ll save you the suspense regarding what choices I had to make in order to get chorddiag to work for me.

# get columns with tags from dataframe
p_id_nam <- p %>%
  select(contains(".name")) %>%
  mutate(issues1.name= ifelse(is.na(issues1.name), issues.name, issues1.name)) %>%
  mutate_each(funs(ifelse(is.na(.), "", .)), starts_with("issues"))

# make matrix
for (i in seq_along(tags)) {
  for (j in c(1,2,3)){ # 1,2,3 are columns I want
    mat[,i] <- as.numeric(tags[i]==p_id_nam[[j]]) +  mat[,i]
    is.na(mat[,i]) <- 0
  }
}

adjmat <- t(mat) %*% mat

# set number of colors needed
colorCount <- length(tags)

# makes function to create palette
getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

# manage use of diagonal cells in adj_mat
remove_diags <- function(mat, rm.lower = TRUE, ...) {
  diag(mat) <- 0
  if (isTRUE(rm.lower)) mat[lower.tri(mat)] <- 0
  mat
}

# ## order plot layering by smallest to largest so larges are on top
ord <- order(rowSums(remove_diags(adjmat, FALSE)))

# Finally:
# with the diags means there's a return
chorddiag::chorddiag(adjmat[ord, ord], margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8  # have to shrink font for web viewing
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors = getPalette(colorCount)    
)