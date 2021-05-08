###no_function()

tinyTools::setwd_project()
setwd("other_files/manuscript/")
rm(list=ls())
dir()

sheet_name =
  readxl::excel_sheets("Compare_all.xlsx")

sheet_name2 =
  readxl::excel_sheets("Compare_all_important.xlsx")

metid_only = readxl::read_xlsx("Compare_all_important.xlsx",
                               sheet = sheet_name2[5])

####get the chemical class of all the metID only metabolites
library(tinyTools)
library(metflow2)

hmdb_id1 =
  metid_only[,c("Annotation", "HMDB")] %>%
  dplyr::distinct(Annotation,.keep_all = TRUE)

kegg_id1 =
  metid_only[,c("Annotation", "KEGG")] %>%
  dplyr::distinct(Annotation,.keep_all = TRUE)

unique_compound_name =
  metid_only[,c("Annotation", "KEGG")] %>% 
  dplyr::distinct(Annotation,.keep_all = TRUE) %>% 
  pull(Annotation)
  

# hmdb_id2 <-
#   t(hmdb_id1) %>%
#   as.data.frame() %>%
#   purrr::map(function(x){
#     x[1] = stringr::str_replace(x[1], "\\([0-9]{1,2}\\)$", "")
#     if(x[2] == 'NA'){
#       metflow2::transID(query = x[1], from = "Chemical name", to = "Human Metabolome Database", top = 1)$`Human Metabolome Database`
#     }else{
#       if(nchar(x[2]) == 9){
#         x[2] = stringr::str_replace(x[2], "HMDB", "HMDB00")
#       }
#       x[2]
#     }
#   })
# 
# save(hmdb_id2, file = "hmdb_id2")
load("hmdb_id2")

hmdb_id2 =
  hmdb_id2 %>%
  unlist %>%
  unname()

# kegg_id2 <-
#   t(kegg_id1) %>%
#   as.data.frame() %>%
#   purrr::map(function(x){
#     cat(x[1], " ")
#     x[is.na(x)] = "NA"
#     x[1] = stringr::str_replace(x[1], "\\([0-9]{1,2}\\)$", "")
#     if(x[2] == 'NA'){
#       metflow2::transID(query = x[1], from = "Chemical name", to = "KEGG", top = 1)$KEGG
#     }else{
#       x[2]
#     }
#   })
# 
# save(kegg_id2, file = "kegg_id2")
load("kegg_id2")

kegg_id2 =
  kegg_id2 %>%
  unlist %>%
  unname()

name1 = hmdb_id2[grep("C", hmdb_id2)]
name2 = kegg_id2[grep("HMDB", kegg_id2)]

hmdb_id2[grep("C", hmdb_id2)] = name2
kegg_id2[grep("HMDB", kegg_id2)] = name1

inchikey1 <-
  hmdb_id2 %>%
  pbapply::pblapply(function(x){
    if(is.na(x)){
      return(NA)
    }
    metflow2::transID(query = x, from = "Human Metabolome Database", to = "InChIKey", top = 1)
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

inchikey2 <-
  kegg_id2 %>%
  pbapply::pblapply(function(x){
    if(is.na(x)){
      return(NA)
    }
    metflow2::transID(query = x, from = "KEGG", to = "InChIKey", top = 1)
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame()

dim(inchikey1)
dim(inchikey2)

inchikey =
  data.frame(inchikey1$InChIKey, inchikey2$InChIKey) %>%
  apply(1, function(x){
    if(all(is.na(x))){
      return(NA)
    }
    x = x[!is.na(x)]
    x[1]
  })

metabolite_class <- vector(mode = "list", length = length(inchikey))

# metabolite_class <-
#   pbapply::pblapply(inchikey, function(x){
#     if(is.na(x)){
#       return(NA)
#     }
#     Sys.sleep(time = 5)
#     result <- metflow2::get_metclass(inchikey = x, sleep = 5)
#   })
# save(metabolite_class, file = "metabolite_class")

load("metabolite_class")

idx <-
  lapply(metabolite_class, class) %>% unlist() %>%
  `==`("logical") %>%
  which()

inchikey[idx]


super_class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Superclass") %>%
    pull(value)
}) %>%
  unlist()


class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Class") %>%
    pull(value)
}) %>%
  unlist()

sub_class <- lapply(metabolite_class, function(x){
  if(is.na(x)) return(NA)
  x@classification_info %>%
    dplyr::filter(name == "Subclass") %>%
    pull(value)
}) %>%
  unlist()

sub_class[which(sub_class == "Not available")] <- NA




metabolite_tags <- data.frame(unique_compound_name,
                              super_class,
                              class,
                              sub_class,
                              stringsAsFactors = FALSE)

library(openxlsx)

openxlsx::write.xlsx(metabolite_tags, file = "metabolite_class.xlsx", asTable = TRUE)

###pie chart
library(ggpol)

#draw a parliament diagram
metabolite_tags$super_class[is.na(metabolite_tags$super_class)] = "Unknown"
temp_data =
  metabolite_tags %>%
  dplyr::filter(!is.na(super_class)) %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(super_class = factor(super_class, levels = super_class))


class_color = c(ggsci::pal_aaas()(n=10),
                gplots::col2hex(cname = "skyblue"))
# names(class_color) = temp_data$super_class

dim(metabolite_tags)
sum(is.na(metabolite_tags$class))
sum(!is.na(metabolite_tags$class))
plot <-
  temp_data %>%
  ggplot() +
  geom_parliament(aes(seats = n, fill =  super_class),
                  color = "white") +
  scale_fill_manual(values = class_color,
                    labels = temp_data$super_class) +
  coord_fixed() +
  theme_void() +
  labs(title  = "479 metabolites",
       subtitle="The number and distribution of class of 645 metabolites")+
  theme(title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(vjust = -3,hjust = 0.9),
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.spacing.y = unit(0.1,"cm"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.key.size = unit(0.8, 'lines'),
        legend.text = element_text(margin = margin(r = 1, unit = 'cm')),
        legend.text.align = 0)+
  annotate("text", x = 0, y = 0.4,
           label = "Metabolite super lass :\n 216 (45.1%) unknown \n 263 (54.9%) known",
           colour = "black",size = 6)+
  guides(fill=guide_legend(nrow = 4,
                           byrow=TRUE,
                           reverse = TRUE,
                           title=NULL))

plot

ggsave(plot,
       filename = "metid_only_class1.pdf", width = 10, height = 7)

names(class_color) = temp_data$super_class

plot =
  metabolite_tags %>%
  dplyr::filter(super_class != "Unknown") %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(super_class = factor(super_class, levels = super_class)) %>%
  ggplot() +
  geom_bar(aes(x = 1, y = n, fill = super_class),
           color = "white",
           stat = "identity", show.legend = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = class_color)
plot
ggsave(plot, filename = "metid_only_class2.pdf", width = 7, height = 7)


metabolite_tags %>% 
  dplyr::filter(!is.na(super_class)) %>% 
  dplyr::filter(super_class != "Unknown") %>% 
  dplyr::group_by(super_class) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(freq = n/sum(n)) %>% 
  dplyr::arrange(n)
