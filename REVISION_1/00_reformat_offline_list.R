library(stringi)
library(dplyr)
library(tidyr)
library(stringr)

df <- read.csv2(paste0(basepath, "revision_1/Offline_list_25-09-2025_at_12-20-58_UTF8.csv" ),
                          sep = ",", quote = "\"",
                          dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


# Make sure x is UTF-8 safe
x <- df$RECSUMMARY
x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")  # replace invalid bytes with space

df_wide <- tibble(row_id = seq_along(x), raw = x) %>%
  mutate(parts = str_split(raw, ";")) %>%
  dplyr::select(-raw) %>%
  unnest(parts) %>%
  mutate(parts = str_trim(parts)) %>%
  filter(parts != "") %>%
  separate(parts, into = c("key", "value"), sep = ":", extra = "merge", fill = "right") %>%
  mutate(
    key   = str_trim(key),
    value = str_trim(value),
    key   = str_replace_all(key, "[^A-Za-z0-9]+", "_"),
    key   = if_else(key == "", "UNKNOWN_KEY", key)  # fix empty keys
  ) %>%
  group_by(row_id, key) %>%
  summarise(value = paste(value, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = key, values_from = value)

View(df_wide)


write.csv(df_wide, paste0(basepath,"revision_1/Offline_list_25-09-2025_at_12-20-58_reformatted.csv"), row.names = F)

