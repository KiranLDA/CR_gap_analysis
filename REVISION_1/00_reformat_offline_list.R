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


##########################################################################

library(stringi)
library(dplyr)
library(tidyr)
library(stringr)


df <- read.csv2(paste0(basepath, "revision_1/Offline_list_25-09-2025_at_12-20-58_UTF8.csv" ),
                sep = ",", quote = "\"",
                dec = ".",header=TRUE,fill=TRUE, encoding = "UTF-8")


x <- df$RECSUMMARY
x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")

df_wide <- tibble(row_id = seq_along(x), raw = x) %>%
  mutate(
    # extract only "KEY: value" chunks; keys = all caps + symbols
    parts = str_extract_all(raw, "[A-Z0-9_/()#&\\- ]+: [^;]*")
  ) %>%
  dplyr::select(-raw) %>%
  unnest(parts) %>%
  separate(parts, into = c("key", "value"), sep = ":", extra = "merge", fill = "right") %>%
  mutate(
    key   = str_trim(key),
    value = str_trim(value),
    key   = str_replace_all(key, "[^A-Za-z0-9]+", "_"),
    key   = if_else(key == "", "UNKNOWN_KEY", key)
  ) %>%
  group_by(row_id, key) %>%
  summarise(value = paste(value, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = key, values_from = value)

cbind(df$ID, df_wide)


View(df_wide)


write.csv(df_wide, paste0(basepath,"revision_1/Offline_list_25-09-2025_at_12-20-58_reformatted.csv"), row.names = F)



##########################

library(dplyr)
library(tidyr)
library(stringr)

# read
df <- read.csv2(
  paste0(basepath, "revision_1/Offline_list_25-09-2025_at_12-20-58_UTF8.csv"),
  sep = ",", quote = "\"", dec = ".", header = TRUE, fill = TRUE, encoding = "UTF-8"
)

# clean RECSUMMARY encoding
x <- df$RECSUMMARY
x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")

df_wide <- tibble(ID = df$ID, raw = x) %>%
  mutate(
    # extract only "KEY: value" chunks where KEY is uppercase-like
    parts = str_extract_all(raw, "[A-Z0-9_/()#&\\- ]+: [^;]*")
  ) %>%
  dplyr::select(-raw) %>%
  unnest(parts, keep_empty = TRUE) %>%  # keep rows even if no parts found
  separate(parts, into = c("key", "value"), sep = ":", extra = "merge", fill = "right") %>%
  mutate(
    key   = str_trim(key),
    value = str_trim(value),
    key   = str_replace_all(key, "[^A-Za-z0-9]+", "_"),
    key   = if_else(key == "" | is.na(key), "UNKNOWN_KEY", key)
  ) %>%
  group_by(ID, key) %>%
  summarise(value = paste(value, collapse = "; "), .groups = "drop") %>%
  pivot_wider(names_from = key, values_from = value)


View(df_wide)
