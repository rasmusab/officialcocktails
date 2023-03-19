library(tidyverse)
library(yaml)
library(glue)

cocktail_descriptions <- read_yaml("cocktail-descriptions.yaml")

posts_path <- file.path("officialcocktails.com", "content", "cocktails")
unlink(posts_path, recursive = TRUE)
dir.create(posts_path)

# Moving the images to the right location
unlink("officialcocktails.com/static/cocktail-images", recursive = TRUE)
file.copy("cocktail-images/", "officialcocktails.com/static/", recursive = TRUE)

cocktail_page_template <- r"-(
---
title: "{name}"
draft: false
image: "/{image_path}"
showonlyimage: false
weight: 1
---

{hook}

<!--more-->

**{name} recipe**

{as.yaml(ingredients)}

{method}

![](/{image_path})


## Directions for how to make a {name}

{extended_method}

## Tips for how to make the perfect {name}

{tips}

## Alcohol-free alternative to a {name}

{alcohol_free_alternative}

## {name} fun facts

{fun_facts}

)-"

walk(cocktail_descriptions, \(cocktail) {
  page <- glue(cocktail_page_template, .envir = cocktail)
  path <- file.path(posts_path, paste0(cocktail$base_fname, ".md"))
  write_file(page, path)
})


