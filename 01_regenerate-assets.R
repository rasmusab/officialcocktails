# This script will create a cocktail-descriptions.yaml file if it doesn't already exist.
# This is a yaml file that contains all the drink info scraped from wikipedia
# (from iba-cocktails-wiki.json) plus NULL fields for all the things we want to 
# fill in, like the image, fun facts, etc. This script will then start filling in all 
# the nulls. As the ChatGPT "hangs" regularly, I needed to rerun this script many times
# until cocktail-descriptions.yaml was completely filled in.
#
# If a generated description or images is wonky, just delete it and rerun this script
# and it will be regenerated.

library(tidyverse)
library(yaml)
library(jsonlite)
library(httr)
library(glue)

openai_api_key <- Sys.getenv("OPENAI_API_KEY")
evoke_api_key <- Sys.getenv("EVOKE_API_KEY")

# Create the cocktail-descriptions.yaml if it doesn't already exist
cocktails <- read_json("iba-cocktails-wiki.json")
if(!file.exists("cocktail-descriptions.yaml")) {
  placeholder_cocktail_descriptions <- map(cocktails, \(cocktail) {
    base_fname = fname <- cocktail$name |>
        tolower() |> 
        str_replace_all("[^a-z0-9]", "_")
    image_path <- file.path("cocktail-images",  paste0(base_fname, ".jpeg"))
    c(cocktail, list(
      base_fname = base_fname,
      image_path = image_path,
      hook = NULL,
      extended_method = NULL,
      tips = NULL,
      fun_facts = NULL,
      alcohol_free_alternative = NULL
    ))
  }) 
  write_yaml(placeholder_cocktail_descriptions, "cocktail-descriptions.yaml")  
}
cocktail_descriptions <- read_yaml("cocktail-descriptions.yaml")
dir.create("cocktail-images", showWarnings = FALSE)


# Calls the ChatGPT API with the given prompt and returns the answer
generate_text <- function(prompt) {
  response <- RETRY(
    "POST",
    times = 3,
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", openai_api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    ),
    timeout(seconds = 30)
  )
  str_trim(content(response)$choices[[1]]$message$content)
}

# Calls a Stable Diffusion API to get the an image. Why not DALL-E?
# Well, some drinks have "shocking" names (like Corpse Reviver), and
# DALL-E content filter refuses to make any images of those drinks.
generate_image <- function(prompt) {
    generate_response <- POST(
    url = "https://xarrreg662.execute-api.us-east-1.amazonaws.com/sdAddEle", 
    content_type_json(),
    encode = "json",
    body = list(
      token = evoke_api_key,
      prompt = prompt
    )
  )
  images_UUID <- content(generate_response)$body$UUID
  retrieve_response <- POST(
    url = "https://qrlh34e4y6.execute-api.us-east-1.amazonaws.com/sdCheckEle", 
    content_type_json(),
    encode = "json",
    body = list(
      token = evoke_api_key,
      UUID = images_UUID
    )
  )
  image_url <- content(retrieve_response)$body
  image_url
}


generate_and_save_image <- function(prompt, path) {
  image_url <- generate_image(prompt)
  download.file(image_url, path)
  path
}

# A function that takes a prompt template and generates another function which 
# takes a cocktail, as from cocktail_descriptions, completes the template and returns
# a ChatGPT answer.
gen_text_func <- function(...) {
  function(cocktail) {
    prompt <- glue(..., .envir = cocktail, .sep = "\n")
    cat(glue("Generating text for: {prompt}"), "\n")
    generate_text(prompt)
  }
}

# A list of generator functions that can generate text/images for the corresponding
# field, if missing.
generators <- list( 
  image_path = \(cocktail) {
    path = file.path("cocktail-images",  paste0(cocktail$base_fname, ".jpeg"))
    prompt = glue("A well-lit photo of a {name} cocktail served in a {standard_drinkware} standing on a bartop, professional food photography, 15mm", .envir = cocktail)
    cat(glue("Generating image for: {prompt}"), "\n")
    generate_and_save_image(prompt, path)
    path
  },
  hook = gen_text_func("Introduce the cocktail {name} in two short sentences."),
  extended_method = gen_text_func(
    "The drink {name} is made with the following ingredients:\n{as.yaml(cocktail$ingredients)}",
    "The drink {name} is often served in a {standard_drinkware} and made with the following method: {method}",
    "Describe the steps to make a {name} drink as a markdown formated list. Don't repeat the list of ingredients."
  ),
  tips = gen_text_func("Give a couple of tips for how to make the perfect {name} cocktail Format the response as markdown. Start directly with the tips and skip the headline."),
  fun_facts = gen_text_func("List five fun facts about the cocktail {name}. Format the response as markdown."),
  alcohol_free_alternative = gen_text_func("Describe an alcohol free alternative to the cocktail {name}")
)

# Looping through the cocktail description and filling in all the blanks.
# As both the text and image generating APIs errors out now and then
# this loop saves every single response directly to cocktail-descriptions.yaml
for(cocktail_i in seq_along(cocktail_descriptions)) {
  cocktail <- cocktail_descriptions[[cocktail_i]]
  if(! is.null(cocktail$image_path) && ! file.exists(cocktail$image_path)) {
    cocktail["image_path"] <- list(NULL)
  }
  items_to_fill <- cocktail |>  keep(is.null) |> names()
  for(item in items_to_fill) {
    cocktail[[item]] <- generators[[item]](cocktail)
    cocktail_descriptions[[cocktail_i]] <- cocktail
    write_yaml(cocktail_descriptions, "cocktail-descriptions.yaml") 
  }
}
