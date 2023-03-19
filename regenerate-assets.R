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
  response <- POST(
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
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}


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

generate_image_dalle <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/images/generations", 
    add_headers(Authorization = paste("Bearer", openai_api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      prompt = prompt, 
      n = 1,
      size = "512x512", # "1024x1024"
      response_format = "url"
      )
    )
  print(response)
  content(response)$data[[1]]$url
}

generate_and_save_image <- function(prompt, path) {
  image_url <- generate_image(prompt)
  download.file(image_url, path)
  path
}

gen_text_func <- function(...) {
  function(cocktail) {
    prompt <- glue(..., .envir = cocktail, .sep = "\n")
    cat(glue("Generating text for: {prompt}"), "\n")
    generate_text(prompt)
  }
}

generators <- list( 
  image_path = \(cocktail) {
    path = file.path("cocktail-images",  paste0(cocktail$base_fname, ".jpeg"))
    prompt = glue("A well-lit photo of a {name} drink served in a {standard_drinkware} standing on a bartop, professional food photography, 15mm", .envir = cocktail)
    generate_and_save_image(prompt, path)
    path
  },
  hook = gen_text_func("Introduce the drink {name} in two short sentences."),
  extended_method = gen_text_func(
    "The drink {name} is made with the following ingredients:\n{as.yaml(cocktail$ingredients)}",
    "The drink {name} is often served in a {standard_drinkware} and made with the following method: {method}",
    "Describe the steps to make a {name} drink as a markdown formated list. Don't repeat the list of ingredients."
  ),
  tips = gen_text_func("Give a couple of tips for how to make the perfect {name} drink. Format the response as markdown. Start directly with the tips and skip the headline."),
  history = gen_text_func("Describe the origin and history of the drink {name}. Format the response as markdown."),
  fun_facts = gen_text_func("List five fun facts about the drink {name}. Format the response as markdown."),
  alcohol_free_alternative = gen_text_func("Describe an alcohol free alternative to the drink {name}")
)


# completed_cocktail_descriptions <- map(cocktail_descriptions, \(cocktail) {
#   if(! is.null(cocktail$image_path) && ! file.exists(cocktail$image_path)) {
#     cocktail["image_path"] <- list(NULL)
#   }
#   items_to_fill <- cocktail |>  keep(is.null) |> names()
#   for(item in items_to_fill) {
#     cocktail[item] <- generators[[item]](cocktail)
#   }
#   cocktail
# })

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




