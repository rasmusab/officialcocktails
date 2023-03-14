library(tidyverse)
library(yaml)
library(jsonlite)
library(httr)
library(glue)

openai_api_key <- Sys.getenv("OPENAI_API_KEY")

# Create the cocktail-descriptions.yaml if it doesn't already exist
cocktails <- read_json("iba-cocktails-wiki.json")[1:5]
if(!file.exists("cocktail-descriptions.yaml")) {
  placeholder_cocktail_descriptions <- map(cocktails, \(cocktail) {
    c(cocktail, list( 
      image_path = NULL,
      hook = NULL,
      extended_method = NULL,
      tips = NULL,
      history = NULL,
      fun_facts = NULL,
      alcohol_free_alternative = NULL
    ))
  }) 
  write_yaml(placeholder_cocktail_descriptions, "cocktail-descriptions.yaml")  
}
cocktail_descriptions <- read_yaml("cocktail-descriptions.yaml")
dir.create("cocktail_images", showWarnings = FALSE)


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
  response <- POST(
    url = "https://api.openai.com/v1/images/generations", 
    add_headers(Authorization = paste("Bearer", openai_api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      prompt = prompt, 
      n = 1,
      size = "256x256", # "1024x1024"
      response_format = "url"
      )
    )
  content(response)$data[[1]]$url
}

generate_and_save_image <- function(prompt, path) {
  image_url <- generate_image(prompt)
  download.file(image_url, path)
  path
}

image_url <- generate_image("A well-lit photo of a Cuba Libre drink served a Highball glass, trending on artstation")
browseURL(image_url)

generate_and_save_image("A colorful 80s manga cartoon of a Cuba Libre drink served in a Highball glass", "test.jpeg")

gen_text_func <- function(...) {
  function(cocktail) {
    prompt <- glue(..., .envir = cocktail, .sep = "\n")
    cat(glue("Generating text for: {prompt}"), "\n")
    generate_text(prompt)
  }
}

generators <- list( 
  image_path = \(cocktail) {
    fname <- cocktail$name |>
      tolower() |> 
      str_replace_all("[^a-z0-9]", "_") |> 
      paste0(".jpeg")
    path = file.path("cocktail_images", fname)
    prompt = glue("A well-lit photo of a {name} drink served in a {standard_drinkware}", .envir = cocktail)
    generate_and_save_image(prompt, path)
    path
  },
  hook = gen_text_func("Introduce the drink {name} in two short sentences."),
  extended_method = gen_text_func(
    "The drink {name} is made with the following ingredients:\n{as.yaml(cocktail$ingredients)}",
    "The drink {name} is often served in a {standard_drinkware} and made with the following method: {method}",
    "Describe how to make a {name} as a markdown formated list."
  ),
  tips = gen_text_func("Give a couple of tips for how to make the perfect {name} drink. Format the response as markdown."),
  history = gen_text_func("Describe the origin and history of the drink {name}."),
  fun_facts = gen_text_func("List some fun facts about the drink {name}"),
  alcohol_free_alternative = gen_text_func("Describe an alcohol free alternative to the drink {name}")
)


completed_cocktail_descriptions <- map(cocktail_descriptions, \(cocktail) {
  items_to_fill <- cocktail |>  keep(is.null) |> names()
  for(item in items_to_fill) {
    cocktail[item] <- generators[[item]](cocktail)
  }
  cocktail
})

write_yaml(completed_cocktail_descriptions, "cocktail-descriptions.yaml")  



