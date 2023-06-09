This repo hosts two small scripts that uses ChatGPT and StableDiffusion to auto-generate the
cocktail website found over here:

[officialcocktails.com](https://officialcocktails.com/)

This was a quick experiment to try out auto-generating a website using (in 2023) modern AI tools, so the code is quick and dirty, and put here more for the record. But shortly, the recipe is:

1. **Take some cocktails data in a nice format**. For example [the IBA list of official cocktails as scraped from wikipedia](https://github.com/rasmusab/iba-cocktails).
2. **Use the given data to generate prompts that the AIs will expand into more text ("list fun facts about Cuba Libre") and images of the cocktails**. This is what `01_regenerate-assets.R` does. That script will create a cocktail-descriptions.yaml file if it doesn't already exist. This is a yaml file that contains all the drink info scraped from Wikipedia (from iba-cocktails-wiki.json) plus NULL fields for all the things we want to fill in, like the image, fun facts, etc. This script will then start filling in all the nulls. As the ChatGPT "hangs" regularly, I needed to rerun this script many times until cocktail-descriptions.yaml was completely filled in. If a generated description or images is wonky, just delete it and rerun this script and it will be regenerated.
3. **Package all the cocktail descriptions and images into a format that a site generator like [hugo](https://gohugo.io/) can render**. This is what `02_generate-website.R` does. That script takes the auto-generated cocktails descriptions and images from  `cocktail-descriptions.yaml` and pastes together one markdown file for each cocktail and put those files in the correct place in the hugo site located in `officialcocktails.com`
4. **Find a decent theme for hugo** (say [hugo-creative-portfolio-theme](https://github.com/kishaningithub/hugo-creative-portfolio-theme)) and hack on it until it does what you want.
5. **Generate the site with hugo**.
6. Success?

Some more details on my blog: https://www.sumsar.net/blog/ai-generated-cocktail-site/
