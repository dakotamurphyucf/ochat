User
Generate 20 candiate prompts to generate the ideal concept of a rap album cover that will be fed to Dalle2 image generator. We want to capture the ideal concept of a rap album cover that is admired by the artists followers. Express the ideal concept of a post apopcalyptic world but still shows hope. Try different types of environments as the focol point of the the ideal concept of this album cover: zombies, badass people, robots, and celestials. Use different styles: figurative, surrealism, limpressionist, fantasy, and ancient. 

```
Try different descriptors and styles. I've found that the more realistic I ask DALL-E 2 to be, the less impressed I am with the results. I've had fun getting DALL-E 2 to mimic impressionist paintings, artists like Vermeer and
Don't expect amazing results the first time you try something. You'll often need to change up your prompt, try a few more variations, and otherwise tweak things to get something awesome.
But don't make your prompts too complicated. If you add in too many characters and details, DALL-E 2 won't quite know what to focus on, and it will just end up a m,
"A Vermeer-style painting of the Justice League teaming up with the Avengers to fight the Rugrats and Bowser from Super Mario" was fun to type, but the resulting images weren't very coherent.
Sometimes, less is more. Prompts can't be more than 1000
characters, in any case. And you can get amazing results from just a few emojis! But if you have a specific outcome in mind, then being specific in your prompt will help.
A simple adjective, like 'action photography' , already embodies
a lot of characteristics (about shutter speed, framing, lens choices, etc) that you might otherwise define separately.
There are 'fingers-crossed' prompt phrases, like AI-era prayers, hoped to nean 'make it really good!' , such as 4k, 8k, highquality, trending, award-winning, acclaimed, on artstation, etc. tested. But feel free to add then!
However, the precise impact of these has not been rigorously
In text AI models, simple prompt tweaks have created huge
boosts in performance: for instance, when a text generator is made to answer a math puzzle, starting with the words 'Let's think things through step-by-step' nakes it 4x more likely to get the right answer.
So no doubt, there are similar DALL-E hacks yet to be found
Adjectives can easily influence multiple factors, e.g: 'art deco' will influence the illustration style, but also the clothing and materials of the subject, unless otherwise defined. Years, decades and eras, like '1924' or 'late-90s' , can also have this
effect.
Even superficially specific prompts have more 'general' effects.
For instance, defining a camera or lens ('Signa 75mm') doesn't it more broadly alludes to 'the kind
just 'create that specific look'
of photo where the lens/camera appears in the description'
which tend to be professional and hence higher-quality. If a style is proving elusive, try 'doubling down with related terms (artists, years, media, movement) years, e.g: rather than simply by Picasso'
, try Cubist painting by Pablo Picasso, 1934, colourful, geometric work of Cubism, in the style of "Two
Detailed prompts are great if you know exactly what you're looking for and are trying to get a specific effect. but DALL-E also has a creative eye, and has studied over 400 million images. So there is nothing wrong with being vague, and seeing what happens! You can also use variations to create further riffs of your favourite output. Sometimes you'll end up on quite a journey!
Putting together this document has been quite an undertaking, as it aims to cover all 16777216*** possible DALL-E images (vs the 10** atoms in the universe) and all possible subjects of images, which is to say, all possible objects and materials in existence, depicted in all known methods.
For 2D art, we've gone a little deeper, looking at particular art styles and art movements. But if you want to create images of buildings, for example, then learning more about architectural periods, famous architects, and names of architectural details will be helpful to create specific outputs. Same for candlesticks, cartoons or candy wrappers.
DALL-E knows a lot about everything, so the deeper your
knowledge of the requisite jargon, the more detailed the
results.
You can name a specific film or TV show (ideally with the year in brackets) to 'steal the look', without needing to know the technical styles used. You can also name non-existent media with genre + year prompts, e.g: 'from action-adventure film "SHIVER ME TIMBERS!" Note: this prompt will also influence the background, costumes, hairstyles, and any other uncontrolled factors
```

I use the script below to run the curl commands, place each description in a prompts array so I can add it to the script.
```sh
#!/bin/bash
# Define an array of prompts
prompts=(
)
# Loop through the prompts and make API calls
for prompt in "${prompts [@]}"; do # Make the API call and store the JSON response in a variable
json_response=$(curl -s "https://api.openai.com/v1/images/generations" \
-H "Content-Type: application/json" \
-H "Authorization: Bearer $OPENAI_API_KEY" \
-d "{\"prompt\": \"$prompt\", \"n\": 5, \"size\": \"1024x1024\"}")
# Use ją to parse the JSON and get the URLS of the images image_urls=$(echo "$json_response" | jq -r '.data[].url')
# Loop through the image URLs and download each image using curl
for url in $image_urls; do
# Extract the filename from the URL
filename=$(basename "$url")
# Download the image using curl and save it with the extracted filename curl -s -Lo "../dalle/$filename" "$url"
done
done
```

prompts=("Post-apocalyptic rap album cover with hopeful zombies in a surrealistic style",
"Rap album cover in a post-apocalyptic world with badass survivors in a figurative style",
"Robotic post-apocalyptic rap album cover with a touch of hope in an impressionist style",
"Post-apocalyptic celestial beings on a rap album cover symbolizing hope in a fantasy style",
"Ancient-style rap album cover set in a post-apocalyptic world with hopeful zombies",
"Rap album cover featuring hopeful survivors in a post-apocalyptic world with a surrealistic touch",
"Post-apocalyptic rap album cover with robots and a sense of hope in a limpressionist style",
"Fantasy-style rap album cover with celestial beings in a post-apocalyptic world",
"Rap album cover with ancient warriors in a post-apocalyptic world full of hope",
"Surrealistic rap album cover with zombies in a hopeful post-apocalyptic setting",
"Post-apocalyptic rap album cover with badass people and robots in a figurative style",
"Hopeful celestial beings on a rap album cover in a post-apocalyptic impressionist world",
"Rap album cover with ancient heroes in a post-apocalyptic world and a touch of surrealism",
"Fantasy-style rap album cover with zombies and hope in a post-apocalyptic setting",
"Post-apocalyptic rap album cover with robots and ancient warriors in a limpressionist style",
"Rap album cover with celestial beings and badass survivors in a hopeful post-apocalyptic world",
"Surrealistic rap album cover with ancient heroes in a post-apocalyptic world",
"Post-apocalyptic rap album cover with hopeful robots in a fantasy style",
"Rap album cover with zombies and celestial beings in a post-apocalyptic impressionist world",
"Hopeful post-apocalyptic rap album cover with badass people in an ancient style")