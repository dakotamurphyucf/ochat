#!/bin/bash

# Define an array of prompts
prompts=(
 "Post-apocalyptic rap album cover with hopeful zombies in a surrealistic style",
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

# Loop through the prompts and make API calls
index=0
for prompt in "${prompts[@]}"; do
  # Make the API call and store the JSON response in a variable
  json_response=$(curl -s "https://api.openai.com/v1/images/generations" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $OPENAI_API_KEY" \
    -d "{\"prompt\": \"$prompt\", \"n\": 1, \"size\": \"1024x1024\"}")
  
  # Use jq to parse the JSON and get the URLs of the images
  image_urls=$(echo "$json_response" | jq -r '.data[].url')

  # Loop through the image URLs and download each image using curl
  for url in $image_urls; do

    echo "$index,$prompt" >> "./dalle/10/prompts.txt"

    # Download the image using curl and save it with the extracted filename
    curl -s -L -o "./dalle/10/$index.png" "$url"
    index=$((index + 1))
  done
  sleep 1
done
