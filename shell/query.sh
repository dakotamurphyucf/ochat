#!/bin/bash

# Get vector-db-folder options from environment variable
IFS=',' read -ra VECTOR_DB_FOLDERS <<< "$VECTOR_DB_FOLDERS"

# Prompt user for vector-db-folder selection
echo "Select a vector-db-folder:"
for i in "${!VECTOR_DB_FOLDERS[@]}"; do
  echo "$((i+1)). ${VECTOR_DB_FOLDERS[i]}"
done

FOLDER_SELECTION=$(sh ./input_with_history.sh "Enter the number of your selection: ")
VECTOR_DB_FOLDER="${VECTOR_DB_FOLDERS[$((FOLDER_SELECTION-1))]}"

# Prompt user for query text
QUERY_TEXT=$(sh ./input_with_history.sh "Enter the query text: ")

# Prompt user for num-results
NUM_RESULTS=$(sh ./input_with_history.sh "Enter the number of results: ")

# Prompt user for output file name
OUTPUT_FILE=$(sh ./input_with_history.sh "Enter the output file name: ")

# Run the command with user inputs
dune exe ./bin/main.exe -- query -vector-db-folder "$VECTOR_DB_FOLDER" -query-text "$QUERY_TEXT" -num-results $NUM_RESULTS >> "./vector_db_query_results/$OUTPUT_FILE"

# Open the output file in Visual Studio Code
code "./vector_db_query_results/$OUTPUT_FILE"
