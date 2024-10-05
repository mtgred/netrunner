#!/bin/bash
set -e

# Check if MONGO_CONNECTION_URI is set
if [ -z "$MONGO_CONNECTION_URI" ]; then
    echo "Error: MONGO_CONNECTION_URI environment variable is not set"
    exit 1
fi

# Jnet needs the database name in the URI. Mongodump must not have a database
# name in the URI when doing namespace transformation. Thus, we must parse the
# provided URI and reconstruct the parts we need.
MONGO_RESTORE_URI=$(echo $MONGO_CONNECTION_URI | sed -E 's/^(.*\/)[^?]*(\?.*)$/\1\2/')
MONGO_RESTORE_DB=$(echo $MONGO_CONNECTION_URI | sed -E 's/^.*\/([^?]*)\?.*/\1/')

if [[ -z "$MONGO_RESTORE_URI" || -z "$MONGO_RESTORE_DB" ]]; then
    echo "Error: MONGO_CONNECTION_URI environment variable could not be parsed"
    exit 1
fi

# For local testing, mongorestore might be somewhere else in the path.  In the
# generated image we expect it to be in 'mongotools'.
if [ -d "mongotools" ]; then
    export PATH="$(pwd)/mongotools/bin:$PATH"
fi

# Update the configuration file with the MongoDB connection URI
mongorestore --uri="${MONGO_RESTORE_URI}" --archive="$MONGO_ARCHIVE" --nsFrom='jnet-tmpl.*' --nsTo="${MONGO_RESTORE_DB}.*"

echo "Database initialization completed successfully."
