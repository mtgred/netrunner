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

# Update the database from the mongodump archive embedded in the docker image.
# The MONGO_ARCHIVE variable is the path the the archive file. Only certain collections
# will be replaced, representing cards, formats, etc. User data, decks, game logs, etc.
# will not be replaced. This needs to be run when https://github.com/NoahTheDuke/netrunner-data
# has changed card definitions, etc.
mongorestore --uri="${MONGO_RESTORE_URI}" --archive="$MONGO_ARCHIVE" --nsFrom='jnet-tmpl.*' --nsTo="${MONGO_RESTORE_DB}.*" \
    --nsInclude='jnet-tmpl.altarts' --nsInclude='jnet-tmpl.cards*' --nsInclude='jnet-tmpl.config' \
    --nsInclude='jnet-tmpl.cycles' --nsInclude='jnet-tmpl.formats' \
    --nsInclude='jnet-tmpl.mwls' --nsInclude='jnet-tmpl.sets' \
    --drop

echo "Database initialization completed successfully."
