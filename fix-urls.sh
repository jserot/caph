#!/bin/bash

ANCIEN="dream.ispr-ip.fr/CAPH"
NOUVEAU="jserot.github.io/caph/docs"

# Corriger les liens dans les fichiers HTML
find . -name "*.html" | xargs sed -i '' "s|$ANCIEN|$NOUVEAU|g"

#git add .
#git commit -m "Fix liens documents"
#git push origin gh-pages
