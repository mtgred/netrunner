# git remote add openshift -f <openshift git repo>

git stash
rhc scp netrunner upload game.js app-root/data
lein cljsbuild clean
lein cljsbuild once prod &
git push openshift master
rhc scp netrunner upload resources/public/js/app.js app-root/repo/resources/public/js
