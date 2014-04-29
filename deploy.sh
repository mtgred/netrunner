# git remote add openshift -f <openshift git repo>

git push openshift
lein cljsbuild once prod
rhc scp netrunner upload resources/js/app.js app-root/repo/resources/js
