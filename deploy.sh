# git remote add openshift -f <openshift git repo>

lein cljsbuild once prod &
git push openshift
rhc scp netrunner upload resources/js/app.js app-root/repo/resources/js
