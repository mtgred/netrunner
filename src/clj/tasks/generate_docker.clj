(ns tasks.generate-docker
  "Generate docker-compose file"
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as str]
   [clostache.parser :as cp]))

  (defn usage
    [options-summary]
    (->> [""
          "Usage: lein generate-docker [options]"
          ""
          "Options:"
          options-summary]
         (str/join \newline)))

  (def cli-options
    [["-t" "--template PATH" "Path to docker-compose template" :default "docker/prod/docker-compose.yml.tpl"]
     ["-o" "--output PATH" "Path to generated docker-compose file" :default "docker-compose.prod.yml"]
     ["-i" "--image IMAGE-NAME" "Image name is required" "Netrunner Docker image name" :missing "Image name is required"]
     ["-p" "--port PORT" "Port exposing Netrunner" :default "1042"]
     ["-r" "--folder-resources FOLDER-RESOURCES" "Path to the public resources" :default "./resources/public/"]
     ["-f" "--config CONFIG-FILE" "Path to the configuration file" :default "./docker/prod/prod.edn"]
     ["-m" "--image-mongodb IMAGE-NAME-MONGODB" "Image name of MongoDB" :default "mongo"]
     ["-d" "--folder-mongodb FOLDER-MONGODB" "Folder of the MongoDB database" :default "./data/"]
     ["-c" "--close-mongodb" "Disable MongoDB connectivity outside of Docker internal network"
      :id :expose-mongodb :default true :parse-fn not]])

  (defn exit [status msg]
    (binding [*out* *err*]
      (println msg))
    (System/exit status))

  (defn command
    [& args]
      (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
        (if (or errors
                (not-empty arguments))
          (exit 1 (str/join \newline (conj errors (usage summary))))
          (let [tpl (slurp (:template options))]
            (spit (:output options) (cp/render tpl {
                                                    :image-name (:image options)
                                                    :config-file (:config options)
                                                    :port-endpoint (:port options)
                                                    :folder-resources (:folder-resources options)
                                                    :image-name-mongodb (:image-mongodb options)
                                                    :folder-mongodb (:folder-mongodb options)
                                                    :expose-mongodb (:expose-mongodb options)}))))))


