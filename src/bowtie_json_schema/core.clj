(ns bowtie-json-schema.core
  (:require [clojure.stacktrace]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [json-schema.core :as json-schema])
  (:import [java.io ByteArrayInputStream IOException UncheckedIOException]
           [java.nio.charset StandardCharsets]
           [org.everit.json.schema.loader SchemaClient SchemaLoader]
           [org.json JSONObject JSONTokener])
  (:gen-class))

(defn- registry-client
  "A SchemaClient which retrieves schemas from a test case's registry,
  and never from the network."
  [registry]
  (reify SchemaClient
    (get [_ url]
      (let [uri (first (string/split url #"#" 2))]
        (if (contains? registry uri)
          (ByteArrayInputStream. (.getBytes (json/write-str (registry uri)) StandardCharsets/UTF_8))
          (throw (UncheckedIOException. (IOException. (str uri " is not in the registry")))))))))

(defn- prepare-schema
  "Load a test case's schema, retrieving the external schemas it refers to from
  the case's registry. json-schema.core's own prepare-schema can only retrieve
  them over the network or from the classpath, but its validate accepts a
  schema loaded this way."
  [test-case]
  (-> (SchemaLoader/builder)
      (.schemaClient (registry-client (get test-case "registry" {})))
      (.schemaJson (JSONObject. (JSONTokener. (json/write-str (test-case "schema")))))
      (.build)
      (.load)
      (.build)))

(defn -main []
  (let [started (atom false)]
    (doseq [request (map json/read-str (line-seq (java.io.BufferedReader. *in*)))]
      (println
        (json/write-str
          (case (request "cmd")
            "start" (do (assert (= (request "version") 1) "Not version 1!")
                        (reset! started true)
                        {:version 1
                         :implementation
                         {:language :clojure
                          :name :json-schema
                          :homepage "https://github.com/luposlip/json-schema"
                          :issues "https://github.com/luposlip/json-schema/issues"
                          :source "https://github.com/luposlip/json-schema"

                          :dialects ["http://json-schema.org/draft-07/schema#",
                                     "http://json-schema.org/draft-06/schema#",
                                     "http://json-schema.org/draft-04/schema#"]
                          :os (System/getProperty "os.name")
                          :os_version (System/getProperty "os.version")
                          :language_version (clojure-version)}})
            "dialect" (do (assert @started "Not started!")
                          {:ok false})
            "run" (do (assert @started "Not started!")
                      (let [test-case (request "case")]
                        (if (= (request "output") "annotations")
                          {:seq (request "seq")
                           :results (mapv (fn [_]
                                            {:skipped true
                                             :message "clojure-json-schema does not support annotation collection"})
                                          (test-case "tests"))}
                          (try
                            {:seq (request "seq")
                             :results (let [schema (prepare-schema test-case)]
                                        (mapv #(try
                                                 (json-schema/validate schema (% "instance"))
                                                 {:valid true}
                                                 (catch clojure.lang.ExceptionInfo e
                                                   ;; Validation errors come with :errors. Anything else,
                                                   ;; like "Unsupported JSON input" for an instance which
                                                   ;; isn't an object or array, isn't a validation result.
                                                   (if (contains? (ex-data e) :errors)
                                                     {:valid false}
                                                     {:errored true
                                                      :context {:message (ex-message e)}})))
                                              (test-case "tests")))}
                            (catch Throwable e
                              {:seq (request "seq")
                               :errored true
                               :context {:traceback (with-out-str (clojure.stacktrace/print-cause-trace e 4))}})))))
            "stop" (do (assert @started "Not started!")
                       (System/exit 0))))))))
