(ns web.tokens
  (:require [clj-time.core :as t]
            [buddy.sign.jwt :as jwt]))

(defn- kp-generator [length]
  (doto (java.security.KeyPairGenerator/getInstance "RSA")
    (.initialize length)))

(defn- generate-keypair [length]
  (assert (>= length 512) "RSA Key must be at least 512 bits long.")
  (.generateKeyPair (kp-generator length)))

(defn- decode64 [str]
  (.decode (java.util.Base64/getDecoder) str))

(defn- encode64 [bytes]
  (.encodeToString (java.util.Base64/getEncoder) bytes))

(defn- der-string->pub-key [string]
  "Generate an RSA public key from a DER-encoded Base64 string.
   Some systems like to line-wrap these at 64 characters, so we
   have to get rid of any newlines before decoding."
  (let [non-wrapped (clojure.string/replace string #"\n" "")
        key-bytes (decode64 non-wrapped)
        spec (java.security.spec.X509EncodedKeySpec. key-bytes)
        key-factory (java.security.KeyFactory/getInstance "RSA")]
    (.generatePublic key-factory spec)))

(defn- public-key->der-string [key]
  "Generate DER-formatted string for a public key."
  (-> key
      .getEncoded
      encode64
      (clojure.string/replace #"\n" "")))

(defn create-api-token [emailhash]
  (let [keypair (generate-keypair 1024)
        private-key (.getPrivate keypair)
        public-key (.getPublic keypair)
        public-key-str (public-key->der-string public-key)
        now (t/now)
        expires (-> 365 t/days t/from-now)
        claims {:sub "deck-api"
                :emailhash emailhash
                :iat now
                :exp expires}]
    {:token (jwt/sign claims private-key {:alg :rs256})
     :public-key public-key-str
     :issued now
     :expires expires}))

(defn verify-api-token [token public-key]
  (try
    (let [decoded-key (der-string->pub-key public-key)
          verified-token (jwt/unsign token decoded-key {:alg :rs256})]
      [:valid verified-token])
    (catch Exception e [:invalid (.getMessage e)])))
