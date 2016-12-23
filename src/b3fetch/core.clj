(ns b3fetch.core
  (:require [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as http-kit]
            [clojure.data.json :as json]
            [clojure.java.io :as io])
  (:gen-class))

(def ^:dynamic *base-url* "http://blog.alexeypegov.com")
(def ^:dynamic *more-selector* [:#more])
(def ^:dynamic *note-selector* [:.note])
(def ^:dynamic *title-selector* [:.title :.name :a])
(def ^:dynamic *text-selector* [:.text])
(def ^:dynamic *info-selector* [:.title :.info :span])
(def ^:dynamic *tags-selector* [:.title :.info :.tags :.tag])
(def ^:dynamic *date-regex* #"^\D+(\d{1,2}\D+\d{4})\S|$")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn next-key
  [res]
  (:data-next-key (:attrs (first (html/select res *more-selector*)))))

(defn notes
  [res]
  (html/select res *note-selector*))

(defn content-field
  [note selector]
  (first (map html/text (html/select note selector))))

(defn html-field
  [note selector]
  (apply str (html/emit* (html/select note selector))))

(defn note-url
  [note]
  (:href (:attrs (first (html/select note *title-selector*)))))

(defn note-date
  [note]
  (last (re-find *date-regex* (html/text (first (html/select note *info-selector*))))))

(defn note-tags
  [note]
  (map html/text (html/select note *tags-selector*)))

(defn parse-note
  [note]
  (let [title   (content-field note *title-selector*)
        date    (note-date note)
        url     (note-url note)
        tags    (note-tags note)
        content (html-field note *text-selector*)]
        (zipmap [:title :date :url :tags :content] [title date url tags content])))

(defn post-more
  [key]
  (if key
    (let [{:keys [status headers body error] :as resp} @(http-kit/post (str *base-url* "/more") {:form-params {:key key}})]
      (if error
        (println "Failed, exception" error)
        (html/html-resource (java.io.StringReader.
          (:html (json/read-str body :key-fn keyword))))))
    nil))

(defn pages-seq
  [res]
  (if res
    (cons res (lazy-seq (pages-seq (post-more (next-key res)))))
    nil))

(defn notes-seq
  []
  (concat (map parse-note (mapcat notes (pages-seq (fetch-url *base-url*))))))

(defn write-json
  [filename]
  (with-open [w (io/writer filename :encoding "UTF-8")]
    (json/write (notes-seq) w :escape-unicode false
                              :escape-js-separators false
                              :escape-slash false)))

(defn -main
  [& args]
  (write-json "blog.json"))
