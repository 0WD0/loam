(ns loam.diagnostic
  "Structured, deterministic compiler diagnostics."
  (:require [loam.ast :as ast]))

(def severities #{:info :warning :error})

(defn line-starts
  "Return zero-based character offsets for the start of every source line."
  [source]
  (loop [index 0
         starts [0]]
    (if-let [newline (and source (.indexOf ^String source "\n" index))]
      (if (neg? newline)
        starts
        (recur (inc newline) (conj starts (inc newline))))
      starts)))

(defn offset->line-column
  "Convert an Emacs/Org one-based character OFFSET to one-based line/column."
  [starts offset]
  (when (and (integer? offset) (pos? offset))
    (let [zero-based (dec offset)
          line-index (loop [low 0 high (dec (count starts)) best 0]
                       (if (> low high)
                         best
                         (let [mid (quot (+ low high) 2)
                               start (nth starts mid)]
                           (if (<= start zero-based)
                             (recur (inc mid) high mid)
                             (recur low (dec mid) best)))))]
      {:line (inc line-index)
       :column (inc (- zero-based (nth starts line-index)))})))

(defn source-span
  "Build a public, repository-relative source span for NODE in DOCUMENT."
  [document node]
  (let [p (ast/props node)
        begin (:begin p)
        end (:end p)
        starts (:line-starts document)
        start-loc (or (when-let [line (or (:start-line p) (:line p))]
                        {:line line :column (or (:start-column p) (:column p) 1)})
                      (offset->line-column starts begin))
        end-loc (or (when-let [line (:end-line p)]
                      {:line line :column (or (:end-column p) 1)})
                    ;; ox-edn/Org :end is an exclusive one-based offset.
                    (offset->line-column starts
                                         (when end (max (or begin 1) (dec end)))))]
    (cond-> {:path (get-in document [:source :path])}
      begin (assoc :begin begin)
      end (assoc :end end)
      start-loc (assoc :start start-loc)
      end-loc (assoc :end-location end-loc))))

(defn diagnostic
  "Create a diagnostic. CONTEXT accepts :document, :node, :source-span,
  :node-type, :phase, and a public :data map."
  [severity code message {:keys [document node source-span node-type phase data]}]
  (when-not (contains? severities severity)
    (throw (ex-info "Invalid Loam diagnostic severity"
                    {:severity severity :code code})))
  (cond-> {:severity severity
           :code code
           :message message
           :source-span (or source-span
                            (when document
                              (loam.diagnostic/source-span document
                                                           (or node (:ast document))))
                            {})
           :node-type (or node-type (:type node) :org-data)}
    phase (assoc :phase phase)
    (seq data) (assoc :data data)))

(defn error
  [code message context]
  (diagnostic :error code message context))

(defn warning
  [code message context]
  (diagnostic :warning code message context))

(defn info
  [code message context]
  (diagnostic :info code message context))

(defn error? [diagnostic]
  (= :error (:severity diagnostic)))

(defn errors [diagnostics]
  (filter error? diagnostics))

(defn sort-key [d]
  [(get-in d [:source-span :path] "")
   (get-in d [:source-span :begin] -1)
   (name (:severity d))
   (name (:code d))
   (:message d)])

(defn sort-diagnostics [diagnostics]
  (vec (sort-by sort-key diagnostics)))

(defn throw-if-errors!
  "Throw one ex-info containing all diagnostics when any error is present."
  ([diagnostics] (throw-if-errors! "Loam docs compilation failed" diagnostics))
  ([message diagnostics]
   (let [diagnostics (sort-diagnostics diagnostics)]
     (when (some error? diagnostics)
       (throw (ex-info message
                       {:code :compilation-failed
                        :diagnostics diagnostics})))
     diagnostics)))
