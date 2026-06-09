(ns eval.metrics
  "Compute CR-Bench-style metrics and print the summary table.
Self-contained: takes data, returns numbers or prints lines.")

;;; --- Compute ---

(defn- count-class [judgments cls-name]
  (count (filter #(= cls-name (:classification %)) judgments)))

(defn- distinct-goldens-hit [judgments]
  (->> judgments
       (filter #(= "BUG_HIT" (:classification %)))
       (keep   :matched_index)
       distinct
       count))

(defn- safe-div [a b]
  (if (pos? b) (/ (double a) b) 0.0))

(defn- f-measure [p r]
  (if (pos? (+ p r))
    (/ (* 2 p r) (+ p r))
    0.0))

(defn compute-metrics
  "CR-Bench-style metrics + standard precision/recall/F1.  JUDGE_FAILED
classifications are excluded from totals — they're unjudged, not negative."
  [judgments num-goldens]
  (let [bug-hits    (count-class judgments "BUG_HIT")
        valid       (count-class judgments "VALID_SUGGESTION")
        noise       (count-class judgments "NOISE")
        failed      (count-class judgments "JUDGE_FAILED")
        total       (+ bug-hits valid noise)
        useful      (+ bug-hits valid)
        goldens-hit (distinct-goldens-hit judgments)
        precision   (safe-div goldens-hit total)
        recall      (safe-div goldens-hit num-goldens)]
    {:bug-hits    bug-hits
     :valid       valid
     :noise       noise
     :failed      failed
     :total       total
     :goldens     num-goldens
     :goldens-hit goldens-hit
     :missed      (- num-goldens goldens-hit)
     :precision   precision
     :recall      recall
     :f1          (f-measure precision recall)
     :usefulness  (safe-div useful total)
     :snr         (if (pos? noise) (/ (double useful) noise) (double useful))}))

;;; --- Summary helpers ---

(defn- metric-sum [records k]
  (apply + (map #(get-in % [:metrics k]) records)))

(defn- metric-avg [records k]
  (if (zero? (count records)) 0.0
      (/ (metric-sum records k) (double (count records)))))

(defn- record-sum [records k]
  (apply + (keep #(get % k) records)))

(defn- record-avg [records k]
  (if (zero? (count records)) 0.0
      (/ (record-sum records k) (double (count records)))))

;;; --- Print blocks ---

(defn- print-counts [records]
  (println (format "PRs evaluated:    %d" (count records)))
  (println (format "Total findings:   %d  (hits %d, valid %d, noise %d)"
                   (metric-sum records :total)
                   (metric-sum records :bug-hits)
                   (metric-sum records :valid)
                   (metric-sum records :noise)))
  (when (pos? (metric-sum records :failed))
    (println (format "Judge failures:   %d  (excluded from totals)"
                     (metric-sum records :failed))))
  (println (format "Total goldens:    %d  (hit %d, missed %d)"
                   (metric-sum records :goldens)
                   (metric-sum records :goldens-hit)
                   (metric-sum records :missed))))

(defn- print-averages [records]
  (println (format "Avg Precision:    %.3f" (metric-avg records :precision)))
  (println (format "Avg Recall:       %.3f" (metric-avg records :recall)))
  (println (format "Avg F1:           %.3f" (metric-avg records :f1)))
  (println (format "Avg Usefulness:   %.3f" (metric-avg records :usefulness)))
  (println (format "Avg SNR:          %.3f" (metric-avg records :snr))))

(defn- print-tok-per-useful [records]
  (let [useful (+ (metric-sum records :bug-hits) (metric-sum records :valid))
        out    (record-sum records :output_tokens)]
    (when (and (pos? useful) (pos? out))
      (println (format "Output tok/useful finding: %d" (int (/ out useful)))))))

(defn- print-efficiency [records]
  (println (format "Avg rounds/PR:    %.1f" (record-avg records :rounds)))
  (println (format "Avg wall/PR:      %.1fs" (/ (record-avg records :wall_ms) 1000.0)))
  (println (format "Total output tok: %d  (avg %d/PR)"
                   (record-sum records :output_tokens)
                   (int (record-avg records :output_tokens))))
  (when (pos? (record-sum records :input_tokens))
    (println (format "Total input tok:  %d  (avg %d/PR)"
                     (record-sum records :input_tokens)
                     (int (record-avg records :input_tokens)))))
  (print-tok-per-useful records))

(defn- hydrate [r]
  (assoc r :metrics (compute-metrics (:judgments r) (:goldens_count r))))

(defn print-summary [records]
  (let [records (mapv hydrate records)]
    (println "\n=== EVAL SUMMARY ===")
    (print-counts records)
    (print-averages records)
    (println "\n--- Efficiency ---")
    (print-efficiency records)))
