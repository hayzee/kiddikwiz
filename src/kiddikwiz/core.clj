(ns kiddikwiz.core
  (:gen-class))

(defn input [prompt]
  (do
    (print prompt)
    (flush)
    (read-line)))

(defn datediff [d1 d2]
  (double (/ (- (.getTime d2) (.getTime d1)) 1000)))

(def questions
  (for [x (range 1 13)
        y (range 1 13)]
    {:x x
    :y y
    :question       (str x " * " y " = ")
    :correct-answer (str (* x y))
    :correct        nil
    :time-taken     0}))

(defn ask-question [q]
  (let [tstart (java.util.Date.)
        a (input (q :question))
        t (datediff tstart (java.util.Date.))]
    (->
      q
      (assoc :correct (= a (q :correct-answer)))
      (assoc :time-taken t))))

(defn ask-questions [qseq]
  (doall (map ask-question qseq)))

(defn show-results [answers]
  (intern *ns* 'answers answers) ;; todo - get rid
  (let [{correct true incorrect false} (group-by :correct answers)
         num_answers (count answers)
         num_correct (count correct)
         ;            num_incorrect (count incorrect)
         total_time (reduce + (map :time-taken answers))]
    (println (format "You scored %s out of %d" num_correct num_answers))
    (println (format "Total time taken %.1f seconds" total_time))
    (println (format "Average time per question %.1f seconds" (/ total_time num_answers)))))

(defn round[questions n]
  (->>
    (shuffle questions)
    (take n)
    ask-questions
    show-results))

(defn play []
  (let [choice (input "Ready? [Y-Yes N-Exit] : ")]
    (if (#{"Y" "y"} choice)
      (do (round questions 10) (recur))
      (println "See ya, wouldn't wanna be ya!"))))

(defn -main
 [& args]
  (play))

