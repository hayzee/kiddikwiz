(ns kiddikwiz.core
  (:gen-class))

(def questions
  "A bunch of questions"
  (for [x (range 1 13)
        y (range 1 13)]
    {:x x
    :y y
    :question       (str x " * " y " = ")
    :correct-answer (str (* x y))
    :correct        nil
    :time-taken     0}))

(defn input
  "Accept user input with a prompt"
  [prompt]
  (do
    (print prompt)
    (flush)
    (read-line)))

(defn datediff
  "Difference (in seconds) between two dates"
  [d1 d2]
  (double (/ (- (.getTime d2) (.getTime d1)) 1000)))

(defn ask-question
  "Ask a question and assoc the response"
  [question]
  (let [tstart (java.util.Date.)
        a (input (question :question))
        t (datediff tstart (java.util.Date.))]
    (->
      question
      (assoc :correct (= a (question :correct-answer)))
      (assoc :time-taken t))))

(defn ask-questions
  "Ask a bunch of questions"
  [qseq]
  (doall (map ask-question qseq)))

(defn show-results
  "Show the results of a round of questions"
  [answers]
  (intern *ns* 'answers answers) ;; todo - get rid
  (let [{correct true incorrect false} (group-by :correct answers)
         num_answers (count answers)
         num_correct (count correct)
         total_time (reduce + (map :time-taken answers))]
    (println (format "You scored %s out of %d" num_correct num_answers))
    (println (format "Total time taken %.1f seconds" total_time))
    (println (format "Average time per question %.1f seconds" (/ total_time num_answers)))))

(defn round
  "Ask a round of `n questions from `questions"
  [questions n]
  (->>
    (shuffle questions)
    (take n)
    ask-questions
    show-results))

(defn play []
  "Main play loop"
  (let [choice (input "Ready? [Y-Yes N-Exit] : ")]
    (if (#{"Y" "y"} choice)
      (do (round questions 10) (recur))
      (println "See ya, wouldn't wanna be ya!"))))

(defn -main
  "Main entry point"
 [& args]
  (play))

