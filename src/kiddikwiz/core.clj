(ns kiddikwiz.core
  (:gen-class))

(def questions
  "A bunch of questions"
  (for [x (range 1 13)
        y (range 1 13)]
      {:no  nil
       :x   x
       :y   y
       :question       (str x " * " y " = ")
       :correct-answer (str (* x y))
       :answer         nil
       :correct        nil
       :time-taken     nil}))

(defn select-questions [questions n]
  "Select n random questions, number them"
  (->>
    (shuffle questions)
    (take n)
    (map-indexed (fn [n m] (assoc m :no (inc n))))))

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
  (let [tstart     (java.util.Date.)
        answer     (input (str "Question " (:no question) " : " (:question question)))
        time-taken (datediff tstart (java.util.Date.))]
    (assoc question
      :answer  answer
      :correct (= answer (question :correct-answer))
      :time-taken time-taken)))

(defn ask-questions
  "Ask a bunch of questions"
  [qseq]
  (doall (map ask-question qseq)))

(defn show-incorrect
  [answers]
  (if-let [wrong (seq (filter (complement :correct) answers))]
    (do
      (println "You answered the following questions incorrectly:")
      (doseq [w wrong]
             (println (str "Question " (w :no) ": " (w :question) (w :correct-answer) ", you answered " (w :answer)))))))

(defn show-results
  "Show the results of a round of questions"
  [answers]
  (intern *ns* 'answers answers) ;; todo - get rid
  (let [{correct true incorrect false} (group-by :correct answers)
         num_answers (count answers)
         num_correct (count correct)
         total_time (reduce + (map :time-taken answers))]
    (println "================================================")
    (println (format "You scored %s out of %d" num_correct num_answers))
    (println (format "Total time taken %.1f seconds" total_time))
    (println (format "Average time per question %.1f seconds" (/ total_time num_answers)))
    (show-incorrect answers)
    (println "================================================")))

(defn round
  "Ask a round of `n questions from `questions"
  [questions n]
  (->>
    (select-questions questions n)
    ask-questions
    show-results))

(defn play []
  "Main play loop"
  (let [choice (input "Ready? [Y-Yes N-Exit] : ")]
    (if (#{"Y" "y"} choice)
      (do
        (println "==========================")
        (round questions 10)
        (recur))
      (println "See ya, wouldn't wanna be ya!"))))

(defn -main
  "Main entry point"
 [& args]
  (play))

