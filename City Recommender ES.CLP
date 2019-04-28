;;;======================================================
;;;   City Recommender Expert System
;;;
;;;     This expert system can recommend a city
;;;	base on user desisions.
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-want-to-travel ""
   (not (want-to-travel ?))
   (not (factor ?))
   (not (destination ?))
   =>
   (assert (want-to-travel (yes-or-no-p "Do you want to travel to a new city (yes/no)? "))))

(defrule determine-important-factor ""
   (want-to-travel yes)
   (not (factor ?))
   (not (destination ?))
   =>
   (assert (factor (ask-question "What is the most important factor for you (weather/adventure/place-type/with-who/budget/time)? "
                    weather adventure place-type with-who budget time))))



(defrule determine-weather-type ""
   (want-to-travel yes)
   (factor weather)
   (not (destination ?))
   =>
   (assert (weather-type (ask-question "What weather do you like (hot/cold/cool)? "
                    hot cold cool))))

(defrule determine-adventure-state ""
   (want-to-travel yes)
   (factor adventure)
   (not (destination ?))
   =>
   (assert (adventure-state (ask-question "What do you like more (adventure/relaxing)? "
                    adventure relaxing))))

(defrule determine-place-type ""
   (want-to-travel yes)
   (factor place-type)
   (not (destination ?))
   =>
   (assert (place-type (ask-question "What place type do you prefare the most (city/beach/countryside/mountains/monuments)? "
                    city beach countryside mountains monuments))))

(defrule determine-with-who ""
   (want-to-travel yes)
   (factor with-who)
   (not (destination ?))
   =>
   (assert (with-who (ask-question "Who will you go with (alone/couple/friends/family)? "
                    alone couple friends family))))

(defrule determine-budget-state ""
   (want-to-travel yes)
   (factor budget)
   (not (destination ?))
   =>
   (assert (budget-state (ask-question "What is your budget state (low/medium/high)? "
                    low medium high))))

(defrule determine-time-state ""
   (want-to-travel yes)
   (factor time)
   (not (destination ?))
   =>
   (assert (time-state (ask-question "How long is your travel will be (holiday/week/more)? "
                    holiday week more))))


;;;****************
;;;* ANSWER RULES *
;;;****************

(defrule no-travel-state ""
   (want-to-travel no)
   (not (destination ?))
   =>
   (assert (destination  "WHY! Maybe next time.")))

(defrule place-type-state-mountains ""
   (place-type mountains)
   (not (destination ?))
   =>
   (assert (destination  "Mount Everest")))

(defrule time-state-more ""
   (time-state more)
   (not (destination ?))
   =>
   (assert (destination  "Alaska")))

(defrule budget-state-high ""
   (budget-state high)
   (not (destination ?))
   =>
   (assert (destination  "Paris")))

(defrule with-who-state-family ""
   (with-who family)
   (not (destination ?))
   =>
   (assert (destination  "maldives")))

(defrule place-type-state-monuments ""
   (place-type monuments)
   (not (destination ?))
   =>
   (assert (destination  "Giza")))

(defrule place-type-state-beach ""
   (place-type beach)
   (not (destination ?))
   =>
   (assert (destination  "Rio de Janeiro")))

(defrule weather-type-state-hot ""
   (weather-type hot)
   (not (destination ?))
   =>
   (assert (destination  "Australia")))

(defrule weather-type-state-cool ""
   (weather-type cool)
   (not (destination ?))
   =>
   (assert (destination  "Roma")))

(defrule adventure-state-adventure ""
   (adventure-state adventure)
   (not (destination ?))
   =>
   (assert (destination  "San Francisco")))

(defrule adventure-state-relaxing ""
   (adventure-state relaxing)
   (not (destination ?))
   =>
   (assert (destination  "Ko Samui")))

(defrule place-type-state-city ""
   (place-type city)
   (not (destination ?))
   =>
   (assert (destination  "NewYork")))

(defrule place-type-state-countryside ""
   (place-type countryside)
   (not (destination ?))
   =>
   (assert (destination  "Ubud")))

(defrule with-who-state-alone ""
   (with-who alone)
   (not (destination ?))
   =>
   (assert (destination  "Montreal")))

(defrule with-who-state-couple ""
   (with-who couple)
   (not (destination ?))
   =>
   (assert (destination  "Amsterdam")))

(defrule with-who-state-friends ""
   (with-who friends)
   (not (destination ?))
   =>
   (assert (destination  "Barcelona")))

(defrule budget-state-low ""
   (budget-state low)
   (not (destination ?))
   =>
   (assert (destination  "Jamaica")))

(defrule budget-state-medium ""
   (budget-state medium)
   (not (destination ?))
   =>
   (assert (destination  "Cairo")))

(defrule time-state-week ""
   (time-state week)
   (not (destination ?))
   =>
   (assert (destination  "London")))

(defrule time-state-holiday ""
   (time-state holiday)
   (not (destination ?))
   =>
   (assert (destination  "Beirut")))

(defrule weather-type-state-cold ""
   (weather-type cold)
   (not (destination ?))
   =>
   (assert (destination  "Moscow")))


;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The City Recommendation Expert System")
  (printout t crlf crlf))

(defrule print-destination ""
  (declare (salience 10))
  (destination ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested City:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))