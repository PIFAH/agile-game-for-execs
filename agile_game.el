;; This is an attempt to quickly program a an Agile game.

;; This is basically a card game.  I'm going to program up the "cards" here
;; into a series of "decks".  There will be 6 turns in the game. After each turn,
;; The user can assign teams to projects.  The point of the game is to get a maximum
;; score based on reach user satisfaction goals.

;; Every card starts with a title, a unique symobl and then includes functions that are to be applied in some way.
(setq team-events [
		  ("Lead Developer wins lottery and joins Peace Corps. Zero Velocity this turn for Team."
		    lead-developer-joins-peace-corps
		    (lambda (vcards) 0)
		    )
		  ("Estimates aren't ALWAYS low. Double Velocity this turn for Team."
		    estimates-are-not-always-low
		    (lambda (vcards) 0)
		    )
		  ("Pivot: Halve the cost of any one goal."
		    halve-the-cost-of-one-goal
		    ;; Someohow here we want to get input as to which goal to halve
		    (lambda () 0)
		    )
		  ("Automated Testing Expert joins Team. While in play, no velocity card is ever less than 10."
		    automate-testing-expert-joins-team
		    ;; Someohow here we want to get input as to which goal to halve
		    (lambda () 0)
		    )
		  ("Top-notch Agile Designer joins Team. Attach to any team for an extra discovery card each turn."
		    agile-developer-joins-team
		    ;; Someohow here we want to get input as to which goal to halve
		    (lambda () 0)
		    )
		  ])
		  
(setq discoverys [("Gradual is better. Split any Goal evenly into two subgoals."
		  split-a-goal
		  )
		 ("New value found! Add a 50 point goal to any project."
		  new-goal-found
		  )
		 ("User research finds better way. Halve cost of any one goal."
		  halve-cost-of-a-goal
		  )
		 ])

(setq global-events [
		    ("Zombie Outbreak! Draw no velocity cards this turn".)
		    ("Congress Mandates COBOL! Add 25 points to cost of every goal.")
		    ])

(setq team-resources [
		      ("Average Team. Draw 1 Velocity Card. Draw 1 Discovery Card on Even-numbered Rounds."
		       Average-Team
		       (lambda (game turn)
			 (list (draw-velocity))))
		      ("Elite Team. Draw 2 Velocity Cards. Draw 1 Discovery Card on Even-numbered Rounds."
		       Elite-Team
		       (lambda (game turn)
			 (list (draw-velocity) (draw-velocity))))
		      ("User-focused Team. Draw 1 Velocity Card. Draw 1 Discovery Card every Turn."
		       User-Focused-Team
		      (lambda (game turn)
			  (list (draw-velocity))))
		      ("Weak Team. Draw 1 Velocity Card and substract 5 from it."
		       Weak-Team
		      (lambda (game turn)
			  (list (- (draw-velocity) 5))))
		      ("Erratic Team. Draw 2 Velocity cards 50% of the time, 0 in other case. Draw 1 Discovery Card on Odd-numbered Rounds."
		       Erratic-Team
		       (lambda (game turn)
			 (list (- (draw-velocity) 5))))
		      ])

(setq projects [
		("Faster-than-light drive."
		 FTL-Project
		 (A 30 30)
		 (B 100 100)
		 (C 200 200)
		)
		("Pocket Fusion Generator."
		 Pocket-Fusion-Project
		(A  75 75)
		(B 150 150)
		)
		("Rejuventation Treatement. All progress is secured at end of turn."
		 Rejuvenation-Project
		 (A  10 10)
		 (B  10 10)
		 (C  10 10)
		 (D  10 10)
		 (E  10 10)
		 (F  10 10)		 
		 (G  10 10)
		 (H  10 10)
		 (I  10 10)
		 (J  10 10)
		)
		("Giga-watt-hour super capacitor"
		 Super-Capacitor-Project
		(A  10 10)
		(B  100 100)
		(C  150 150)
		)
	       ])

(defun choose-from-array (a)
  (aref a (random (length a))))

(defun choose-n-with-replacement (a n)
  (let (value)      ; otherwise a value is a void variable
  (dotimes (number n value)
    (setq value (cons (choose-from-array a) value)))))

(defun choose-n-without-replacement (a n)
  (let ((indices (number-sequence 0 (- (length a) 1))))
    (let (value)      ; otherwise a value is a void variable
      (dotimes (number n value)
	(progn
	  (let ((q (nth (random (length indices)) indices)))
	    (setq indices (remq q indices))
	    (setq value (cons (aref a q) value))))))))

(defun draw-velocity ()
  (* 10 (random 3)))

(defun create-turn (alist global-events)
  `(turn (assignments . ,alist)
	 (globals-cards . ,global-events)
	 (team-cards . ())
	 ))

(defun cons-game (projects teams turns)
  `((projects . ,projects)
    (teams . ,teams)
    (turns . ,turns)))

(defun start-game ()
  "Create a game by performing the random draws necessary. Return Value is the game"
  ;; A game consists of projects, turns, resource assignments, etc.
  (cons-game
   (choose-n-without-replacement projects 3)
   (choose-n-without-replacement team-resources 5)
   ()))

(defun choose-waterfall-project (game)
  )

(defun executive-choice ()
  "Here we allow the executive to choose to move a team. If one of the projects is not yet assigned as the waterfall project, that"
  )


(defun exec-team-event-draws (game turn teams)
  "Here we use the rules to draw team events and velocity cards for each team. The result here is a set of card draws associated with each team."
  (mapcar #'(lambda (x)
	      (progn
		(let ((draws (funcall (car (cddr x)) game turn)))
		  (cons (cadr x) draws)))
	      )
	  teams))

(defun add-turn-and-events (game turn events)
  (cons-game
   (cdr (assoc 'projects game))
   (cdr (assoc 'teams game))
   (cons
    (let* ((global-cards (cdr (assoc 'global-cards (cdr turn))))
	   (team-cards (cdr (assoc 'team-cards (cdr turn))))
	   (assignments (cdr (assoc 'assignments (cdr turn))))
	   )
      `(turn (assignments . ,assignments)
	     (global-cards . ,global-cards)
	     (team-cards . ,events))
      )
    (cdr (assoc 'turns game))
    )
   )
  )

(defun add-turn (game turn)
  (cons-game (cdr (assoc 'projects game))
	     (cdr (assoc 'teams game))
	     (cons turn (cdr (assoc 'turns game)))))

(defun make-turn (game alist)
  "perform a single turn. Argument is assoc list assigning teams to projects."
  (let* ((teams (cdr (assoc 'teams game)))
	 (turns (cdr (assoc 'turns game)))
	 (turn-num (+ 1 (length turns)))
	 (turn (create-turn alist (choose-from-array global-event))))
    (let* ((team-events (exec-team-event-draws game turn teams)))
      (add-turn-and-events game turn team-events)
      )
    ))
  

(defun get-velocity (project turn)
  "Get the velocity for the project based on this turn."
  ;; The problem here is that this is looking at only one team.
  ;; But there is a group of teams associated with the project.
  ;; We need to get the set of teams, and if in that set, add up the score.
  (let ((team (car (rassoc project (cdr (assoc 'assignments turn)))))
	(team-cards (cdr (assoc 'team-cards turn))))
    ;; (print (cdr (assoc 'assignments turn)))
    ;; (print project)
    ;; (print team)
    ;; (print team-cards)

    (apply '+ (mapcar (lambda (element)
		(let ((lteam (car element))
		      (value (cdr element)))
		  (if (equal
		       project
		       (cdr (assoc lteam (cdr (assoc 'assignments turn)))))
		      (apply '+ value)
		    0
		    )))
		   team-cards))
    ))
    

(defun compute-velocity-project-through-turns (game project turns)
    (apply
     '+
     (mapcar #'(lambda (tn)
		 (get-velocity project tn)
		 )
	     turns))
    )


(defun compute-velocity-project (game project)
  (let ((turns (cdr (assoc 'turns game))))
    (compute-velocity-project-through-turns game project turns)
    )
  )

(defun get-projects (game)
  (cdr (assoc 'projects game)))

(defun get-project-symbols (game)
  (mapcar (lambda (p) (cadr p)) (get-projects game)))

(defun get-teams (game)
  (cdr (assoc 'teams game)))

(defun get-team-symbols (game)
  (mapcar (lambda (p) (cadr p)) (get-teams game)))


(defun compute-velocitys (game)
  (mapcar #'(lambda (proj)
	      (let ((p (cadr proj)))
		(cons p (compute-velocity-project game p))))
	  (get-projects game)))

(defun last-n (lst n)
  (if (<= (length lst) n)
      lst
     (last-n (cdr lst) n)))

(defun compute-velocitys-through-turn (game i)
  (let ((turns (last-n (cdr (assoc 'turns game)) i)))
    (mapcar #'(lambda (proj)
		(let ((p (cadr proj)))
		  (cons p (compute-velocity-project-through-turns game p turns))))
	    (get-projects game))))

(defun render-as-ascii-chart (alist)
  (mapcar
   (lambda (element)
     (let ((key (car element))
	   (value (/ (cdr element) 10)))
       (cons key 
	     (let (rvalue)      ; otherwise a value is a void variable
	       (dotimes (i value rvalue)
		 (setq rvalue (concat "*" rvalue))
		 )))
	 ))
   alist))

(defun compute-score (velocity project)
  (let ((goals (cddr project)))
    (apply '+
	   (mapcar (lambda (g)
		     (let ((cost (cadr g))
			   (victory-points (nth 2 g)))
		       (if (>= velocity cost)
			   victory-points
			 0)
		       ))
		   goals)
	   )
    ))

(defun get-project-from-sym (psym projects)
  (if (null projects)
      ()
    (let ((p (car projects)))
      (if (equal (cadr p) psym)
	  (car projects)
	(get-project-from-sym psym (cdr projects))))))

(defun render-turn (game i)
  (let ((velocities (compute-velocitys-through-turn game i)))
    (list
     (cons (format "turn %s:" i)
	   (compute-velocitys-through-turn game i))
     (cons "victory points"
	   (mapcar
	    (lambda (element)
	      (let ((psym (car element))
		    (v (cdr element)))
		(let ((project (get-project-from-sym psym (get-projects game))))
		  (cons psym (compute-score v project)))
		))
	    velocities))
     ))
  )

(defun render-game (game)
  "render the game in a visually attractive way"
  (let* ((turns (cdr (assoc 'turns game)))
	 (n (length turns)))
    (reverse
     (let (value)      ; otherwise a value is a void variable
      (dotimes (i n value)
	(setq value (cons (render-turn game i) value))
	))
  )))

;; Should I now present victory conditions? That is really fundamental --- to cross reference the goals with the
;; velocity to compute victory contions.


;; Example game:
;; (setq game (start-game))
;; (setq first-turn '((Elite-Team . FTL-Project) (Erratic-Team . Rejuvenation-Project) (Average-Team . Rejuvenation-Project) (Weak-Team . Rejuvenation-Project)))
;; (setq game2 (make-turn game first-turn))
;; (setq game3 (make-turn game2 first-turn))
;; (setq game4 (make-turn game3 first-turn))
;; (setq game5 (make-turn game4 first-turn))
;; (setq game6 (make-turn game5 first-turn))

(defun simulate-play ()
  (setq game (start-game))
  (setq first-turn '((Elite-Team . FTL-Project) (Erratic-Team . Rejuvenation-Project) (Average-Team . Pocket-Fusion-Project) (Weak-Team . Super-Capacitor-Project) (User-Focused-Team . Super-Capactior-Project)))
  (setq game2 (make-turn game first-turn))
  (setq game3 (make-turn game2 first-turn))
  (setq game4 (make-turn game3 first-turn))
  (setq game5 (make-turn game4 first-turn))
  (setq game6 (make-turn game5 first-turn))
  )


(defun start-i ()
  (interactive)
  (setq game (start-game))
  (print (render-game game))
  )

(setq assignment '())
;; What we really want to do here is to construct the "assignment" of teams to projects.
;; We don't need to "play" the game in the mini buffer except to help with the typing of commands.
;; I'm happy entering lisp commands.
(defun assign-i (project team)             ; foo3 takes one argument,
  (interactive (list (completing-read "Select project: " (get-project-symbols game))
		     (completing-read "Select team: " (get-team-symbols game))
		     ))
  (let ((tm (intern-soft team))
	(pr (intern-soft project)))
    (print tm)
    (print pr)
    ;; we should probably delete the team assignment here, because there can only be one.
    (assq-delete-all tm assignment)
    (setq assignment (cons (cons tm pr) assignment))
    (print assignment)))

(defun turn-i ()             ; foo3 takes one argument,
  (interactive)
  (print assignment)
  (setq game (make-turn game assignment))
  (print (render-game game)))
  

