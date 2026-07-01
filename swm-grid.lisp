;;;; swm-grid.lisp

(in-package #:swm-grid)

(export '(*width*
          *height*
          *default-group*
					*background-color-occupied-group*
					*background-color-selected-group*
					*foreground-color-occupied-group*
					*foreground-color-selected-group*
          create-groups
          left-group
          right-group
          down-group
          up-group
					send-left-group
					send-right-group
					send-down-group
					send-up-group))

;;
;; Default to a 3x3 grid, opening at center.
;;

(defvar *width*         3)
(defvar *height*        3)
(defvar *default-group* 4)

(defvar *background-color-occupied-group* "blue")
(defvar *foreground-color-occupied-group* "white")

(defvar *background-color-selected-group* "white")
(defvar *foreground-color-selected-group* "black")

(setq *background-color-occupied-group* "blue")
(setq *foreground-color-occupied-group* "white")

(setq *background-color-selected-group* "white")
(setq *foreground-color-selected-group* "black")

(defun map-color-sequence (colorname)
	(cond ((string= colorname "black")
				 "0")
				((string= colorname "red")
				 "1")
				((string= colorname "green")
				 "2")
				((string= colorname "yellow")
				 "3")
				((string= colorname "blue")
				 "4")
				((string= colorname "magenta")
				 "5")
				((string= colorname "cyan")
				 "6")
				((string= colorname "white")
				 "7")
				((eq colorname nil)
				 "n")))

(defun create-groups ()
	"Create enough workspaces to fill the grid."
	(let*
			((groups  (stumpwm::sort-groups (stumpwm:current-screen)))
			 (ngroups (length groups)))
		(dotimes (i (- (* *width* *height*) ngroups))
			(stumpwm:gnew (write-to-string i))))
	(let
			((groups (stumpwm::sort-groups (stumpwm:current-screen))))
		(stumpwm::switch-to-group (nth *default-group* groups))))

(defun gnav-get-y (i)
	"Get the row given a group index."
	(floor (/ i *width*)))

(defun gnav-get-x (i)
	"Get the column given a group index."
	(mod i *width*))

(defun gnav-get-i (x y)
	"Get the group index of a row, column pair."
	(+ (* y *width*) x))

(defun gnav-step (i direction)
	"From group index I, step in one direction in '(:right :left :down :up)"
	(let*
			((curx      (gnav-get-x i))
			 (cury      (gnav-get-y i))
			 (newx      (case direction
										(:left     (max 0 (min (- *width* 1) (- curx 1))))
										(:right    (max 0 (min (- *width* 1) (+ curx 1))))
										(otherwise curx)))
			 (newy      (case direction
										(:up       (max 0 (min (- *height* 1) (- cury 1))))
										(:down     (max 0 (min (- *height* 1) (+ cury 1))))
										(otherwise cury)))
			 (newi      (gnav-get-i newx newy)))
		newi))

(defun gnav-current-index ()
	"Get the current group index."
	(position (current-group) (stumpwm::sort-groups (current-screen))))

(defun navigate-groups (direction)
	"Move groups as though they're in an MxN grid"
	(let*
			((groups    (stumpwm::sort-groups (current-screen)))
			 (curi      (position (current-group) groups))
			 (newi      (gnav-step curi direction))
			 (new-group (nth newi groups)))
		(progn
			(if (not (eq newi curi))
					(stumpwm::switch-to-group new-group))
			(gnav-echo-graph))))

(defun nl-join (items)
	"Join on new lines."
	(format nil (concatenate 'string "~{~A~^" (string #\newline) "~}") items))

(defun join (delim items)
	"Join strings on delimiter."
	(format nil (concatenate 'string "~{~A~^" (string delim) "~}") items))

(defun group-has-window-p (index)
	(let* ((screen (stumpwm:current-screen))
				 (group (find index (stumpwm:screen-groups screen) :key #'stumpwm::group-number)))
		(when group
			(not (null (stumpwm::group-windows group))))))

(defun num-digits (num)
	(length (write-to-string num)))

(defun pad-number (num max-digits)
	(format nil "~VD" (num-digits max-digits) num))

(defun gnav-get-fmt-row (y)
	(let*
			((curi   (gnav-current-index))
			 (firsti (gnav-get-i 0 y))
			 (lasti  (gnav-get-i *width* y)))
		(join " " (mapcar
							 (lambda (i)
								 (cond ((eq i curi)
												(concatenate 'string (format nil "^~A~A" (map-color-sequence *foreground-color-selected-group*) (map-color-sequence *background-color-selected-group*)) (pad-number i (* *width* *height*)) "^n"))
											 ((group-has-window-p (+ i 1))
												(concatenate 'string (format nil "^~A~A" (map-color-sequence *foreground-color-occupied-group*) (map-color-sequence *background-color-occupied-group*)) (pad-number i (* *width* *height*)) "^n"))
											 ((pad-number i (* *width* *height*)))))
							 (range lasti :min firsti)))))

(defun gnav-echo-graph ()
	"Echo an ascii workspace indicator."
	(let*
			((rows (mapcar 'gnav-get-fmt-row (range *height*)))
			 (img  (nl-join rows)))
		(echo img)))

(defun range (max &key (min 0) (step 1))
	"https://stackoverflow.com/questions/13937520/pythons-range-analog-in-common-lisp"
	(loop for n from min below max by step
				collect n))

(defun send-window-to-group (direction)
	"Send current window to group in direction within MxN grid"
	(let*
			((groups (stumpwm::sort-groups (current-screen)))
			 (curi (position (current-group) groups))
			 (newi (gnav-step curi direction))
			 (new-group (nth newi groups)))
		(progn
			(if (not (eq newi curi))
					(stumpwm::gmove new-group))
			(gnav-echo-graph))))


(defcommand left-group ()
	() ""
	(navigate-groups :left))

(defcommand right-group ()
	() ""
	(navigate-groups :right))

(defcommand down-group ()
	() ""
	(navigate-groups :down))

(defcommand up-group ()
	() ""
	(navigate-groups :up))

(defcommand send-left-group ()
	() ""
	(send-window-to-group :left))

(defcommand send-right-group ()
	() ""
	(send-window-to-group :right))

(defcommand send-down-group ()
	() ""
	(send-window-to-group :down))

(defcommand send-up-group ()
	() ""
	(send-window-to-group :up))
