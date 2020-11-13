;;;-*- Mode: common-lisp; syntax: common-lisp; package: gps; base: 10 -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(provide :gps1) ; by seiji

;;;; File gps1.lisp: First version of GPS (General Problem Solver)

(in-package :gps)

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS1 (*state* goals &optional (*ops* *ops*))
  "General Problem Solver: achieve all goals using *ops*."
  (if (every #'achieve1 goals) 'solved))

(defun appropriate-p1 (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op1 (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve1 (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defun achieve1 (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op1 
            (find-all goal *ops* :test #'appropriate-p1))))

#|
(defun achieve1 (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (print *state*)
  (or (member goal *state* :test #'equal)
      (some #'apply-op1 
            (find-all goal *ops* :test #'appropriate-p1))))

(defun appropriate-p1 (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op) :test #'equal))

(defun apply-op1 (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve1 (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op) :test #'equal))
    (setf *state* (union *state* (op-add-list op) :test #'equal))
    t))



(defun GPS1 (*state* goals &optional (*ops* *ops*))
  "General Problem Solver: achieve all goals using *ops*."
  (when (every #'(lambda (g) (achieve1 g nil)) goals) 'solved))

(defun achieve1 (goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (cond ((member goal *state* :test #'equal))
        ((member goal goal-stack) (break))
        (t (some #'(lambda (op) (apply-op1 op (cons goal goal-stack)))
             (remove-if-not #'(lambda (op) (appropriate-p1 goal op)) *ops*)))))

(defun apply-op1 (op goal-stack)
  "Print a message and update *state* if op is applicable."
  (print (list 'applying (op-action op)))
  (when (every #'(lambda (g) (achieve1 g goal-stack)) (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op) :test #'equal))
    (setf *state* (union *state* (op-add-list op) :test #'equal))
    t))
|#
;;; ==============================
(defparameter *space-robot-ops*
  (list 
   (make-op :action 'latch-ORU
            :preconds '((unlatched ORU) (has ORU))
            :add-list '((latched ORU))
            :del-list '((unlatched ORU)))
   (make-op :action 'unlatch-ORU
            :preconds '((latched ORU) (has ORU))
            :add-list '((unlatched ORU))
            :del-list '((latched ORU)))
   (make-op :action 'grasp-ORU
            :preconds '((has NOTHING) (on HAND ORU) (latched ORU) (aligned-to HAND ORU))
            :add-list '((has ORU))
            :del-list '((has NOTHING)))
   (make-op :action 'ungrasp-ORU
            :preconds '((has ORU) (latched ORU))
            :add-list '((has NOTHING))
            :del-list '((has ORU)))
   (make-op :action 'attach-HAND-to-ORU
            :preconds '((has NOTHING) (at HAND ORU) (aligned-to HAND ORU))
            :add-list '((on HAND ORU))
            :del-list '((at HAND ORU)))
   (make-op :action 'attach-ORU-to-PORT1
            :preconds '((has ORU) (at ORU PORT1))
            :add-list '((on ORU PORT1))
            :del-list '((at ORU PORT1)))
   (make-op :action 'attach-ORU-to-PORT2
            :preconds '((has ORU) (at ORU PORT2))
            :add-list '((on ORU PORT2))
            :del-list '((at ORU PORT2)))
   (make-op :action 'detach-HAND-from-ORU
            :preconds '((has NOTHING) (on HAND ORU))
            :add-list '((at HAND ORU))
            :del-list '((on HAND ORU)))
   (make-op :action 'detach-ORU-from-PORT1
            :preconds '((has ORU) (unlatched ORU) (on ORU PORT1))
            :add-list '((at ORU PORT1))
            :del-list '((on ORU PORT1)))
   (make-op :action 'detach-ORU-from-PORT2
            :preconds '((has ORU) (unlatched ORU) (on ORU PORT2))
            :add-list '((at ORU PORT2))
            :del-list '((on ORU PORT2)))
   (make-op :action 'approach-HAND-to-ORU-from-HOME
            :preconds '((has NOTHING) (at HAND HOME))
            :add-list '((at HAND ORU))
            :del-list '((at HAND HOME)))
   (make-op :action 'approach-HAND-to-HOME-from-ORU
            :preconds '((has NOTHING) (at HAND ORU))
            :add-list '((at HAND HOME))
            :del-list '((at HAND ORU)))
   (make-op :action 'approach-ORU-to-PORT2-from-PORT1
            :preconds '((has ORU) (at ORU PORT1) (unlatched ORU))
            :add-list '((at ORU PORT2))
            :del-list '((at ORU PORT1)))
   (make-op :action 'approach-ORU-to-PORT1-from-PORT2
            :preconds '((has ORU) (at ORU PORT2) (unlatched ORU))
            :add-list '((at ORU PORT1))
            :del-list '((at ORU PORT2)))
   (make-op :action 'align-HAND-to-ORU-from-SELF
            :preconds '((has NOTHING) (aligned-to HAND SELF))
            :add-list '((aligned-to HAND ORU))
            :del-list '((aligned-to HAND SELF)))
   (make-op :action 'align-HAND-to-SELF-from-ORU
            :preconds '((has NOTHING) (aligned-to HAND ORU))
            :add-list '((aligned-to HAND SELF))
            :del-list '((aligned-to HAND ORU)))
   (make-op :action 'align-ORU-to-PORT2-from-PORT1
            :preconds '((has ORU) (aligned-to ORU PORT1))
            :add-list '((aligned-to ORU PORT2))
            :del-list '((aligned-to ORU PORT1)))
   (make-op :action 'align-ORU-to-PORT1-from-PORT2
            :preconds '((has ORU) (aligned-to ORU PORT2))
            :add-list '((aligned-to ORU PORT1))
            :del-list '((aligned-to ORU PORT2)))
   ))

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))
    (make-op :action 'shop-installs-battery
         :preconds '(car-needs-battery shop-knows-problem shop-has-money)
         :add-list '(car-works))
    (make-op :action 'tell-shop-problem
         :preconds '(in-communication-with-shop)
         :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
         :preconds '(know-phone-number)
         :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
         :preconds '(have-phone-book)
         :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
         :preconds '(have-money)
         :add-list '(shop-has-money)
         :del-list '(have-money))))
