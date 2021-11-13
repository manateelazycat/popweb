;;; epcs.el --- EPC Server              -*- lexical-binding: t; -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(defmacro popweb-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar popweb-deferred-debug nil
  "Debug output switch.")

(defvar popweb-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun popweb-deferred-log (&rest args)
  "[internal] Debug log function."
  (when popweb-deferred-debug
    (with-current-buffer (get-buffer-create "*popweb-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" popweb-deferred-debug-count (apply #'format args)))))
    (cl-incf popweb-deferred-debug-count)))

(defvar popweb-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(defmacro popweb-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`popweb-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal popweb-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar popweb-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar popweb-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `popweb-deferred-post-task' and `popweb-deferred-worker'.")

(defun popweb-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`popweb-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack popweb-deferred-queue)
    (popweb-deferred-log "QUEUE-POST [%s]: %s" (length popweb-deferred-queue) pack)
    (run-at-time popweb-deferred-tick-time nil 'popweb-deferred-worker)
    d))

(defun popweb-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when popweb-deferred-queue
    (let* ((pack (car (last popweb-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq popweb-deferred-queue (nbutlast popweb-deferred-queue))
      (condition-case err
          (setq value (popweb-deferred-exec-task d which arg))
        (error
         (popweb-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: popweb-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `popweb-deferred-resignal')
;; cancel      : a canceling function (default `popweb-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct popweb-deferred-object
  (callback 'identity)
  (errorback 'popweb-deferred-resignal)
  (cancel 'popweb-deferred-default-cancel)
  next status value)

(defun popweb-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun popweb-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (popweb-deferred-log "CANCEL : %s" d)
  (setf (popweb-deferred-object-callback d) 'identity)
  (setf (popweb-deferred-object-errorback d) 'popweb-deferred-resignal)
  (setf (popweb-deferred-object-next d) nil)
  d)

(defun popweb-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (popweb-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "popweb-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (popweb-deferred-object-callback d)
                    (popweb-deferred-object-errorback d)))
        (next-deferred (popweb-deferred-object-next d)))
    (cond
     (callback
      (popweb-deferred-condition-case err
                                      (let ((value (funcall callback arg)))
                                        (cond
                                         ((popweb-deferred-object-p value)
                                          (popweb-deferred-log "WAIT NEST : %s" value)
                                          (if next-deferred
                                              (popweb-deferred-set-next value next-deferred)
                                            value))
                                         (t
                                          (if next-deferred
                                              (popweb-deferred-post-task next-deferred 'ok value)
                                            (setf (popweb-deferred-object-status d) 'ok)
                                            (setf (popweb-deferred-object-value d) value)
                                            value))))
                                      (error
                                       (cond
                                        (next-deferred
                                         (popweb-deferred-post-task next-deferred 'ng err))
                                        (t
                                         (popweb-deferred-log "ERROR : %S" err)
                                         (message "deferred error : %S" err)
                                         (setf (popweb-deferred-object-status d) 'ng)
                                         (setf (popweb-deferred-object-value d) err)
                                         err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (popweb-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (popweb-deferred-resignal arg)))))))

(defun popweb-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (popweb-deferred-object-next prev) next)
  (cond
   ((eq 'ok (popweb-deferred-object-status prev))
    (setf (popweb-deferred-object-status prev) nil)
    (let ((ret (popweb-deferred-exec-task
                next 'ok (popweb-deferred-object-value prev))))
      (if (popweb-deferred-object-p ret) ret
        next)))
   ((eq 'ng (popweb-deferred-object-status prev))
    (setf (popweb-deferred-object-status prev) nil)
    (let ((ret (popweb-deferred-exec-task next 'ng (popweb-deferred-object-value prev))))
      (if (popweb-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun popweb-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-popweb-deferred-object :callback callback)
    (make-popweb-deferred-object)))

(defun popweb-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (popweb-deferred-exec-task d 'ok arg))

(defun popweb-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (popweb-deferred-exec-task d 'ng arg))

(defun popweb-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (popweb-deferred-post-task d 'ok arg))

(defun popweb-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (popweb-deferred-callback-post (popweb-deferred-new callback))."
  (let ((d (if callback
               (make-popweb-deferred-object :callback callback)
             (make-popweb-deferred-object))))
    (popweb-deferred-callback-post d arg)
    d))

(defun popweb-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-popweb-deferred-object :callback callback)))
    (popweb-deferred-set-next d nd)))

(defun popweb-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-popweb-deferred-object :errorback callback)))
    (popweb-deferred-set-next d nd)))

(defvar popweb-epc-debug nil)

(defun popweb-epc-log (&rest args)
  (when popweb-epc-debug
    (with-current-buffer (get-buffer-create "*popweb-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun popweb-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar popweb-epc-uid 1)

(defun popweb-epc-uid ()
  (cl-incf popweb-epc-uid))

(defvar popweb-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct popweb-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun popweb-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return popweb-epc-connection object."
  (popweb-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (popweb-epc-uid))
         (connection-name (format "epc con %s" connection-id))
         (connection-buf (popweb-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-popweb-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (popweb-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (popweb-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (popweb-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun popweb-epc-process-sentinel (connection process msg)
  (popweb-epc-log "!! Process Sentinel [%s] : %S : %S"
                  (popweb-epc-connection-name connection) process msg)
  (popweb-epc-disconnect connection))

(defun popweb-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (popweb-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (popweb-epc-connection-process connection)))
    (popweb-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun popweb-epc-disconnect (connection)
  (let ((process (popweb-epc-connection-process connection))
        (buf (popweb-epc-connection-buffer connection))
        (name (popweb-epc-connection-name connection)))
    (popweb-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (popweb-epc-log "!! Disconnected finished [%s]" name)))

(defun popweb-epc-process-filter (connection process message)
  (popweb-epc-log "INCOMING: [%s] [%S]" (popweb-epc-connection-name connection) message)
  (with-current-buffer (popweb-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (popweb-epc-process-available-input connection process)))

(defun popweb-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (popweb-deferred-new callback)
             (popweb-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun popweb-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (popweb-deferred-callback-post d event))))

(defun popweb-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (popweb-epc-net-have-input-p)
      (let ((event (popweb-epc-net-read-or-lose process))
            (ok nil))
        (popweb-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'popweb-epc-signal-send
                         (cons (popweb-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (popweb-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (popweb-epc-process-available-input connection process)))))))

(defun popweb-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (popweb-epc-net-decode-length))))

(defun popweb-epc-net-read-or-lose (_process)
  (condition-case error
      (popweb-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun popweb-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (popweb-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun popweb-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun popweb-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct popweb-epc-manager
  "Root object that holds all information related to an EPC activity.

`popweb-epc-start-epc' returns this object.

title          : instance name for displaying on the `popweb-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : popweb-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct popweb-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar popweb-epc-live-connections nil
  "[internal] A list of `popweb-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun popweb-epc-server-process-name (uid)
  (format "popweb-epc-server:%s" uid))

(defun popweb-epc-server-buffer-name (uid)
  (format " *%s*" (popweb-epc-server-process-name uid)))

(defun popweb-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (popweb-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (popweb-epc-disconnect (popweb-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 popweb-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq popweb-epc-live-connections (delete mngr popweb-epc-live-connections))
    ))

(defun popweb-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun popweb-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an popweb-epc-connection instance."
  (let* ((mngr mngr)
         (conn (popweb-epc-manager-connection mngr))
         (channel (popweb-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (popweb-epc-log "SIG CALL: %S" args)
                    (apply 'popweb-epc-handler-called-method ,mngr (popweb-epc-args args))))
               (return
                . (lambda (args)
                    (popweb-epc-log "SIG RET: %S" args)
                    (apply 'popweb-epc-handler-return ,mngr (popweb-epc-args args))))
               (return-error
                . (lambda (args)
                    (popweb-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'popweb-epc-handler-return-error ,mngr (popweb-epc-args args))))
               (epc-error
                . (lambda (args)
                    (popweb-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'popweb-epc-handler-epc-error ,mngr (popweb-epc-args args))))
               (methods
                . (lambda (args)
                    (popweb-epc-log "SIG METHODS: %S" args)
                    (popweb-epc-handler-methods ,mngr (caadr args))))
               ) do
             (popweb-epc-signal-connect channel method body))
    (push mngr popweb-epc-live-connections)
    mngr))

(defun popweb-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (popweb-epc-manager-connection mngr)))
    (popweb-epc-net-send conn (cons method messages))))

(defun popweb-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (popweb-epc-manager-methods mngr)
           if (eq method-name (popweb-epc-method-name i))
           do (cl-return i)))

(defun popweb-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (popweb-epc-manager-methods mngr)
                  collect
                  (list
                   (popweb-epc-method-name i)
                   (or (popweb-epc-method-arg-specs i) "")
                   (or (popweb-epc-method-docstring i) "")))))
    (popweb-epc-manager-send mngr 'return uid info)))

(defun popweb-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (popweb-epc-manager-methods mngr))
           (method (popweb-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (popweb-epc-log "ERR: No such method : %s" name)
        (popweb-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (popweb-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((popweb-deferred-object-p ret)
                (popweb-deferred-nextc ret
                                       (lambda (xx) (popweb-epc-manager-send mngr 'return uid xx))))
               (t (popweb-epc-manager-send mngr 'return uid ret))))
          (error
           (popweb-epc-log "ERROR : %S" err)
           (popweb-epc-manager-send mngr 'return-error uid err))))))))

(defun popweb-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (popweb-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (popweb-epc-manager-sessions mngr) ret)))

(defun popweb-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (popweb-epc-manager-sessions mngr))))
    (cond
     (pair
      (popweb-epc-log "RET: id:%s [%S]" uid args)
      (popweb-epc-manager-remove-session mngr uid)
      (popweb-deferred-callback (cdr pair) args))
     (t                                 ; error
      (popweb-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun popweb-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (popweb-epc-manager-sessions mngr))))
    (cond
     (pair
      (popweb-epc-log "RET-ERR: id:%s [%S]" uid args)
      (popweb-epc-manager-remove-session mngr uid)
      (popweb-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (popweb-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun popweb-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (popweb-epc-manager-sessions mngr))))
    (cond
     (pair
      (popweb-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (popweb-epc-manager-remove-session mngr uid)
      (popweb-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (popweb-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun popweb-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (popweb-epc-uid))
        (sessions (popweb-epc-manager-sessions mngr))
        (d (popweb-deferred-new)))
    (push (cons uid d) sessions)
    (setf (popweb-epc-manager-sessions mngr) sessions)
    (popweb-epc-manager-send mngr 'call uid method-name args)
    d))

(defun popweb-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-popweb-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (popweb-epc-manager-methods mngr))))
    (setf (popweb-epc-manager-methods mngr) methods)
    method))

(defun popweb-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'popweb-epc-nothing))
    (popweb-deferred-chain
     d
     (popweb-deferred-nextc it
                            (lambda (x) (setq result x)))
     (popweb-deferred-error it
                            (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'popweb-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (popweb-epc-connection-process (popweb-epc-manager-connection mngr))
         0 popweb-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun popweb-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (popweb-epc-sync mngr (popweb-epc-call-deferred mngr method-name args)))

(defun popweb-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (popweb-epc-connection-process (popweb-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar popweb-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`popweb-epc-manager' instance]).
When the server process accepts the client connection, the
`popweb-epc-manager' instance is created and stored in this variable
`popweb-epc-server-client-processes'. This variable is used for the management
purpose.")

;; popweb-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `popweb-epc-manager' instances
(cl-defstruct popweb-epc-server name process port connect-function)

(defvar popweb-epc-server-processes nil
  "[internal] A list of ([process object] . [`popweb-epc-server' instance]).
This variable is used for the management purpose.")

(defun popweb-epc-server-get-manager-by-process (proc)
  "[internal] Return the popweb-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in popweb-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun popweb-epc-server-accept (process)
  "[internal] Initialize the process and return popweb-epc-manager object."
  (popweb-epc-log "POPWEB-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (popweb-epc-uid))
         (connection-name (format "epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-popweb-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (popweb-epc-log "POPWEB-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (popweb-epc-process-filter connection p m)))
    (set-process-sentinel process
                          (lambda (p e)
                            (popweb-epc-process-sentinel connection p e)))
    (make-popweb-epc-manager :server-process process :port t
                             :connection connection)))

(defun popweb-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (popweb-epc-log "POPWEB-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (popweb-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (popweb-epc-server-accept process)))
            (push (cons process mngr) popweb-epc-server-client-processes)
            (popweb-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (popweb-epc-log "POPWEB-EPC-SERVER- Protocol error: %S" err)
         (popweb-epc-log "POPWEB-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process popweb-epc-server-client-processes)) _d)
        (when pair
          (popweb-epc-log "POPWEB-EPC-SERVER- DISCONNECT %S" process)
          (popweb-epc-stop-epc (cdr pair))
          (setq popweb-epc-server-client-processes
                (assq-delete-all process popweb-epc-server-client-processes))
          ))
      nil))))

(defun popweb-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (popweb-epc-uid)))
       (buf (popweb-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :sentinel
         (lambda (process message)
           (popweb-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-popweb-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          popweb-epc-server-processes)
    main-process))

(provide 'popweb-epc)
;;; popweb-epc.el ends here
