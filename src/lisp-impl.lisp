(defpackage #:mondo/lisp-impl
  (:use #:cl)
  (:export #:normalize-lisp-name
           #:build-swank-command))
(in-package #:mondo/lisp-impl)

(defun normalize-lisp-name (name)
  "Convert Roswell-style names to simple names: sbcl-bin → sbcl"
  (check-type name string)
  (cond
    ((equal name "sbcl-bin") "sbcl")
    ((equal name "ccl-bin") "ccl")
    ((equal name "cmu-bin") "cmucl")
    ((equal name "clasp-bin") "clasp")
    ((equal name "abcl-bin") "abcl")
    (t name)))

(defun lisp-eval-option (impl-name)
  "Return the eval option for the given implementation"
  (cond
    ((member impl-name '("sbcl" "ccl" "abcl") :test #'equal) "--eval")
    ((member impl-name '("allegro" "clasp") :test #'equal) "-e")
    ((equal impl-name "clisp") "-x")
    ((member impl-name '("ecl" "cmucl") :test #'equal) "-eval")
    (t "--eval")))

(defun lisp-load-option (impl-name)
  "Return the load option for the given implementation"
  (cond
    ((member impl-name '("sbcl" "ccl" "ecl" "cmucl" "clasp" "abcl") :test #'equal) "--load")
    ((equal impl-name "allegro") "-L")
    ((equal impl-name "clisp") "-i")
    (t "--load")))

(defun lisp-batch-options (impl-name)
  "Return non-interactive/batch mode options"
  (cond
    ((equal impl-name "sbcl")
     '("--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"))
    ((equal impl-name "ccl")
     '("--no-init" "--quiet" "--batch"))
    ((equal impl-name "clisp")
     '("-norc" "--quiet" "--silent" "-on-error" "exit"))
    ((equal impl-name "allegro")
     '("--qq"))
    ((equal impl-name "cmucl")
     '("-noinit"))
    ((equal impl-name "ecl")
     '("-norc"))
    ((equal impl-name "clasp")
     '("--noinform" "--norc" "--non-interactive"))
    ((equal impl-name "abcl")
     '("--noinform" "--noinit"))
    (t '())))

(defun build-swank-command (impl-name &key port source-registry swank-directory quicklisp-home)
  "Build command-line arguments to start Swank server"
  (check-type impl-name string)
  (check-type port (integer 1 65535))
  (let ((eval-opt (lisp-eval-option impl-name))
        (load-opt (lisp-load-option impl-name)))
    (append
     ;; Batch mode options
     (lisp-batch-options impl-name)

     ;; Load Quicklisp setup.lisp ONLY if quicklisp-home is specified
     (when quicklisp-home
       (let ((setup-file (merge-pathnames "setup.lisp" quicklisp-home)))
         (when (uiop:file-exists-p setup-file)
           (list load-opt (uiop:native-namestring setup-file)))))

     ;; Require ASDF
     (list eval-opt "(require 'asdf)")

     ;; Add user's source-registry to ASDF central registry
     (when source-registry
       (list eval-opt
             (format nil "(push ~S asdf:*central-registry*)"
                     (uiop:native-namestring source-registry))))

     ;; Add mondo's bundled Swank directory and configure swank-loader
     (when swank-directory
       (list eval-opt
             (format nil "(progn (push ~S asdf:*central-registry*) (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (asdf:load-system :swank)) (setf (symbol-value (intern \"*SOURCE-DIRECTORY*\" 'swank-loader)) ~:*~S))"
                     (uiop:native-namestring swank-directory))))

     ;; Load Swank (if not already loaded above)
     (unless swank-directory
       (list eval-opt
             "(if (find :quicklisp *features*) (uiop:symbol-call :ql :quickload :swank :silent t) (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (asdf:load-system :swank)))"))

     ;; Configure Swank
     (list eval-opt
           "(setf swank::*swank-debug-p* nil swank::*log-output* (make-broadcast-stream))")

     (list eval-opt
           "(handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning)) (swank:swank-require 'swank-repl))")

     ;; Start Swank server and keep process alive
     (list eval-opt
           (format nil "(swank:create-server :port ~D :dont-close t)" port))

     ;; Keep the process alive
     (list eval-opt "(loop (sleep 1))"))))
