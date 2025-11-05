(defpackage #:mondo/swank/server
  (:use #:cl)
  (:shadowing-import-from #:mondo/logger
                          #:log)
  (:import-from #:mondo/utils
                #:random-port)
  (:import-from #:mondo/lisp-impl
                #:normalize-lisp-name
                #:build-swank-command)
  (:import-from #:usocket)
  (:export #:create-swank-server
           #:swank-server
           #:swank-server-process
           #:swank-server-host
           #:swank-server-port
           #:make-swank-server))
(in-package #:mondo/swank/server)

(defstruct swank-server
  process
  host
  port)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)
    (usocket:connection-reset-error () nil)))

(defun try-start-swank-server (lisp port source-registry swank-directory quicklisp-home)
  "Attempt to start a Swank server with the given configuration"
  (let* ((command-args (build-swank-command lisp
                                            :port port
                                            :source-registry source-registry
                                            :swank-directory swank-directory
                                            :quicklisp-home quicklisp-home))
         (command (cons lisp command-args))
         (process (handler-case
                      (progn
                        (log :debug "Command: ~{~S~^ ~}" command)
                        (uiop:launch-program command
                                             :input :stream
                                             :output nil
                                             :error-output nil))
                    (error (e)
                      (log :error "Failed to launch ~A: ~A" lisp e)
                      (uiop:quit -1)))))
    (values process
            (loop repeat 300
                  do (sleep 0.1)
                  when (server-running-p port)
                    do (return :success)
                  unless (uiop:process-alive-p process)
                    do (return :failed)
                  finally (return :timeout)))))

(defun find-mondo-swank-directory ()
  "Find Swank installation in mondo's .qlot directory"
  (let ((mondo-dir (asdf:system-source-directory :mondo)))
    (when mondo-dir
      (let ((slime-software-dir (merge-pathnames #P".qlot/dists/quicklisp/software/" mondo-dir)))
        (when (uiop:directory-exists-p slime-software-dir)
          ;; Find directory matching slime-v*
          (let ((slime-dirs (uiop:subdirectories slime-software-dir)))
            (find-if (lambda (dir)
                       (let ((dirname (car (last (pathname-directory dir)))))
                         (and (stringp dirname)
                              (uiop:string-prefix-p "slime-v" dirname)
                              (uiop:file-exists-p (merge-pathnames "swank.asd" dir)))))
                     slime-dirs)))))))

(defun create-swank-server (&key lisp source-registry quicklisp port)
  (let* ((lisp (or lisp "sbcl"))
         (normalized-lisp (normalize-lisp-name lisp))
         (port (or port (random-port)))
         (swank-directory (find-mondo-swank-directory)))
    (log :debug "Starting a swank server on ~A at port=~A" normalized-lisp port)

    (multiple-value-bind (process status)
        (try-start-swank-server normalized-lisp port source-registry swank-directory quicklisp)
      (cond
        ((eq status :success)
         (make-swank-server :process process
                            :host "127.0.0.1"
                            :port port))
        (t
         (log :error "~A"
              (case status
                (:failed "Failed to start a swank server")
                (:timeout "Took too long for the server to start. Timeout.")))
         (uiop:quit -1))))))
