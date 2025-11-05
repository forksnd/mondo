(defsystem "mondo"
  :version "0.1.0"
  :description "Common Lisp REPL interface"
  :author "Eitaro Fukamachi"
  :license "GPL-3.0"
  :depends-on ("swank"
               "usocket"
               "bordeaux-threads"
               "alexandria"
               "cl-readline"
               "cffi"
               "yason")
  :pathname "src"
  :serial t
  :components
  ((:file "utils")
   (:file "logger")
   (:file "color")
   (:file "lisp-impl")
   (:module "sexp"
    :components
    ((:file "parse")
     (:file "indent")))
   (:file "readline")
   (:module "swank-module"
    :pathname "swank"
    :components
    ((:file "server")
     (:file "connection")
     (:file "protocol")
     (:file "client")))
   (:file "swank")
   (:file "debugger")
   (:file "server")
   (:module "vlime"
    :pathname "server/vlime"
    :components
    ((:file "protocol")
     (:file "server")))
   (:file "repl")
   (:file "main")
   (:file "cli"))
  :in-order-to ((test-op (test-op "mondo/tests"))))

(defsystem "mondo/command"
  :depends-on ("mondo")
  :build-operation "program-op"
  :build-pathname "mondo"
  :entry-point "mondo/cli:main")

(defsystem "mondo/tests"
  :depends-on ("mondo"
               "rove"
               "cl-interpol")
  :pathname "tests"
  :components
  ((:module "sexp"
    :components
    ((:file "parse")
     (:file "indent")))
   (:file "utils"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
