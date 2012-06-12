;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:cl-yahoo-finance-query
  :components ((:file "cl-yahoo-finance-query"))
  :depends-on (#:cl-yahoo-finance
	       #:cl-finance-query)
  :name "cl-yahoo-finance-query"
  ;:version "dev"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :license "LLGPL"
  :description "Shim between a generic protocol and cl-yahoo-finance"
  :long-description "Shim between a generic protocol and cl-yahoo-finance")
