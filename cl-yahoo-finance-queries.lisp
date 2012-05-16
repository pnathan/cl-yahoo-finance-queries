;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implements the cl-finance-query protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :cl-yahoo-finance-query
  (:use :common-lisp)
  (:export
   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
   :company-current-statistics
   :company-historical-statistics))