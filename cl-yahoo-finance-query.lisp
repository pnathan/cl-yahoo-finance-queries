;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implements the cl-finance-query protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :cl-yahoo-finance-query
  (:use :common-lisp :cl-yahoo-finance :cl-finance-query)
  (:export
   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
   :company-current-statistics
   :company-historical-statistics))

(in-package :cl-yahoo-finance-query)


(defmethod specialization-options (&rest specifics))

(defmethod current-price (reader symbol-or-symbol-list))

(defmethod historical-prices (reader
			  symbol-or-symbol-list
			  start-date
			  end-date))

(defmethod historical-splits (reader
			       symbol-or-symbol-list
			       start-date
			       end-date))

(defmethod company-immutables (reader
				symbol-or-symbol-list))

(defmethod company-current-statistics (reader
					symbol-or-symbol-list))

(defmethod company-historical-statistics (reader
					   symbol-or-symbol-list
					   start-date
					   end-date))
