;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implements the cl-finance-query protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :cl-yahoo-finance-query
  (:use :common-lisp :cl-yahoo-finance :cl-finance-query)
  (:export
   :create
   :destroy
   :specialization-options
   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
   :company-current-statistics
   :company-historical-statistics))

(in-package :cl-yahoo-finance-query)

(defclass cl-yahoo-finance-querier (cl-finance-querier) ())

(defmethod create ()
  (make-instance 'cl-yahoo-finance-querier))

(defmethod destroy ((obj cl-yahoo-finance-querier)) )


(defmethod specialization-options ( (reader cl-yahoo-finance-querier) &rest specifics))

(defmethod current-price ((reader cl-yahoo-finance-querier) symbol-or-symbol-list))

(defmethod historical-prices ((reader cl-yahoo-finance-querier)
			  symbol-or-symbol-list
			  start-date
			  end-date))

(defmethod historical-splits ((reader cl-yahoo-finance-querier)
			       symbol-or-symbol-list
			       start-date
			       end-date))

(defmethod company-immutables ((reader cl-yahoo-finance-querier)
				symbol-or-symbol-list))

(defmethod company-current-statistics ((reader cl-yahoo-finance-querier)
					symbol-or-symbol-list))

(defmethod company-historical-statistics ((reader cl-yahoo-finance-querier)
					   symbol-or-symbol-list
					   start-date
					   end-date))
