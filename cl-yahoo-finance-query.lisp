;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implements the cl-finance-query protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :cl-yahoo-finance-query
  (:use :common-lisp :cl-yahoo-finance :cl-finance-query)
  (:export
   :proxy
   :destroy
   :specialization-options
   :cl-yahoo-finance-querier

   :current-price
   :historical-prices
   :historical-splits
   :company-immutables
   :company-current-statistics
   :company-historical-statistics))

(in-package :cl-yahoo-finance-query)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass cl-yahoo-finance-querier (cl-finance-querier)
  ((proxy :accessor proxy :initform nil :initarg :proxy))
  (:documentation "Yahoo finance class."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod destroy ((obj cl-yahoo-finance-querier))
  "We don't need to destroy any resource"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod specialization-options ( (reader cl-yahoo-finance-querier) &rest specifics)
  "Pass in a proxy address if one exists"
  (setf (proxy reader) (car specifics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-value-at-key (key data)
  "Parses the value in the a-list `data` with `key`.

If value is nil, an error is raised.
If value is N/A, nil is returned
If value is not decimal-convertible, an error is raised
Else, the number is converted to a ratio and returned.
"
  (unless (cdr (assoc key data ))
    (error "key ~a returns nil" key))

  (unless (equal (cdr (assoc key data))
             "N/A")
    ;; This will raise if not numeric.
    (wu-decimal:parse-decimal (cdr (assoc key data )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod current-price ((reader cl-yahoo-finance-querier) symbol-or-symbol-list)
  (let ((alist-data (cl-yahoo-finance:read-current-data-from-csv symbol-or-symbol-list)))
    (loop for data in alist-data
          collect
          (let ((pricing (make-instance 'cl-finance-query:pricing)))
            ;; TODO: MACRO THIS. SO MUCH REPEATING
            (setf (cl-finance-query:ask pricing) (parse-value-at-key :ask data))
            (setf (cl-finance-query:bid pricing) (parse-value-at-key :bid data))
            (setf (cl-finance-query:volume pricing) (parse-value-at-key :volume data))
            (setf (cl-finance-query:short-ratio pricing) (parse-value-at-key :short_ratio data))
            (setf (cl-finance-query:pe-ratio pricing ) (parse-value-at-key :pe_ratio data))
            (setf (cl-finance-query:peg-ratio pricing ) (parse-value-at-key :peg_ratio data))

            (setf (cl-finance-query:year-high pricing )  (parse-value-at-key :high_52_weeks data))
            (setf (cl-finance-query:year-low pricing )  (parse-value-at-key :low_52_weeks data))

            (setf (cl-finance-query:days-high pricing) (parse-value-at-key :high data))
            (setf (cl-finance-query:days-low pricing) (parse-value-at-key :low data))

            (setf (cl-finance-query:dividend-yield pricing)
                  (parse-value-at-key :dividend_yield data))
            (setf (cl-finance-query:eps-estimate-next-quarter pricing)
                  (parse-value-at-key :eps_estimate_next_quarter data))

            ;; Non-numerics
            (setf (cl-finance-query:ebitda pricing )  (cdr (assoc :ebitda data)))
            (setf (cl-finance-query:symbol pricing) (cdr (assoc :symbol data)))
            (setf (cl-finance-query:name pricing) (cdr (assoc :name data)))
            pricing))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod historical-prices ((reader cl-yahoo-finance-querier)
			  symbol
			  start-date
			  end-date)
  "Start-date is a triple mm dd yy format, as is end-date"


  ;; check for errors.
  (flet ((validate-date-triple (date)
           (destructuring-bind (mm dd yy) date
             (when (> mm 12) (error "starting month should be < 12, not ~a" mm))
             (when (> dd 32) (error "starting day should be < 32, not ~a" dd))
             ;; Modern stock market didn't really get rolling until early modern age.
             (when (< yy 1600) (error "starting day should be > 1600, not ~a" yy)))))

    (validate-date-triple start-date)
    (validate-date-triple end-date))

  ;; get the data
  (let ((price-list (rest (cl-yahoo-finance:read-historical-data
                           symbol
                           start-date
                           end-date))))

    (loop for data in price-list
          collect
          (let ((price-point (make-instance 'historical-pricing)))
            ;; Destructure-bind out the data in a given list
            (destructuring-bind (date open high low close volume adj-close)
              data

              (setf (cl-finance-query:name price-point) symbol)
              (setf (cl-finance-query:date price-point) date)
              (setf (cl-finance-query:open-price price-point) open)
              (setf (cl-finance-query:close-price price-point) close)
              (setf (cl-finance-query:high price-point) high)
              (setf (cl-finance-query:low price-point) low)
              (setf (cl-finance-query:volume price-point) volume)
              (setf (cl-finance-query:adjusted-close price-point) adj-close)
              price-point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod historical-splits ((reader cl-yahoo-finance-querier)
			       symbol-or-symbol-list
			       start-date
			       end-date)
  (error "Not implemented"))

(defmethod company-immutables ((reader cl-yahoo-finance-querier)
				symbol-or-symbol-list)
  (error "Not implemented"))

(defmethod company-current-statistics ((reader cl-yahoo-finance-querier)
					symbol-or-symbol-list)
  (error "Not implemented"))

(defmethod company-historical-statistics ((reader cl-yahoo-finance-querier)
					   symbol-or-symbol-list
					   start-date
					   end-date)
  (error "Not implemented"))
