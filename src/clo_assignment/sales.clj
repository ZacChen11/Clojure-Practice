(ns a2.sales)
  (require '[clojure.string :as str])

; convert string to integer
(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))
    
;read input file
(defn read-file [file-path]
  (def data-map {})
  (def input-file (slurp file-path))
  (def input-file (str/split input-file #"\n"))
  (doseq [each-entry input-file]
    [
     (def record (str/split each-entry #"\|"))
     (def ID (parse-int(get record 0)))
     (def data-map (assoc data-map ID record))
    ]
   )
  (def data-map (into (sorted-map) data-map))
  (let [result data-map] result)
 )

; load three files 
(def cust (read-file "cust.txt"))
(def prod (read-file "prod.txt"))
(def sales (read-file "sales.txt"))

;display customer or product table
(defn display-data [data]
  (def id-keys (keys data))
  (doseq [id id-keys]
    [
     (def record (subvec (get data id) 1))
     (def record (str/join ", " record))
     (println id ":" record )
     ])
  )

;Display Customer Table
;(display-data cust)

;Display Product Table
;(display-data prod)

;display sales table
(defn display-sales-table [cust, prod, sales]
   (def cust-keys (keys cust))
   (def prod-keys (keys prod))
   (def sales-keys (keys sales))
   (doseq [sale-id sales-keys]
     [
      (def record (get sales sale-id))
      (def cust-id (parse-int(get record 1)))
      (def prod-id (parse-int(get record 2)))
      (def item-count (get record 3))
      (def cust-name (get (get cust cust-id) 1))
      (def prod-name (get (get prod prod-id) 1))
      (println sale-id ":" cust-name"," prod-name"," item-count)
      ])
 )

;Display Sales Table
;(display-sales-table cust prod sales)

;compute total sales for customer
(defn total-sales [cust-name cust prod sales]
  (def cust-id nil)
  (doseq [cust-record-map cust]
    [(def cust-record (get cust-record-map 1))
    (def cust-record-name (get cust-record 1))
    (if (= cust-name cust-record-name) 
      (do (def cust-id (parse-int(get cust-record 0)))))]
   )
  ;(println "cust id" cust-id)
  (def record-container [])
  (doseq [shopping-record sales]
    [(def sale-record (get shopping-record 1))
     (def sale-cust-id (parse-int(get sale-record 1)))
     (if (= cust-id sale-cust-id)
       (do (def record-container (conj record-container sale-record))))]
   )
  ;(println record-container)
  (def all-items-cost 0)
  (doseq [entry record-container]
    [(def prod-id (parse-int(get entry 2)))
     (def prod-record (get prod prod-id))
     (def prod-price (read-string (get prod-record 2)))
     (def item-count (parse-int (get entry 3)))
     (def total-cost (* prod-price item-count))
     (def all-items-cost (+ all-items-cost total-cost))]
   )
  (if (nil? cust-id)
    (do
      (println cust-name "is Invalid.")
      (println "Input a customer in the following list:")
      (def ckeys (keys cust))
      (doseq [cid ckeys]
        [(def cname (get (get cust cid) 1))
         (println cname)] )
      )      
    (println cust-name ": $" all-items-cost))   
 )

;Total Sales for Customer
;(total-sales "Sue Jones" cust prod sales)

; compute total count for one product
(defn total-count-for-product [prod-name cust prod sales]
  (def prod-id nil)
  (doseq [prod-record-map prod]
  [(def prod-record (get prod-record-map 1))
  (def prod-record-name (get prod-record 1))
  (if (= prod-name prod-record-name) 
    (do (def prod-id (parse-int(get prod-record 0)))))]
  )
  ;(println "prod id" prod-id)
  (def prod-count 0)
  (doseq [shopping-record sales]
    [(def sale-record (get shopping-record 1))
     (def sale-prod-id (parse-int(get sale-record 2)))
     (if (= prod-id sale-prod-id)
       (do (def item-count (parse-int (get sale-record 3))) 
         (def prod-count (+ prod-count item-count))))]
   )
  (if (nil? prod-id)
    (do
      (println prod-name "is Invalid.")
      (println "Input a product in the following list:")
      (def pkeys (keys prod))
      (doseq [pid pkeys]
        [(def pname (get (get prod pid) 1))
         (println pname)] )
      )      
    (println prod-name ":" prod-count))
)

;Total Count for Product
;(total-count-for-product "gum" cust prod sales)

;main memu content
(defn show-menu []
  (def flag true)
  (while(= flag true)
    (def menu [" "
               "*** Sales Menu ***"
               "------------------"
               "1. Display Customer Table"
               "2. Display Product Table" 
               "3. Display Sales Table" 
               "4. Total Sales for Customer" 
               "5. Total Count for Product" 
               "6. Exit"
               " "
               "Enter an option? "])
    (doseq [item menu](println item))
    (let [op (read-line)] 
      (println ">>" op)
      (condp = op
        "1" (display-data cust)
        "2" (display-data prod)
        "3" (display-sales-table cust prod sales)
        "4" (do (println "Enter a customer?")
              (let [cst-name (read-line)]
                (println ">>" cst-name)
                (total-sales cst-name cust prod sales)))
        "5" (do (println "Enter a product?")
              (let [prd-name (read-line)]
                (println ">>" prd-name)
                (total-count-for-product prd-name cust prod sales)))
        "6" (def flag false)
        (println "Invalid Input: Enter an option(1-6)."))
      )
    )
)
 
;show the main menu
(show-menu) 
(println "Good Bye!")