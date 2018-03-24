;;*-scheme-*
;;; netcdf.scm
;;
;; Copyright (c) 2011, 2012, 2013, 2016, 2018 Matthew Love <matthew.love@colorado.edu>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

;; Define the modules
(define-module (netcdf netcdf)
  #:use-module (ice-9 format)
  #:export 
  (export 
   guile-netcdf-version
   display-version
   ;; Variables
   nc-global
   nc-verbose
   ;; NetCDF
   nc-inq-libvers
   nc-guile-inq-libvers
   nc-make-nctype
   nc-display-nctype
   nc-display-short-nctype
   ;; Error
   nc-strerror
   ;; Datasets
   nc-file?
   nc-create
   nc-open
   nc-redef
   nc-enddef
   nc-close
   nc-sync
   nc-abort
   ;;---inq
   nc-inq
   nc-inq-ndims
   nc-inq-nvars 
   nc-inq-natts
   nc-inq-unlimdim
   nc-inq-format
   ;; Groups
   nc-inq-ncid
   nc-inq-grps
   nc-inq-dimids
   nc-inq-varids
   ;; Dimensions
   nc-inq-dimid
   nc-inq-dim
   nc-inq-dimname
   nc-inq-dimlen
   ;; Variables
   ;;---inq
   nc-inq-varid
   nc-inq-var
   nc-inq-varname
   nc-inq-vartype
   nc-inq-varndims
   nc-inq-vardimid
   nc-inq-varnatts
   ;;---get
   nc-get-var1
   nc-get-var1-text
   nc-get-var1-uchar
   nc-get-var1-schar
   nc-get-var1-short
   nc-get-var1-ushort
   nc-get-var1-int
   nc-get-var1-uint
   nc-get-var1-long
   nc-get-var1-float
   nc-get-var1-double
   nc-get-var-int
   nc-get-var-long
   nc-get-var-short
   nc-get-var-double
   nc-get-var-float
   nc-get-var
   nc-scan-var-range
   ;;---put
   nc-put-var1
   nc-put-var1-double
   nc-put-var1-float
   nc-put-var1-int
   nc-put-var1-short
   ;;---other
   nc-rename-var
   ;; Attributes
   ;;---inq
   nc-inq-att
   nc-inq-atttype
   nc-inq-attlen
   nc-inq-attname
   ;;---get
   nc-get-att-text
   nc-get-att-string
   nc-get-att-uchar
   nc-get-att-schar
   nc-get-att-short
   nc-get-att-int
   nc-get-att-long
   nc-get-att-float
   nc-get-att-double
   nc-get-att
   ;;---put
   nc-put-att-text
   nc-put-att-double
   ;;--editing
   nc-rename-att
   nc-del-att
   ;; Scheme Functions
   ;;--macros
   with-nc-file
   with-nc-var
   with-ncid-varids
   with-ncid-dimids
   ;;--header info
   nc-list-varid-dims
   nc-list-atts
   nc-list-header
   nc-list-dimensions
   nc-list-variables
   nc-list-global
   ;;--editing
   nc-attribute?
   nc-append-attribute
   nc-edit-attribute
   nc-delete-attribute
   nc-rename-attribute
   nc-append-history
   nc-append-history-args
   nc-var-ref
   parse-att-spec
   nc-append-atts
   nc-rename-atts
   nc-delete-atts
   nc-edit-atts
   nc-get-var-scale
   nc-var-get-scale
   nc-var-get-range
   nc-var-scan-range
   nc-query-var
   nc-rename-var-spec
   nc-put-var1-spec))

;; Load the extension
(load-extension "libguile-netcdf" "scm_init_netcdf")

;;------------;;
;;  Generic   ;;
;;------------;;

(define guile-netcdf-version "0.2.6")

(define (display-version progn progv) 
  (format #t "\
~a version ~a | guile-netcdf version ~a
Copyright (c) 2011, 2012, 2013, 2018 Matthew Love <matthew.love@colorado.edu>
ncinfo.scm is liscensed under the GPL v.2 or later and 
is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 
<http://www.gnu.org/licenses/>
" progn progv guile-netcdf-version))

;;------------;;
;;   MACROS   ;;
;;------------;;

(define-syntax with-nc-file
  (syntax-rules ()
    ((_ (ncid-var nc-path nc-omode)) #f)
    ((_ (ncid-var nc-path nc-omode) exp exp* ...)
     (if (and (file-exists? nc-path) (nc-file? nc-path))
	 (let ((ncid-var (nc-open nc-path nc-omode))
	       (return #f))
	   (if (eq? nc-omode 'write)
	       (begin
		 ;;; Define mode doesn't work with writing new value to variable.
		 (nc-redef ncid-var)
		 (set! return (begin exp exp* ...))
		 (nc-enddef ncid-var))
	       (set! return (begin exp exp* ...)))
	   (nc-close ncid-var)
	   return)
	 #f))))

(define-syntax with-nc-var
  (syntax-rules ()
    ((_ (ncid-var nc-path nc-omode nc-variable nc-varid)) #f)
    ((_ (ncid-var nc-path nc-omode nc-variable nc-varid) exp exp* ...)
     (if (and (file-exists? nc-path) (nc-file? nc-path))
	 (let ((ncid-var (nc-open nc-path nc-omode))
	       (return #f))
	   (let ((nc-varid '()))
	     (if (or (eq? nc-variable nc-global)
		     (equal? nc-variable "nc-global"))
		 (set! nc-varid nc-global)
		 (set! nc-varid (nc-inq-varid ncid-var nc-variable)))
	     (if (eq? nc-omode 'write)
		 (begin
		 ;;; Define mode doesn't work with writing new value to variable.
		   (nc-redef ncid-var)
		   (set! return (begin exp exp* ...))
		   (nc-enddef ncid-var))
		 (set! return (begin exp exp* ...))))
	   (nc-close ncid-var)
	   return)
	 #f))))

(define-syntax with-nc-var1
  (syntax-rules ()
    ((_ (ncid-var1 nc-path nc-omode nc-variable nc-varid) body)
     (with-nc-file (ncid-var1 nc-path nc-omode)
		   (let ((nc-varid '()))
		     (if (or (eq? nc-variable nc-global)
			     (equal? nc-variable "nc-global"))
			 (set! nc-varid nc-global)
			 (set! nc-varid (nc-inq-varid ncid-var1 nc-variable)))
		     body)))))

; Run code 'body using '(ncid dimid), where dimid is a variable
; This will run the given exp on all the available dimensions
; in the given ncid, each variable will be placed in the 
; variable 'dimid
(define-syntax with-ncid-dimids
  (syntax-rules ()
    ((_ (ncid dimid) body)
     (let ((dimids (nc-inq-dimids ncid)))
       (map (lambda (dimid)
	      body)
	    dimids)))))

; Run code 'body using '(ncid varid)
; This will run the given exp on all the available variables 
; in the given ncid, each variable will be placed in the 
; variable 'varid
(define-syntax with-ncid-varids
  (syntax-rules ()
    ((_ (ncid varid) body)
     (let ((varids (nc-inq-varids ncid)))
       (map (lambda (varid)
	      body)
	    varids)))))

(define-syntax nc-var-ref
  (syntax-rules ()
    ((_ ncid variable) #f)
    ((_ ncid variable bound bound* ...)
     (let ((varid (nc-inq-varid ncid variable)))
       (nc-get-var1 ncid varid (list bound bound* ...))))))

;;------------------;;
;; LIST HEADER INFO ;;
;;------------------;;

(define (nc-list-varid-dims ncid dimids)
  "List the dimids of 'ncid"
  (map (lambda (dimid)
	 (nc-inq-dimname ncid dimid))
       dimids))

(define (nc-list-atts ncid varid natts)
  "List the atts of 'varid and 'ncid"
  (if (> natts 0) 
      (cons (list
	     (nc-inq-attname ncid varid (- natts 1))
	     (nc-get-att ncid varid (nc-inq-attname ncid varid (- natts 1))))
       (nc-list-atts ncid varid (- natts 1)))
      '()))

(define (nc-list-header nc-file)
  "Return an alist of the netcdf file header"
  (with-nc-file 
   (this-nc nc-file 'read)
   (let ((these-dimids (nc-inq-dimids this-nc))
	 (these-varids (nc-inq-varids this-nc)))
     (list
      ;; Dimensions
      (cons
       'dimensions
       (with-ncid-dimids 
	(this-nc dimid)
	(cons
	 (nc-inq-dimname this-nc dimid)
	 (nc-inq-dimlen this-nc dimid))))
      ;; Variables
      (cons
       'variables
       (with-ncid-varids 
	(this-nc varid)
	(cons
	 (nc-inq-varname this-nc varid)
	 (cons (cons
		(nc-display-nctype (nc-inq-vartype this-nc varid))
		(nc-list-varid-dims this-nc (nc-inq-vardimid this-nc varid)))
	       (nc-list-atts this-nc varid (nc-inq-varnatts this-nc varid))))))
      ;; Global
      (cons
       'global
       (nc-list-atts this-nc nc-global (nc-inq-varnatts this-nc nc-global)))))))

(define (nc-list-dimensions nc-file)
  "Return an alist of the dimensions in 'nc-file"
  (with-nc-file
   (this-nc nc-file 'read)
   (with-ncid-dimids 
    (this-nc dimid)
    (cons
     (nc-inq-dimname this-nc dimid)
     (nc-inq-dimlen this-nc dimid)))))
   
(define (nc-list-variables nc-file)
  "Return an alist of the variables in 'nc-file"
  (with-nc-file
   (this-nc nc-file 'read)
   (with-ncid-varids 
    (this-nc varid)
    (cons
     (nc-inq-varname this-nc varid)
     (cons 
      (cons
       (nc-display-nctype (nc-inq-vartype this-nc varid))
       (nc-list-varid-dims this-nc (nc-inq-vardimid this-nc varid)))
      (nc-list-atts this-nc varid (nc-inq-varnatts this-nc varid)))))))

(define (nc-list-global nc-file)
  "Return an alist of the global variables in 'nc-file"
  (with-nc-file
   (this-nc nc-file 'read)
   (nc-list-atts this-nc nc-global (nc-inq-varnatts this-nc nc-global))))

;;-----------------------;;
;;   Attribute Editing   ;;
;;-----------------------;;

(define (nc-attribute? ncid varid att-name)
  (if (nc-inq-att ncid varid att-name) #t
      #f))

(define (nc-edit-attribute ncfile variable attribute attribute-value attribute-type append?)
  "Edit the given 'attribute in 'variable and 'ncfile, using 
'attribute-value and attribute-type"
  (cond
   ((eq? 'text attribute-type)
    (if (not append?)
	(nc-delete-attribute ncfile variable attribute))
    (map (lambda (x)
	   (nc-append-attribute ncfile variable attribute x 'text))
	 attribute-value))
   ((eq? 'double attribute-type)
    (if (not append?)
	(nc-delete-attribute ncfile variable attribute))
    (map (lambda (x)
	   (nc-append-attribute ncfile variable attribute x 'double))
	 attribute-value))
   (else
    (display "fail\n"))))
       
(define (nc-append-attribute ncfile variable attribute attribute-value attribute-type)
  (with-nc-var
   (ncid ncfile 'write variable varid)
   (begin
     (if (nc-attribute? ncid varid attribute)
	 ;; if attribute exists, append to it.
	 (let ((old-att (nc-get-att ncid varid attribute)))
	   (cond
	    ((eq? 'double attribute-type)
	     (if (number? attribute-value)
		 (if (list? old-att)
		     (nc-put-att-double ncid varid attribute (append old-att (list attribute-value)))
		     (nc-put-att-double ncid varid attribute (list old-att attribute-value)))
		 (if (list? old-att)
		     (nc-put-att-double ncid varid attribute (append old-att (list (string->number attribute-value))))
		     (nc-put-att-double ncid varid attribute (list old-att (string->number attribute-value))))))
	    ((eq? 'text attribute-type)
	     (nc-put-att-text ncid varid attribute (string-append attribute-value "\n" old-att)))
	    (else
	     (format (current-error-port) "netcdf: Appending is not currently supported for type ~a\n" attribute-type))))
	 ;; if the attribute doesn't exist, make it.
	 (cond
	  ((eq? 'double attribute-type)
	   (if (number? attribute-value)
	       (nc-put-att-double ncid varid attribute attribute-value)
	       (nc-put-att-double ncid varid attribute (string->number attribute-value))))
	  ((eq? 'text attribute-type)
	   (nc-put-att-text ncid varid attribute attribute-value))
	  (else
	   #f))))))

(define (nc-delete-attribute ncfile variable attribute)
  "Delete the given 'attribute in 'variable and 'ncfile"
  (with-nc-var 
   (ncid ncfile 'write variable varid)
     (nc-del-att ncid varid attribute)))

(define (nc-rename-attribute ncfile variable attribute new-attribute)
  "Rename the given 'attribute in 'variable and 'ncfile to 'new-attribute"
  (with-nc-var 
   (ncid ncfile 'write variable varid)
   (nc-rename-att ncid varid attribute new-attribute)))

(define (nc-append-history ncfile history-string)
  ;; Check if global history attribute exists
  (if (with-nc-file (ncid ncfile 'read) (nc-attribute? ncid varid "history"))
      (nc-append-attribute ncfile nc-global "history" history-string 'text)
      (nc-edit-attribute ncfile nc-global "history" (list history-string) 'text #f)))

(define (nc-append-history-args ncfile args)
  ;; Check if global history attribute exists
  (let ((history-string (string-append (basename (car args)) " " (string-join (cdr args) " "))))
    (if (with-nc-file (ncid ncfile 'read) (nc-inq-att ncid nc-global "history"))
	(nc-append-attribute ncfile nc-global "history" history-string 'text)
	(nc-edit-attribute ncfile nc-global "history" (list history-string) 'text #f))))

;; (nc-edit-spec (ncvar ncfile entry-var filespec) (body))
(define-macro (parse-spec file-spec body)
  (let ((file-var (list-ref file-spec 0))
	(file-path (list-ref file-spec 1))
	(entry-var (list-ref file-spec 2))
	(spec (list-ref file-spec 3)))
    `(let ((,file-var ,file-path)
	   (cs-list (string-split ,spec #\,)))
       (map (lambda (x)
	      (let ((,entry-var (string-split x #\:)))
		,body))
	    cs-list))))

(define (nc-append-atts nc-file spec)
  (parse-spec
   (in nc-file entry spec)
   (nc-edit-attribute
    in
    (car entry)
    (cadr entry)
    (string-split (caddr entry) #\/)
    (string->symbol (cadddr entry))
    #t)))

;; spec: "varname:attname:attvalue:atttype,..."
(define (nc-edit-atts nc-file spec)
  (parse-spec 
   (in nc-file entry spec)
   ;(if (eq? 4 (length entry))
       (nc-edit-attribute 
	in 
	(car entry) 
	(cadr entry) 
	(string-split (caddr entry) #\/)
	(string->symbol (cadddr entry))
	#f)))
       ;(format (current-error-port) "ncaedit: ERROR! ~a is not a valid edit spec, \
;make sure there are no commas or colons in the attribute value.\n" entry))))

;; spec: "varname:attname,..."
(define (nc-delete-atts nc-file spec)
  (parse-spec
   (in nc-file entry spec)
   (nc-delete-attribute
    in (car entry) (cadr entry))))

;; spec: "varname:attname:newattname,..."
(define (nc-rename-atts nc-file spec)
  (parse-spec
   (in nc-file entry spec)
   (nc-rename-attribute
    in (car entry) (cadr entry) (caddr entry))))

;; spec: "varname:location(dim/dim):scale"
(define (nc-query-var nc-file spec)
  (parse-spec
   (in nc-file entry spec)
   (let ((loc (map (lambda (x) 
		     (string->number x))
		   (string-split (cadr entry) #\/))))
     (with-nc-var (ncid in 'read (car entry) varid)
		  (if (<= 3 (length entry))
		      (let ((nc-scale (nc-get-var-scale ncid varid)))
			(* nc-scale (nc-get-var1 ncid varid loc)))
		      (nc-get-var1 ncid varid loc))))))

;; spec: "varname:new-varname"
(define (nc-rename-var-spec nc-file spec)
  (parse-spec
   (in nc-file entry spec)
   (with-nc-var (ncid in 'write (car entry) varid)
		(nc-rename-var ncid varid (cadr entry)))))

;; spec: "varname:location(dim/dim:value"
(define (nc-put-var1-spec nc-file spec)
  (parse-spec
   (in nc-file entry spec)
      (let ((loc (map (lambda (x) 
		     (string->number x))
		   (string-split (cadr entry) #\/))))
	(with-nc-var (ncid in 'write-data (car entry) varid)
		     (nc-put-var1 ncid varid loc (string->number (caddr entry)))))))
;;-----------------------;;
;;   Varibale Querying   ;;
;;-----------------------;;

;; (define (nc-scanline ncid varid dims)
;;   (if (>= (car dims -1) 0)
;;       (begin
;; 	(nc-inq-var1 ncid varid (cons (- 1 (car dims) (cadr dims)))))))

;; (define (test-scalnine ncfile variable)
;;    (with-nc-var
;;     (ncid ncfile 'read variable varid)
;;     (let ((dimids (nc-inq-vardimid this-nc varid))
;;  	 (dims '()))
;;       (map (lambda (x)
;;  	    (append dims (nc-inq-dimlen ncid x)))
;;  	  dimids)

;; Check for a scale factor in a variable
(define (nc-var-get-scale ncfile variable)
  (with-nc-var
   (ncid ncfile 'read variable varid)
   (let ((nc-scale (nc-get-att ncid varid "scale_factor")))
     (if nc-scale nc-scale 1.0))))

(define (nc-get-var-scale ncid varid)
  (let ((nc-scale (nc-get-att ncid varid "scale_factor")))
    (if nc-scale nc-scale 1.0)))

(define (nc-var-get-range ncfile variable)
  (with-nc-var
   (ncid ncfile 'read variable varid)
   (let ((var-array (nc-get-var ncid varid))
	 (var-scale (nc-get-var-scale ncid varid)))
     (cons (* var-scale (array-ref (sort var-array <) 0)) 
	   (* var-scale (array-ref (sort var-array >) 0))))))

(define (nc-var-scan-range ncfile variable)
  (with-nc-var
   (ncid ncfile 'read variable varid)
   (let ((var-range (nc-scan-var-range ncid varid))
	 (var-scale (nc-get-var-scale ncid varid)))
     (cons (* var-scale (car var-range))
	   (* var-scale (cadr var-range))))))

;;; End
