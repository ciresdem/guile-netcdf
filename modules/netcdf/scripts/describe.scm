;; describe.scm
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
;; Usage: ncguile describe [ -hv [ args ] ] [ files ]
;;
;; nc - describe.
;;
;; Options:
;;  -l, --list       Output a scheme a-list of the header, instead of CDL format.
;;  -d, --dimensions Display the dimesion information from the header.
;;  -v, --variable   Display the variable information from the header.
;;  -g, --global     Display the global-variable information from the header.
;;  -s, --scan       Scan data for actual variable ranges
;;  -c, --convention Display the netcdf convention associated with the input-file.
;;
;;  --verbose        Increase verbosity [~a].
;;
;;  --version        Display version
;;  --help           Display this help
;;
;;; Code:

(define-module (netcdf scripts describe)
  #:use-module (netcdf netcdf)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 optargs))

(define describe-version "0.1.9")

(define %summary "Describe a netcdf file.")

(define command-synopsis
  '((version (value #f))
    (help (value #f))
    (verbose (value #f))
    (dimensions (single-char #\d) (value #f))
    (variables (single-char #\v) (value #f))
    (global (single-char #\g) (value #f))
    (scan (single-char #\s) (value #f))
    (convention (single-char #\c) (value #f))
    (list (single-char #\l) (value #f))))

;; INFO
(define (display-help)
  (format #t "\
ncinfo.scm input-file [options]

  input-file       The input NetCDF file

 Options:
  -l, --list       Output a scheme a-list of the header, instead of CDL format.
  -d, --dimensions Display the dimesion information from the header.
  -v, --variable   Display the variable information from the header.
  -g, --global     Display the global-variable information from the header.

  -s, --scan       Scan data for actual variable ranges
  -c, --convention Display the netcdf convention associated with the input-file.

  --verbose        Increase verbosity [~a].

  --version        Display version
  --help           Display this help

" nc-verbose))

(define (display-version)
  (format #t "\
describe (guile-netcdf) version ~a
Copyright (c) 2011, 2012, 2013, 2018 Matthew Love

License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
" describe-version))

;; MAIN
(define (nc-describe . args)
  (let* ((options (getopt-long (cons "describe" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (verbose-wanted (option-ref options 'verbose #f))
	  (dims-wanted (option-ref options 'dimensions #f))
	  (vars-wanted (option-ref options 'variables #f))
	  (global-wanted (option-ref options 'global #f))
	  (scan-wanted (option-ref options 'scan #f))
	  (list-wanted (option-ref options 'list #f)))

      (if verbose-wanted
	  (set! nc-verbose #t))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else	
	(let ((input (option-ref options '() #f)))
	  (if (or (not (pair? input)) (not (nc-file? (car input)))) (display-help)
	      (cond 
	       (scan-wanted
		(display-scan-vars (car input)))
	       (list-wanted
		(if (or dims-wanted vars-wanted global-wanted)
		    (nc-write-header-list (car input) dims-wanted vars-wanted global-wanted)
		    (nc-write-header-list (car input) #t #t #t)))
	       ((not list-wanted)
		(if (or dims-wanted vars-wanted global-wanted)
		    (nc-display-header (car input) dims-wanted vars-wanted global-wanted)
		    (nc-display-header (car input) #t #t #t)))
	       (else
		(display-help))))))))))

(define main nc-describe)

(define (display-attribute-list attribute-list)
  (if (pair? attribute-list)
      (begin
	(write (car attribute-list))
	(if (pair? (cdr attribute-list))
	    (if (string? (car attribute-list))
		(begin
		  (display ",")
		  (newline)
		  (display "\t\t\t"))
		(display ", ")))
	(display-attribute-list (cdr attribute-list)))))

(define (display-text-atts attribute-value)
  (if (string? attribute-value)
      (let ((avs (string-split attribute-value #\newline)))
	(if (> (length avs) 1)
	    (display-attribute-list avs)
	    (write (car avs))))
      (display attribute-value)))

(define* (display-atts ncid varid natts #:optional (tatts 0))
  "Display the atts of 'varid and 'ncid"
  (if (> natts 0)
      (begin
	(newline)
	(display "\t\t")
	(if (not (eq? varid nc-global))
	    (display (nc-inq-varname ncid varid))
	    (display ""))
	(display ":")
	(display (nc-inq-attname ncid varid tatts))
	(display " = ")
	(let ((attribute (nc-get-att ncid varid (nc-inq-attname ncid varid tatts))))
	  (if (string? attribute)
	      (display-text-atts attribute)
	      (if (pair? attribute)
		  (display-attribute-list attribute)
		  (display attribute))))
	(display " ;")
	(display-atts ncid varid (- natts 1) (+ 1 tatts)))
      (newline)))

(define (display-dims ncid solo?)
  (begin
    (if (not solo?) ""
	(display (string-append "netcdf " nc-file " {\n")))
    (display "dimensions: \n")
    (with-ncid-dimids 
     (ncid this-dimid)
     (begin 
       (display 
	(string-append 
	 "\t" (nc-inq-dimname ncid this-dimid)
	 " = "
	 (number->string (nc-inq-dimlen ncid this-dimid)) " ;"))
       (newline)))
    (if (not solo?) ""
	(display "}\n"))))

(define (display-vars ncid solo?)
  (begin
    (if (not solo?) ""
	(display (string-append "netcdf " nc-file " {\n")))
    (display "variables: \n")
    (with-ncid-varids 
     (ncid this-varid)
     (let ((var-dimids (nc-inq-vardimid ncid this-varid))
	   (var-natts (nc-inq-varnatts ncid this-varid))
	   (var-nctype (nc-inq-vartype ncid this-varid)))
       (display 
	(string-append 
	 "\t"
	 (nc-display-short-nctype var-nctype)
	 " "
	 (nc-inq-varname ncid this-varid)))
       (display (string-append "(" (string-join (nc-list-varid-dims ncid var-dimids) ", ") ")"))
       (display " ;")
       (display-atts ncid this-varid var-natts)))
    (newline)
    (if (not solo?) ""
	(display "}\n"))))

(define (display-scan-vars nc-file)
  (begin
    (display "variables: \n")
    (with-nc-file 
     (this-ncid nc-file 'read)
     (with-ncid-varids 
      (this-ncid this-varid)
      (let ((var-range (nc-scan-var-range this-ncid this-varid))
	    (var-scale (nc-get-var-scale this-ncid this-varid)))
	(display (string-append (nc-inq-varname this-ncid this-varid) ": "))
	(display (cons (* var-scale (car var-range))
		       (* var-scale (cadr var-range))))
	(newline))))
    (newline)))

(define (display-global ncid solo?)
  (begin
    (if (not solo?) ""
	(display (string-append "netcdf " nc-file " {\n")))
    (display "// global attributes:")
    (display-atts ncid nc-global (nc-inq-varnatts ncid nc-global))
    (newline)
    (if (not solo?) ""
	(display "n"))))

(define (nc-display-header nc-file dims? vars? global?)
  "Display the head associated with 'nc-file"
  (with-nc-file 
   (ncid nc-file 'read)
   (begin
     (display (string-append "netcdf " nc-file " {\n"))
     (if dims?
	 (display-dims ncid #f))
     (if vars?
	 (display-vars ncid #f))
     (if global?
	 (display-global ncid #f))
     (display "}")
     (newline))))

(define (nc-write-header-list nc-file dims? vars? global?)
  (let ((header-list (nc-list-header nc-file)))
    (write 
     (delq '() 
	   (list
	    (if (not dims?) '()
		(assq 'dimensions header-list))
	    (if (not vars?) '()
		(assq 'variables header-list))
	    (if (not global?) '()
		(assq 'global header-list)))))
    (newline)))

;;; End
