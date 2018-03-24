;;; attribute-edit.scm
;;
;; Copyright (c) 2018 Matthew Love <matthew.love@colorado.edu>
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
;; attribute-edit input-file [options]
;;
;;   input-file       The input NetCDF file
;;
;;  Options:
;;   -e, --edit       The attribute editing spec:
;;                    variable-name:attribute-name:attribute-value:attribute-type,...
;;                    e.g. -e nc-global:Title:\"My nc File!\":text,nc-global:version:0.1:double
;;   -a, --append     The attribute appending spec:
;;                    variable-name:attribute-name:attribute-value:attribute-type,...
;;   -d, --delete     The attribute deleting spec:
;;                    variable-name:attribute-name
;;                    e.g. -d nc-global:Title,nc-global:version
;;   -r, --rename     The attribute renaming spec:
;;                    variable-name:attribute-name:new-attribute-name
;;                    e.g. -r nc-global:Title:old_Title,nc-global:version:old_version
;;
;;   --verbose        Increase verbosity [~a].
;;
;;   --version        Display version
;;   --help           Display this help
;;
;;; Code:

(define-module (netcdf scripts attribute-edit)
  #:use-module (ice-9 getopt-long)
  #:use-module (netcdf netcdf))

(define attribute-edit-version "0.0.1")

(define %summary "Edit the attributes of a NetCDF file.")

(define command-synopsis
  '((version (value #f))
    (help (value #f))
    (verbose (value #f))
    (edit (single-char #\e) (value #t))
    (rename (single-char #\r) (value #t))
    (append (single-char #\a) (value #t))
    (range (single-char #\R) (value #t))
    (delete (single-char #\d) (value #t))))

(define (display-help)
  (format #t "\
attribute-edit input-file [options]

  input-file       The input NetCDF file

 Options:
  -e, --edit       The attribute editing spec:
                   variable-name:attribute-name:attribute-value:attribute-type,...
                   e.g. -e nc-global:Title:\"My nc File!\":text,nc-global:version:0.1:double
  -a, --append     The attribute appending spec:
                   variable-name:attribute-name:attribute-value:attribute-type,...
  -d, --delete     The attribute deleting spec:
                   variable-name:attribute-name
                   e.g. -d nc-global:Title,nc-global:version
  -r, --rename     The attribute renaming spec:
                   variable-name:attribute-name:new-attribute-name
                   e.g. -r nc-global:Title:old_Title,nc-global:version:old_version

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
" attribute-edit-version))

(define (attribute-edit . args)
  (let* ((options (getopt-long (cons "attribute-edit" args) command-synopsis)))
    (let ((help-wanted (option-ref options 'help #f))
	  (version-wanted (option-ref options 'version #f))
	  (verbose-wanted (option-ref options 'verbose #f))
	  (input (option-ref options '() #f))
	  (edit (option-ref options 'edit #f))
	  (rename (option-ref options 'rename #f))
	  (append (option-ref options 'append #f))
	  (range (option-ref options 'range #f))
	  (delete (option-ref options 'delete #f)))

      (if verbose-wanted
	  (set! nc-verbose #t))

      (cond
       (version-wanted (display-version))
       (help-wanted (display-help))
       (else	
	(let ((input (option-ref options '() #f)))
	  (if (and (pair? input)
		   (nc-file? (car input)))
	      (cond
	       ;; Append attributes
	       (append
		(nc-append-atts (car input) append)
		(nc-append-history-args (car input) args))
	       ;; Edit attributes
	       (edit
		(nc-edit-atts (car input) edit)
		(nc-append-history-args (car input) args))
	       ;; Delete attributes
	       (delete
		(nc-delete-atts (car input) delete)
		(nc-append-history-args (car input) args))
	       ;; Rename attributes
	       (rename
		(nc-rename-atts (car input) rename)
		(nc-append-history-args (car input) args))
	       (range
		(display-help))
	       (else (display-help)))
	      ;; Display Help
	      (display-help))))))))
;; (nc-append-history (car input) (string-join args " "))))))
;;; End
