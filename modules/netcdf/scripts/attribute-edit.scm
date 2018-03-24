#!/usr/bin/guile \
-e main -s
!#

;; Copyright (c) 2011, 2012 Matthew Love <matth.love@gmail.com>
;; This file is part of guile-netcdf
;; GUILE-NETCDF is liscensed under the GPL v.2 or later and 
;; is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details. 
;; <http://www.gnu.org/licenses/>

;; Modules
(use-modules (ice-9 getopt-long))
;(use-modules (ice-9 format))
;(use-syntax (ice-9 syncase))
;; Load the netcdf module
(use-modules (netcdf netcdf))

(define ncaedit-version "0.1.9")

(define (display-help)
  (format #t "\
ncaedit.scm input-file [options]

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

(define (main args)
  (let* ((option-spec '((version (value #f))
			(help (value #f))
			(verbose (value #f))
			(edit (single-char #\e) (value #t))
			(rename (single-char #\r) (value #t))
			(append (single-char #\a) (value #t))
			(range (single-char #\R) (value #t))
			(delete (single-char #\d) (value #t))))

	 (options (getopt-long args option-spec))
	 (help-wanted (option-ref options 'help #f))
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

    (if (or version-wanted help-wanted)
	(begin
	  (if version-wanted
	      (display-version "ncaedit.scm" ncaedit-version))
	  (if help-wanted
	      (display-help)))

	(if (and (pair? input)
		 (nc-file? (car input)))
	    (begin
	      ;; Append attributes
	      (if append
		  (begin
		    (nc-append-atts (car input) append)
		    (nc-append-history-args (car input) args)))
	      ;; Edit attributes
	      (if edit
		  (begin
		    (nc-edit-atts (car input) edit)
		    (nc-append-history-args (car input) args)))
	      ;; Delete attributes
	      (if delete
		  (begin
		    (nc-delete-atts (car input) delete)
		    (nc-append-history-args (car input) args)))
	      ;; Rename attributes
	      (if rename
		  (begin
		    (nc-rename-atts (car input) rename)
		    (nc-append-history-args (car input) args)))
	      (if range
		  (begin
		    ())))
	    ;; Display Help
	    (display-help)))))
	  ;(nc-append-history (car input) (string-join args " "))))))
;;END
