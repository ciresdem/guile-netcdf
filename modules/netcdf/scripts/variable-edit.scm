#!/usr/bin/guile \
-e main -s
!#

;; Copyright (c) 2011, 2012, 2013 Matthew Love <matth.love@gmail.com>
;; This file is part of guile-netcdf
;; GUILE-NETCDF is liscensed under the GPL v.2 or later and 
;; is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details. 
;; <http://www.gnu.org/licenses/>

;;---BEGIN
;; MODULES
(use-modules (ice-9 getopt-long))
(use-modules (netcdf netcdf))

(define ncvmod-version "0.1.0")

;; INFO
(define (display-help)
  (format #t "\
ncvmod.scm input-file [options]

  input-file      The input NetCDF file

 Options:
  -r, --rename     The variable-renaming spec:
                   variable-name:new-variable-name,...
  -i, --index      The modify-value-by-index spec:
                   variable-name:index0[/index1/...]:value,...

  --verbose        Increase verbosity [~a].

  --version        Display version
  --help           Display this help

" nc-verbose))

;; MAIN
(define (main args)
  (let* ((option-spec '((version (value #f))
			(help (value #f))
			(verbose (value #f))
			(vindex (single-char #\i) (value #t))
			(rename (single-char #\r) (value #t))))

	 (options (getopt-long args option-spec))
	 (help-wanted (option-ref options 'help #f))
	 (version-wanted (option-ref options 'version #f))
	 (verbose-wanted (option-ref options 'verbose #f))
	 (input (option-ref options '() #f))
	 (vindex (option-ref options 'vindex #f))
	 (rename (option-ref options 'rename #f)))

    (if verbose-wanted
	(set! nc-verbose #t))

    (if (or version-wanted help-wanted)
	(begin
	  (if version-wanted
	      (display-version "ncvmod.scm" ncvmod-version))
	  (if help-wanted
	      (display-help)))

	(if (and (pair? input)
		 (nc-file? (car input)))
	    (begin
	      ;; Rename Variable
	      (if rename
		  (begin 
		    (nc-rename-var-spec (car input) rename)
		    (nc-append-history-args (car input) args)))
	      ;; put-var1
	      (if vindex
		  (begin
		    (nc-put-var1-spec (car input) vindex)
		    (nc-append-history-args (car input) args))))

	    ;; Display Help
	    (display-help)))))

;;---END
