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

;; TODO: account for scale-factor and offset

;;---BEGIN
;; MODULES
(use-modules (netcdf netcdf))
;(use-modules (ice-9 format))
(use-modules (ice-9 getopt-long))

(define ncquery-version "0.1.5")

;; INFO
(define (display-help)
  (format #t "\
ncquery.scm input-file [options]

  input-file      The input NetCDF file

 Options:
  -i, --index      The query-by-index spec:
                   variable-name:index0[/index1/...][:(apply-scale)],...
                   e.g. -i z:100/1230:,z:200/1230:
                   will return the value located at index 100/1230 
                   and 200/1230 in the z variable, and will apply 
                   the scale_factor found for that variable if aplicable.

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
			(location (single-char #\l) (value #t))))

	 (options (getopt-long args option-spec))
	 (help-wanted (option-ref options 'help #f))
	 (version-wanted (option-ref options 'version #f))
	 (verbose-wanted (option-ref options 'verbose #f))
	 (input (option-ref options '() #f))
	 (vindex (option-ref options 'vindex #f))
	 (location (option-ref options 'location #f)))

    (if verbose-wanted
	(set! nc-verbose #t))

    (if (or version-wanted help-wanted)
	(begin
	  (if version-wanted
	      (display-version "ncquery.scm" ncquery-version))
	  (if help-wanted
	      (display-help)))
	(begin
	  (cond 
	   ((and (pair? input) 
		 (nc-file? (car input))
		 vindex)
	    (begin
	      (let ((loc-val (nc-query-var (car input) vindex)))
		(if loc-val
		    (display loc-val)
		    (display (nan)))
		(newline))))
	   (else
	    (display-help)))))))
;;---END