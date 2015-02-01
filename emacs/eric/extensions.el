(defvar eric-pre-extensions
  '(
    ;; pre extension erics go here
    )
  "List of all extensions to load before the packages.")

(defvar eric-post-extensions
  '(
    ;; post extension erics go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function eric/init-<extension-eric>
;;
;; (defun eric/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
