;;; test/run-tests.el --- Test runner for mcp.el

;;; Code:

;; Add parent directory to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Load dependencies
(require 'mcp)

;; Load all test files
(dolist (file '("test-helper"
                "test-jsonrpc"))
  (load (expand-file-name file (file-name-directory load-file-name))))

;; Run tests
(ert-run-tests-batch-and-exit)
;;; run-tests.el ends here
