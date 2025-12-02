;;; test/test-jsonprc.el --- Tests for stdio message format fix

(require 'ert)
(require 'mcp)
(require 'test-helper)

;;; Code:

(ert-deftest mcp-test-stdio-message-format ()
  "Test stdio notification format: valid JSON with LF-only terminator.
This is the primary regression test for the CRLF → LF fix.
Consolidates format validation, JSON parsing, and character counting."
  (let ((sent-data nil))
    ;; Mock process-send-string to capture what's sent
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (proc string)
                 (setq sent-data string))))

      (mcp-test-with-mock-connection conn
                                     ;; Send a notification via stdio
                                     (jsonrpc-connection-send conn :id 1 :method "request" :params '(:param "value"))

                                     ;; Verify the sent data exists
                                     (should sent-data)
                                     (should (stringp sent-data))

                                     ;; 1. Message ends with \n
                                     (should (string-suffix-p "\n" sent-data))

                                     ;; 2. Message does NOT end with \r\n (the bug this fixes)
                                     (should-not (string-suffix-p "\r\n" sent-data))

                                     ;; 3. Message contains no carriage returns at all
                                     (should-not (string-match-p "\r" sent-data))

                                     ;; 4. Verify it's properly formatted JSON followed by single newline
                                     (should (string-match "\\`{.*}\\s-*\n\\'" sent-data))

                                     ;; 5. Validate JSON parsing
                                     (let ((json-string (string-trim sent-data)))
                                       (should (json-parse-string json-string)))

                                     ;; 6. Character counting validation
                                     (let ((newline-count (cl-count ?\n sent-data))
                                           (cr-count (cl-count ?\r sent-data)))
                                       (should (= newline-count 1))
                                       (should (= cr-count 0)))))))

(provide 'test-jsonrpc)
;;; test-jsonrpc.el ends here
