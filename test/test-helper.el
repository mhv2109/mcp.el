
;;; test/test-helper.el

;;; Code:

(defmacro mcp-test-with-mock-connection (connection-var &rest body)
  "Execute BODY with a mock connection bound to CONNECTION-VAR."
  (declare (indent 1))
  `(let* ((mock-buffer (generate-new-buffer " *mock-mcp*"))
          (mock-proc (make-process :name "mock-mcp"
                                   :buffer mock-buffer
                                   :command '("cat")
                                   :connection-type 'pipe))
          (,connection-var (make-instance 'mcp-stdio-process-connection
                                          :name "test"
                                          :process mock-proc
                                          :connection-type 'stdio)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p mock-proc)
         (kill-process mock-proc))
       (when (buffer-live-p mock-buffer)
         (kill-buffer mock-buffer)))))

(provide 'test-helper)
;;; test-helper.el ends here
