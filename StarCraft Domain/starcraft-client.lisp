(ql:quickload :usocket)

(defvar starcraft-conn* nil)

#| Create the TCP client |#

;; host = host ip
;; port = connecting port
(defun tcp-client (host port)
  (usocket:socket-connect host port))

#| Send data through tcp to the server |#
  
;; message = message to send through tcp
;; conn = client connection to send on\
(defun tcp-send (message conn)
  (let (stream)
    (setq stream (usocket:socket-stream conn))
    (clear-output stream)
    (format stream message)
    (format t "Sent out message~%")
    (finish-output stream)))

#| Receive response from server |#

;; conn = client connection to send on  
(defun tcp-receive(conn)
  (let (data)
    (cond 
     ((not (null conn))
      (terpri)
      (format t "Stream established.~%")
      (setq data (read (usocket:socket-stream conn) nil))
      (clear-input (usocket:socket-stream conn)))
     (t
      (error "~S is not a valid connection to server!" conn)))
    data))
	
	
	
#| NOTES
	
	Host in (tcp-client) can be local host "127.0.0.1". This is what worked on Windows
	The server needs to return something thats (read)-able from tcp-receive.
	
|#
