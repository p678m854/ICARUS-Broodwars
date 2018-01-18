;; Connect to Starcraft Host
(defun connect-starcraft-connector (host port)
  (setq starcraft-conn* (tcp-client host port)))

;;Preattend
(defun preattend ()
  (tcp-send "preattend" starcraft-conn*)
  (tcp-receive starcraft-conn*))

;;Automize the server connection to BWAPI
(defvar port 27015);Defualt in BWAPI
(defvar host "127.0.0.1");Default in Windows
(connect-starcraft-connector host port);Autoconnect

#| NOTES:

"preattend", port, and host are all defaults set up in ExampleAIModule.cpp

autoconnects Icarus to Starcraft, future versions may include
multiple Icarus agents for multi-unmanned teams

connect-starcraft-connector is pretty basic for future expansion

#|
