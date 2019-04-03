;;initialize world
(defun initialize-world ())

;; Connect to Starcraft Host
(defun connect-starcraft-connector (host port)
  (setq starcraft-conn* (tcp-client host port)))

;;Preattend
(defun preattend ()
  (tcp-send "preattend" starcraft-conn*)
  (tcp-receive starcraft-conn*))

;;Update world -> real-time
(defun update-world ()
  (wait 1))

(defun *moveto (unit x y)
  (let ((message (concatenate 'string
			      "moveto "
			      (symbol-name unit)
			      " "
			      (write-to-string x)
			      " "
			      (write-to-string y)
			      "!")))
    (tcp-send message starcraft-conn*)
    (tcp-receive starcraft-conn*)))


;; Gather Command
(defun *gather (worker resource)
  (let ((message (concatenate 'string
			      '"gather "
			      (symbol-name worker)
			      '" "
			      (symbol-name resource)
			      '"!")))
    (tcp-send message starcraft-conn*)))

;; Build Terran-Refinery
(defun *build-gas-harvester (worker x-tile y-tile)
  (let ((message (concatenate 'string
			      '"build_gas_harvester "
			      (symbol-name worker)
			      '" "
			      (write-to-string x-tile)
			      '" "
			      (write-to-string y-tile)
			      '"!")))
    (tcp-send message starcraft-conn*)))

(defun *make-worker (building)
  (let ((message (concatenate 'string
			      '"make_worker "
			      (symbol-name building)
			      '"!")))
    (tcp-send message starcraft-conn*)))

(defun *make-ground-troop (building)
  (let ((message (concatenate 'string
			      '"make_ground_troop "
			      (symbol-name building)
			      '"!")))
    (tcp-send message starcraft-conn*)))

(defun *build-additional-supply (w-unit &optional (x-tile 1) (y-tile 1))
  (let ((message (concatenate 'string
			      '"build_additional_supply "
			      (symbol-name w-unit)
			      '" "
			      ;; x-tile is optional for Zerg
			      (write-to-string x-tile)
			      '" "
			      ;; y-tile is optional for Zerg
			      (write-to-string y-tile)
			      '"!")))
    (tcp-send message starcraft-conn*)))

(defun *build-ground-troop-building (w-unit x-tile y-tile)
    (let ((message (concatenate 'string
			      '"build_ground_troop_building "
			      (symbol-name w-unit)
			      '" "
			      (write-to-string x-tile)
			      '" "
			      (write-to-string y-tile)
			      '"!")))
      (tcp-send message starcraft-conn*)))

(defun *unitAttack (playerUnit enemyUnit)
  (let ((message (concatenate 'string
			      '"attack "
			      (symbol-name playerUnit)
			      '" "
			      (symbol-name enemyUnit)
			      '"!")))
    (tcp-send message starcraft-conn*)))

#|
(defun unitSetAttack (playerUnitSet enemyUnit)
  (loop for playerUnit in playerUnitSet
	do (unitAttack playerunit enemyUnit) ))
|#			      			  

#| 

NOTES:

"preattend", port, and host are all defaults set up in ExampleAIModule.cpp

autoconnects Icarus to Starcraft, future versions may include
multiple Icarus agents for multi-unmanned teams

connect-starcraft-connector is pretty basic for future expansion

#|
