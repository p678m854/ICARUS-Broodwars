;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  starcraft-auxiliary-functions.lisp
;;  functions to assist with concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function: typeCheck
;;inputs: type = string of name we check
;;        name = name of object checked
;;outputs: bool if name indicates object is the type
(defun typeCheck (type name)
  (let ((pattern (coerce type 'list))
	(nameList (coerce (symbol-name name) 'list)))
    (dolist (key pattern)
      (if (char-equal (pop nameList) key)
	  nil
	(return-from typeCheck nil))))
  t)

;;Function: isWorker
;;inputs: name = name of unit (ex: Protoss_Probe17)
;;        race = race of player
;;Outputs: bool to indicate if worker
(defun isWorker (name race)
  (case race
	(Terran (return-from isWorker (typeCheck "Terran_SCV" name)))
	(Zerg (return-from isWorker (typeCheck "Zerg_Drone" name)))
	(Protoss (return-from isWorker (typeCheck "Protoss_Probe" name)))
	)
  nil
  )

;;Function: isGroundTroop
;;inputs: name = name with type info imbedded
;;outputs: bool is ground troop
;;notes: only first tier ground troops currently
(defun isGroundTroop (name)
  (let ((troopTypes '("Terran_Marine"
		      "Zerg_Zergling"
		      "Protoss_Zealot")
		    ))
    (dolist (troopType troopTypes)
      (if (typeCheck troopType name)
	  (return-from isGroundTroop t)
	nil))
    nil))

;;Function: isGroundTroopProducer
;;inputs: building = name of specific building
;;outputs: bool if ground troop producing
(defun isGroundTroopProducer (building)
  (let ((gtbuildings '("Terran_Barracks"
		       "Zerg_Spawning_Pool"
		       "Protoss_Gateway")
		     ))
    (dolist (gtbuilding gtbuildings)
      (if (typeCheck gtbuilding building)
	  (return-from isGroundTroopProducer t)
	nil))
    nil))

;;Function: isHarvestGasBuilding
;;inputs: building = name of specific building
;;outputs: bool if ground troop producing
(defun isHarvestGasBuilding (building)
  (let ((gasbuildings '("Terran_Refinery"
		       "Zerg_Extractor"
		       "Protoss_Assimilator")
		     ))
    (dolist (gasbuilding gasbuildings)
      (if (typeCheck gasbuilding building)
	  (return-from isHarvestGasBuilding t)
	nil))
    nil))

;;Function: supplyForT1GT (Tier 1 Ground Troops)
;;inputs: supply-used
;;        supply-limit
;;        race
;;output: bool if meets supply limit
(defun suppliesForT1GT (supply-used supply-limit race)
  (case race
	(Terran (>= (- supply-limit supply-used) 1))
	(Zerg (>= (- supply-limit supply-used) 0.5))
	(Protoss (>= (- supply-limit supply-used) 2))
	))

;;Function: supplyTestWorker
;;inputs: supply-used
;;        supply-limit
;;output: bool if meets supply limit
(defun suppliesForWorker (supply-used supply-limit)
  (>= (- supply-limit supply-used) 1))

;;Function: resourcesForWorker
;;inputs: minerals
;;output: bool if meets
(defun resourcesForWorker (minerals)
  (>= minerals 50))

;;Function: resourcesForT1GT
;;inputs: minerals
;;        race
;;output: bool if meets
(defun resourcesForT1GT (minerals race)
  (case race
	(Terran (>= minerals 50))
	(Zerg (>= minerals 25))
	(Protoss (>= minerals 100))
	))

;;Function: wait
;;inputs: time (default 10s)
;;notes: really doing this for the build time. No way currently to re-examine goals
(defun wait (&optional (time 10))
  (sleep time))

;;Function: canProduceWorkers
;;inputs: a building name
;;outputs: bool if they can
(defun canProduceWorkers (building)
  (let ((possibleBuildingList '("Protoss_Nexus"
				"Terran_Command_Center"
				"Zerg_Larva")))
    (dolist (possibleBuilding possibleBuildingList)
      (if (typeCheck possibleBuilding building)
	  (return-from canProduceWorkers t)
	nil))
    nil))

;;Function: canProduceT1GT
;;inputs: a building name
;;outputs: bool if they can
(defun canProduceT1GT (building)
  (let ((possibleBuildingList '("Protoss_Gateway"
				"Terran_Barracks"
				"Zerg_Spawning_Pool")))
    (dolist (possibleBuilding possibleBuildingList)
      (if (typeCheck possibleBuilding building)
	  (return-from canProduceWorkers t)
	nil))
    nil))

;;Function: resourcesForT1GT
;;inputs: minerals
;;        race
;;output: bool if meets
(defun resourcesForGasHarvestor (minerals race)
  (case race
	(Terran (>= minerals 100))
	(Zerg (>= minerals 50))
	(Protoss (>= minerals 100))
	))
