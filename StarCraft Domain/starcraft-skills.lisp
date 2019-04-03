;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  starcraft-skills.lisp
;;  basic skills
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-skills

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Resource skills
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;Gather minerals
 ((gatherMinerals ?worker)
  :elements ((self ?self))
  :conditions ((idleWorker ?worker)
	       (mineralField ?m))
  :actions ((*gather ?worker ?m))
  :effects ((gatheringMinerals ?self)
	    (not (idleWorker ?worker))
	    (fullProduction ?self))
  )

 ;;Gather gas
 ((gatherGas ?worker)
  :elements ((self ?self))
  :conditions ((canHarvestGas ?self)
	       (idleWorker ?worker)
	       (vespeneGas ?g))
  :actions ((*gather ?worker ?g))
  :effects ((gatheringGas ?self)
	    (not (idleWorker ?worker))
	    (fullProduction ?self))
  )

 ((waitingOnMinerals ?self)
  :elements ((self ?self))
  :conditions ((fullProduction ?self)
	       (gatheringMinerals ?self)
	       (not (resourcesForWorker ?self)))
  :actions ((wait 5))
  :effects ((resourcesForWorker ?self))
  )

 ((waitingOnMinerals ?self)
  :elements ((self ?self))
  :conditions ((fullProduction ?self)
	       (gatheringMinerals ?self)
	       (not (resourcesForGasHarvester ?self)))
  :actions ((wait 5))
  :effects ((resourcesForGasHarvester ?self))
  )
 
 ((waitingOnMinerals ?self)
  :elements ((self ?self))
  :conditions ((fullProduction ?self)
	       (gatheringMinerals ?self)
	       (not (resourcesForT1GT ?self)))
  :actions ((wait 5))
  :effects ((resourcesForT1GT ?self))
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Building skills
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ((buildGasHarvester ?self ?worker ?gas)
  :elements ((self ?self)
	     (neutral ?gas x ?x y ?y))
  :conditions ((hasWorkers ?self)
	       (resourcesForGasHarvester ?self)
	       (vespeneGas ?gas)
	       (worker ?worker))
  :actions ((*build-gas-harvester ?worker ?x ?y)
	    (wait 10));; average time to get there
  :effects ((canHarvestGas ?self))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Unit making skills
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ((makeWorker ?building)
  :elements ((self ?self)
	     (building ?building))
  :conditions ((supplyForWorker ?self)
	       (resourcesForWorker ?self)
	       (workerProducer ?building)
	       (not (idleWorker ?worker)))
  :actions ((*make-worker ?building))
  :effects ((hasWorkers ?self)
	    (beingConstructed ?worker)
	    (worker ?worker))
  )

 ((waitingOnWorkerConstruction ?worker)
  :conditions ((worker ?worker)
	       (beingConstructed ?worker))
  :actions ((wait 4))
  :effects ((idleWorker ?worker))
  )

 ((makeT1GroundTroop ?building)
  :elements ((self ?self)
	     (building ?building))
  :conditions ((supplyForT1GT ?self)
	       (resourcesForWorker ?self)
	       (workerProducer ?building))
  :actions ((*make-ground-troop ?building)
	    (wait 20))
  :effects ((hasWorkers ?self)
	    (isGroundTroop ?troop))
  )
 )


