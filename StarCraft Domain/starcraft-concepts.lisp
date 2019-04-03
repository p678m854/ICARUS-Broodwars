;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  starcraft-concepts.lisp
;;  basic concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-concepts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;;Win condition
 ((victorious ?self)
  :elements ((self ?self isVictorious ?yesno))
  :tests ((= ?yesno 1))
  )
 
 ;;Enemy organic
 ((enemy ?enemy)
  :elements ((self ?self)
	     (organic ?enemy player ?rival))
  :tests ((not (equal ?self ?rival)))
  )
 ;;Enemy robotic
 ((enemy ?enemy)
  :elements ((self ?self)
	     (robotic ?enemy player ?rival))
  :tests ((not (equal ?self ?rival)))
  )
 ;;Enemy building
 ((enemy ?enemy)
  :elements ((self ?self)
	     (building ?enemy player ?rival))
  :tests ((not (equal ?self ?rival)))
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ((playerUnit ?unit)
  :elements ((self ?self)
	     (building ?unit player ?self))
  )
 ((playerUnit ?unit)
  :elements ((self ?self)
	     (organic ?unit player ?self))
  )
  ((playerUnit ?unit)
  :elements ((self ?self)
	     (robotic ?unit player ?self))
  )
 
 ;;Robotic worker version (Protoss)
 ((worker ?worker ?task)
  :elements ((self ?self)
	     (robotic ?worker player ?self assignment ?task))
  :tests ((typeCheck "Protoss_Probe" ?worker))
  )
 ;;Terran
 ((worker ?worker ?task)
  :elements ((self ?self)
	     (organic ?worker player ?self assignment ?task))
  :tests ((typeCheck "Terran_SCV" ?worker))
  )
 ;;Zerg
 ((worker ?worker ?task)
  :elements ((self ?self)
	     (organic ?worker player ?self assignment ?task))
  :tests ((typeCheck "Zerg_Drone" ?worker))
  )

 ;;Buildings that generate workers
 ((workerProducer ?building)
  :conditions ((playerUnit ?building))
  :tests ((canProduceWorkers ?building))
  )
 
 ;;Buildings that generate workers
 ((groundTroopProducer ?building)
  :conditions ((playerUnit ?building))
  :tests ((canProduceT1GT ?building))
  )

 ;;organic being generated
 ((beingConstructed ?unit)
  :elements ((organic ?unit assignment ?task))
  :conditions ((playerUnit ?unit))
  :tests ((eq ?task 'under-construction))
  )
 ;;robotic being generated
 ((beingConstructed ?unit)
  :elements ((robotic ?unit assignment ?task))
  :conditions ((playerUnit ?unit))
  :tests ((eq ?task 'under-construction))
  )
 ;;building being generated
 ((beingConstructed ?unit)
  :elements ((building ?unit assignment ?task))
  :conditions ((playerUnit ?unit))
  :tests ((eq ?task 'under-construction))
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player abilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;;Can harvest Vespene Gas
 ((canHarvestGas ?self)
  :elements ((self ?self)
	     (building ?building player ?self))
  :tests ((isHarvestGasBuilding ?building))
  )
 
 
 ;;Has workers (organic versions)
 ((hasWorkers ?self)
  :elements ((self ?self)
	     (organic ?worker player ?self))
  :conditions ((worker ?worker))
  )
 
 ;;Has workers (robotic versions)
 ((hasWorkers ?self)
  :elements ((self ?self)
	     (robotic ?worker player ?self))
  :conditions ((worker ?worker))
  )

 ;;Can build ground troops
 ((canBuildGroundTroops ?self)
  :elements ((self ?self)
	     (building ?building))
  :tests ((isGroundTroopProducer ?building))
  )
 
 ;;Has ground troops
 ((hasGroundTroops ?self)
  :elements ((self ?self))
  :conditions  ((groundTroop ?any))
  )
 
 ;;Has minerals for worker
 ((resourcesForWorker ?self)
  :elements ((self ?self minerals ?m))
  :tests ((resourcesForWorker ?m))
  )
 
 ;;Has minerals/gas for T1 Ground troops
 ((resourcesForT1GT ?self)
  :elements ((self ?self minerals ?m race ?r))
  :tests ((resourcesForT1GT ?m ?r))
  )
 
 ;;Has supply for worker
 ((supplyForWorker ?self)
  :elements ((self ?self supply-used ?su supply-limit ?sl))
  :tests ((suppliesForWorker ?su ?sl))
  )

 ((resourcesForGasHarvester ?self)
  :elements ((self ?self minerals ?m race ?r))
  :tests ((resourcesForGasHarvestor ?m ?r))
  )
 
 ;;Has supply for T1 Ground troops
 ((supplyForT1GT ?self)
  :elements ((self ?self supply-used ?su supply-limit ?sl race ?r))
  :tests ((suppliesForT1GT ?r ?su ?sl))
  )
 
 ;;Safe
 ((safe ?self)
  :elements ((self ?self))
  :conditions ((not (enemy ?any)))
  )
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Resource Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;;Minerals check
 ((mineralField ?m)
  :elements ((neutral ?m))
  :tests ((typeCheck "Resource_Mineral_Field" ?m))
  )
 
 ;;Gas check
 ((vespeneGas ?g)
  :elements ((neutral ?g))
  :tests ((typeCheck "Resource_Vespene_Geyser" ?g))
  )

 ;;Gas Refinery
 ((gasRefinery ?self ?building)
  :elements ((self ?self)
	     (building ?building player ?self))
  :tests ((isHarvestGasBuilding ?building))
  )
 
 ;;Robotic worker version (Protoss)
 ((idleWorker ?worker)
  :elements ((robotic ?worker assignment ?task))
  :conditions ((worker ?worker))
  :tests ((eq ?task 'idle))
  )
 
 ;;Organic worker version (Terran)
 ((idleWorker ?worker)
  :elements ((organic ?worker assignment ?task))
  :conditions ((worker ?worker))
  :tests ((eq ?task 'idle))
  )
 
 ;;Gathering minerals
 ((gatheringMinerals ?self)
  :elements ((self ?self))
  :conditions ((worker ?worker ?a))
  :tests ((eq ?a 'gatherMinerals))
  )
 
 ;;Gathering gas
 ((gatheringGas ?self)
  :elements ((self ?self))
  :conditions ((worker ?worker ?a))
  :tests ((eq ?a 'gatherVespeneGas))
  )
 
 
 ;;All workers busy
 ((fullProduction ?self)
  :elements ((self ?self))
  :conditions ((hasWorkers ?self)
	       (not (idleWorker ?any)))
  )
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Troop Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((groundTroop ?troop)
  :elements ((self ?self)
	     (organic ?troop player ?self))
  :tests ((isGroundTroop ?troop))
  )
 )
