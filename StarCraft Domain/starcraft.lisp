;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  starcraft.lisp
;;  Domain for StarCraft: BroodWars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "Domains/StarCraft/starcraft-auxiliary-functions")
(load "Domains/StarCraft/starcraft-concepts")
(load "Domains/StarCraft/starcraft-skills")
(load "Domains/StarCraft/starcraft-client")
(load "Domains/StarCraft/starcraft-client-setup")

(create-problems ((canHarvestGas Icarus)))

;;Automize the server connection to BWAPI
(defvar port 27015);Defualt in BWAPI
(defvar host "127.0.0.1");Default in Windows
(connect-starcraft-connector host port);Autoconnect


