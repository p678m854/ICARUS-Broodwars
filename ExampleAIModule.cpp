//TCP Headers
#undef UNICODE
#define WIN32_LEAN_AND_MEAN
#define DEFAULT_BUFLEN 512
#define DEFAULT_PORT "27015"
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <thread>
//End TCP Headers
//Start ExampleAIModule.cpp
#include "ExampleAIModule.h"
#include "BWEM/src/bwem.h" //BWEM header
#include <iostream>

#pragma comment (lib, "Ws2_32.lib")

using namespace BWAPI;
using namespace Filter;

//BWEM Add-on
using namespace BWEM;
namespace { auto & theMap = BWEM::Map::Instance(); }
//using namespace BWEM::BWAPI_ext;
//using namespace BWEM::utils;

//Start TCP Server (CHOI)
//Theading/TCP Host Server
std::thread* server;
static void startServer(GameWrapper& bw);
static void startServer(GameWrapper& bw)
{
	bw << "Starting server..." << std::endl;
	int iResult;
	WSADATA wsaData;

	SOCKET ListenSocket = INVALID_SOCKET;
	SOCKET ClientSocket = INVALID_SOCKET;

	struct addrinfo *result = NULL;
	struct addrinfo hints;

	int iSendResult;
	char recvbuf[DEFAULT_BUFLEN];
	int recvbuflen = DEFAULT_BUFLEN;

	//messages
	char* preattend = "preattend";
	//Unit Commands
	char* moveto = "moveto";
	char* gather = "gather";
	char* attack = "attack";
	//Building Constructors
	char* buildGasHarvester = "build_gas_harvester";
	char* buildAdditionalSupply = "build_additional_supply";
	char* buildGroundTroopBuilding = "build_ground_troop_building";
	//Unit Constructors
	char* makeWorker = "make_worker";
	char* makeGroundTroop = "make_ground_troop";
	//Resources
	char mineral[] = "mineral";
	char* mineral_ptr = mineral;
	char vespene[] = "resource_vespene_geyser";
	char* vespene_ptr = vespene;
	//TCP deliminators
	char* space = " ";
	char* endComm = "!";

	// Message Lengths
	const int mineral_l = sizeof(mineral)-2;
	const int vespene_l = sizeof(vespene);
	const int buildGasHarvester_l = sizeof(buildGasHarvester) - 2;
	const int makeWorker_l = sizeof(makeWorker) - 2;

	//Initialize Winsock
	iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);

	if (iResult != 0)
	{
		bw << "WSAStartup failed with error: " << iResult << std::endl;
		return;
	}

	bw << "WSAStartup success!" << std::endl;

	ZeroMemory(&hints, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = IPPROTO_TCP;

	//Resolve the server address and port
	iResult = getaddrinfo(NULL, DEFAULT_PORT, &hints, &result);

	if (iResult != 0) {
		bw << "getaddrinfo failed with error: " << iResult << std::endl;
		WSACleanup();
		return;
	}

	bw << "getaddrinfo success!" << std::endl;

	//Create a SOCKET for connecting to server
	ListenSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
	if (ListenSocket == INVALID_SOCKET) {
		bw << "socket failed with error: " << WSAGetLastError << std::endl;
		freeaddrinfo(result);
		WSACleanup();
		return;
	}

	bw << "Listen socket success!" << std::endl;

	//Setup the TCP listening socket
	iResult = bind(ListenSocket, result->ai_addr, (int)result->ai_addrlen);
	if (iResult == SOCKET_ERROR) {
		bw << "bind failed with error: " << WSAGetLastError() << std::endl;
		freeaddrinfo(result);
		WSACleanup();
		return;
	}

	bw << "Bind listen socket success!" << std::endl;

	freeaddrinfo(result);

	iResult = listen(ListenSocket, SOMAXCONN);
	if (iResult == SOCKET_ERROR) {
		bw << "listen failed with error: " << WSAGetLastError() << std::endl;
		closesocket(ListenSocket);
		WSACleanup();
		return;
	}

	bw << "Listening for connections (blocking) ..." << std::endl;
	//Accept a client socket
	ClientSocket = accept(ListenSocket, NULL, NULL);

	if (ClientSocket == INVALID_SOCKET) {
		bw << "accept failed with error: " << WSAGetLastError() << std::endl;
		closesocket(ListenSocket);
		WSACleanup();
		return;
	}

	bw << "Accepted client success!" << std::endl;
	closesocket(ListenSocket); //No longer need server socket

							   //Recieve until the peer shuts down the connection
	do
	{
		iResult = recv(ClientSocket, recvbuf, recvbuflen, 0);
		if (iResult > 0) {
			bw << "Bytes received: " << iResult << std::endl;
			// Preattend
			if (strcmp(preattend, recvbuf) == 0) {
				//Iterate all the players in the game using a std:: iterator
				Playerset players = bw->getPlayers();
				std::string percepts = "(";
				std::vector<int> knownAreas;
				for (auto p : players) {
					// Only print the player if they are not an observer
					if (!p->isObserver()) {
						bw << p->getName() << ", playing as " << p->getRace() << std::endl;
					}
					if (Broodwar->self()->getName() == p->getName()) {
						percepts += "(self (" + p->getName() + ") race " + p->getRace().getName()
							+ " minerals " + std::to_string(p->minerals())
							+ " gas " + std::to_string(p->gas())
							+ " supply-limit " + std::to_string(p->supplyTotal())
							+ " supply-used " + std::to_string(p->supplyUsed()) + 
							+ " isVictorious " + std::to_string(p->isVictorious()) + ")\n";
					}

					Unitset playerUnits = p->getUnits();
					for (auto &u : playerUnits)
					{
						Position pos = u->getPosition();//{X,Y}
						TilePosition tpos = u->getTilePosition(); //Get tile position of unit
						const BWEM::Area *u_area = theMap.GetNearestArea(tpos); // Area of tile position
						if (knownAreas.empty()) {
							knownAreas.push_back(u_area->Id()); // No known area condition
						}
						else
						{
							bool newAreaFlag = true; // Assumes new area
							for (int area : knownAreas) {
								if (area == (u_area->Id()))
								{
									newAreaFlag = false; // Found the area in previous list
									break; // Breaks checking the known areas
								}
							}
							if (newAreaFlag) {
								knownAreas.push_back(u_area->Id()); // Adds new area to list
							}
						}

						//Match unit to type
						if (u->getType().isMineralField())
						{
							percepts += "(mineral mineral" + std::to_string(u->getID()) + " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y)
								+ " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isAddon())
						{
							percepts += "(addon add" + std::to_string(u->getID()) + " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y)
								+ " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isBeacon())
						{
							percepts += "(beacon beacon" + std::to_string(u->getID()) + " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y)
								+ " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isNeutral())
						{
							percepts += "(neutral " + u->getType().getName() + std::to_string(u->getID())
								+ " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y) + " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isBuilding())
						{
							percepts += "(building " + u->getType().getName() + std::to_string(u->getID())
								+ " player (" + p->getName() + ") area" + std::to_string(u_area->Id())
								+ " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y);
							if ((Broodwar->self()->getName()) == (p->getName())) {
								percepts += " assignment ";
								if (u->isIdle()) {
									if(!u->isCompleted()){
										percepts += "under-construction";
									}
									else {
										percepts += "idle";
									}
								}
								else if (u->isTraining()) {
									percepts += "(";
									auto t = u->getTrainingQueue();
									for (unsigned int i = 0; i < t.size(); i++) {
										percepts += t[i].getName();
										if (i < t.size() - 1) {
											percepts += " ";
										}
									}
									percepts += ")";
								}
								else {
									percepts += "busy";
								}
							}
							percepts += ")\n";
						}
						else if (u->getType().isRobotic())
						{
							percepts += "(robotic " + u->getType().getName() + std::to_string(u->getID()) + " player (" + p->getName()
								+ ") area area" + std::to_string(u_area->Id()) + " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y);
							if ((Broodwar->self()->getName()) == (p->getName())) {
								percepts += " assignment ";
								if (u->isIdle()) {
									percepts += "idle";
								}
								else {
									if (u->getType().isWorker()) {
										if (u->getBuildType().getName() == "None") {
											percepts += "gathering-resources";
										}
										else {
											percepts += "constructing-";
											percepts += u->getBuildType().getName();
										}
									}
									else {
										percepts += "busy";
									}
								}
							}
							percepts += ")\n";
						}
						else if (u->getType().isOrganic())
						{
							percepts += "(organic " + u->getType().getName() + std::to_string(u->getID()) + " player (" + p->getName()
								+ ") area area" + std::to_string(u_area->Id()) + " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y);
							if (Broodwar->self()->getName() == p->getName()) {
								percepts += " assignment ";
								if (u->isIdle()) {
									percepts += "idle";
								}
								else {
									if (u->getType().isWorker()) {
										if (u->getBuildType().getName() == "None") {
											percepts += "gathering-resources";
										}
										else {
											percepts += "constructing-";
											percepts += u->getBuildType().getName();
										}
									}
									else if (u->isMorphing()) {
										percepts += "morphing";
									}
									else {
										percepts += "busy";
									}
								}
							}
							percepts += ")\n";
						}
						else if (u->getType().isPowerup())
						{
							percepts += "(powerup powerup" + std::to_string(u->getID()) + " name " + u->getType().getName()
								+ " x " + std::to_string(tpos.x) + " y " + std::to_string(tpos.y) + " area area" + std::to_string(u_area->Id()) + ")\n";
						}
					}
				}
				//Terrain Analyzer concepts
				std::vector<int> cp_list = { 0 };//List of already done chokepoints (zero doesn't exist)
				for (const BWEM::Area & area : theMap.Areas()) { //Iterate through the areas
					for (const ChokePoint * cp : area.ChokePoints()) {
						int cp_index = cp->Index();//Current Index Number
						bool cp_flag = true;       //Default ChokePoint isn't in list already done
						for (int index : cp_list) {
							if (index == cp_index) {//If CP is already done
								cp_flag = false;    //Flag set to false
								break;              //Break out of list check
							}
						}
						if (cp_flag == true) {//If new chokepoint
							cp_list.push_back(cp_index);                   //Add to list
							std::pair<const Area *, const Area *> cp_areas;//2 Areas that it connects
							cp_areas = cp->GetAreas();
							bool knownAreaFlag = false;
							for (int area : knownAreas) {
								if ((cp_areas.first->Id() == area) || (cp_areas.second->Id() == area)) {
									knownAreaFlag = true;
									break;
								}
							}
							if (knownAreaFlag) {
								WalkPosition cpPos = cp->Center();
								percepts += "(ChokePoint CP" + std::to_string(cp->Index())
									+ " x " + std::to_string(cpPos.x /4) + " y " + std::to_string(cpPos.y /4) //Going from Walk to Tile
									+ " A1 Area" + std::to_string(cp_areas.first->Id())
									+ " A2 Area" + std::to_string(cp_areas.second->Id()) + " Access ";
								if ((cp->IsPseudo()) == false) {
									percepts += "Open)\n";                     //if chokepoint is open
								}
								else {										 //If chokepoint is blocked
									Neutral *blocker = cp->BlockingNeutral();//Neutral that's blocking
									Unit b_unit = blocker->Unit();           //Going from BWEM to BWAPI classes
									UnitType b_UnitType = blocker->Type();
									std::string b_ID = std::to_string(b_unit->getID());
									percepts += b_UnitType.getName();
									/*
									if (blocker->IsMineral() != nullptr) {
										percepts += " mineral";
									}
									else if (b_UnitType.isAddon()) {
										percepts += " add";
									}
									else if (b_UnitType.isBeacon()) {
										percepts += " beacon";
									}
									else if (b_UnitType.isNeutral()) {
										percepts += " neutral";
									}
									else if (b_UnitType.isBuilding()) {
										percepts += " building";
									}
									else if (b_UnitType.isOrganic()) {
										percepts += " organic";
									}
									else if (b_UnitType.isPowerup()) {
										percepts += " powerup";
									}
									*/
									percepts += b_ID + ")\n";
								}
							}
						}
					}
				}
				percepts += ")/n";
				iSendResult = send(ClientSocket, percepts.c_str(), percepts.length(), 0);
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			//moveto icarus command
			else if (strncmp(moveto, recvbuf, 6) == 0) {
				//Processing command
				char targetName[30];
				char xpos_c[4];
				char ypos_c[4];
				char *Reader1 = strpbrk(recvbuf,space);
				char *Reader2 = strpbrk(++Reader1,space);
				int targetNameLen = Reader2 - Reader1;
				//std::cout << "Length of Target Name: " << targetNameLen << std::endl;
				for (int i = 0; i < targetNameLen; i++)
				{
					targetName[i] = *Reader1++;
				}
				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, space);
				int xpos_cLen = Reader2 - Reader1;
				for (int i = 0; i < xpos_cLen; i++) {
					xpos_c[i] = *Reader1++;
				}
				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);
				int ypos_cLen = Reader2 - Reader1;
				for (int i = 0; i < ypos_cLen; i++) {
					ypos_c[i] = *Reader1++;
				}
				std::string xpos_s = xpos_c;
				std::string ypos_s = ypos_c;
				std::string::size_type sz;
				int p_x = std::stoi(xpos_s, &sz);
				int p_y = std::stoi(ypos_s, &sz);
				BWAPI::TilePosition p_desired = BWAPI::Point<int, TILEPOSITION_SCALE>(p_x, p_y);
				for (auto u : Broodwar->self()->getUnits()) {
					std::string u_name = u->getType().getName() + std::to_string(u->getID());
					int u_nameLength = u_name.length();
					if (u_nameLength == targetNameLen) {
						if (strncmp(u_name.c_str(), targetName, targetNameLen) == 0) {
							u->move(BWAPI::Point<int, 1>(p_x*TILEPOSITION_SCALE, p_y*TILEPOSITION_SCALE));
							break;
						}
					}
				}
				//Debugging text
				/*
				bw << "(Moveto ";
				for (int i = 0; i < targetNameLen; i++) {
					bw << targetName[i];
				} 
				bw << " x ";
				for (int i = 0; i < xpos_cLen; i++) {
					bw << xpos_c[i];
				}
				bw << " y ";
				for (int i = 0; i < ypos_cLen; i++) {
					bw << ypos_c[i];
				}
				bw << ") Command Recieved" << std::endl;
				bw << "Unit Selected: ";
				for (int i = 0; i < targetNameLen; i++) {
					bw << targetName[i];
				}
				bw << std::endl;
				bw << "Tile Position: " << p_desired << std::endl;
				*/
				//Confirming on LISP Terminal
				char* confMT = "(Moveto Command Recieved)/n";
				int confMT_len = strlen(confMT);
				iSendResult = send(ClientSocket, confMT, confMT_len, 0);
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			//gather icarus command
			else if (strncmp(gather, recvbuf, 6) == 0) {
				//Setting up memory allocation and reading pointers
				char workerName[30];
				char resourceName[40];
				char resourceID_c[3];//Hold numeric characters for ID
				int resourceID_n = 0;//Numeric resource ID
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, space);

				//Getting worker name (case sensitive)
				int workerNameLength = Reader2 - Reader1;
				for (int i = 0; i < workerNameLength; i++) {
					workerName[i] = *Reader1++;
				}
				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);
				int resourceNameLength = Reader2 - Reader1;

				//Getting resource name (necessary to lowercase?)
				for (int i = 0; i < resourceNameLength; i++) {
					resourceName[i] = tolower(*Reader1++);
				}

				//Flags for finding workers and resources
				bool workerFound = false;
				bool resourceFound = false;
				
				// Debugging text
				
				bw << "Looking for: ";
				for (int i = 0; i < workerNameLength; i++) {
					bw << workerName[i];
				}
				bw << std::endl;
				// Iterate through ICARUS's units
				for (auto u : Broodwar->self()->getUnits()) {
					// finding workers
					if (u->getType().isWorker())
					{
						std::string w_name = u->getType().getName() + std::to_string(u->getID());
						if (strncmp(w_name.c_str(),workerName,workerNameLength) == 0)
						{
							bw << "Found Worker Unit " << w_name << std::endl;
							workerFound = true;
							bw << "Looking for: ";
							for (int i = 0; i < resourceNameLength; i++) {
								bw << resourceName[i];
							}
							bw << std::endl;

							bool mineral_f;
							// Is resource a mineral?
							if (strncmp(mineral_ptr, resourceName, mineral_l) == 0) {
								mineral_f = true;
							}
							else {
								mineral_f = false;
							}
							//Debugging text output:
							/*bw << "MINERAL(?): ";
							if (mineral_f) {
								bw << "TRUE";
							}
							else {
								bw << "FALSE";
							}
							bw << std::endl;*/
							//Mineral Option
							if(mineral_f)
							{
								int id_counter = 0;// Counter to see how long the ID at the end of resource is
								while (isdigit(resourceName[mineral_l+id_counter+1]))
								{
									resourceID_c[id_counter] = resourceName[mineral_l + id_counter + 1];
									id_counter++;
								}
								// Iterate through minerals in map
								for (auto &r : Broodwar->getMinerals()){
									int r_ID = r->getID(); //Get id of mineral
									std::string r_name_ID = std::to_string(r_ID); //String of ID to compare to TCP result
									int num_digits;//How many digits (base 10) are being checked
									if (r_ID != 0) {
										num_digits = ceil(log10(r_ID));//Get digits if ID isn't zero
									}
									else {
										num_digits = 1;// if ID is 0
									}
									if (r_ID == 1 || r_ID == 10 || r_ID == 100) {
										num_digits++;// Case where ceil() doesn't change anythin
									}
									char r_name_ID_c[3];
									for (int i = 0; i < 3; i++) {
										r_name_ID_c[i] = r_name_ID[i];
									}
									// if the number of digits in the resource id match the digits in the desired id
									if (num_digits == id_counter) {
										// check digits left to right
										for (int i = 0; i < num_digits; i++) {
											//If mismatch
											if (r_name_ID[i] != resourceID_c[i]) {
												break;//
											}
											// Mismatch isn't triggered and i has reached end of digits [shifted to 0 indexing]
											else if (i == (num_digits-1)) {
												resourceFound = true; //Found the resource
												u->gather(r);//Execute the command (desired_worker -> gather(desired_resource)
											}
										}
										// The resouce has been found
										if (resourceFound) {
											break; // Stop Iterating through the loop
										}
									}
								}
							}
							// Vespene Gas Option
							else if(strncmp(resourceName,vespene_ptr,vespene_l) == 0){
								for (auto &r : Broodwar->getGeysers()) {
									std::string r_name = r->getType().getName() + std::to_string(r->getID());
									if (strncmp(r_name.c_str(), resourceName, resourceNameLength) == 0) {
										resourceFound = true;
										u->gather(r);
										break;
									}
								}
							}
							//Debugging text
							/*
							if (!resourceFound) {
								bw << "Looked for: ";
								for (int i = 0; i < resourceNameLength; i++) {
									bw << resourceName[i];
								}
								bw << std::endl;
								bw << "Resource was not found" << std::endl;
							}
							else {
								bw << "Resource was found" << std::endl;
							}
							*/
							break;
						}
					}
				}
				// Debugging stuff:
				if (!workerFound) {
					bw << "Worker not found: "<< workerName << std::endl;
					bw << "Listed Player Workers: " << std::endl;
					for (auto &u : Broodwar->self()->getUnits()) {
						if (u->getType().isWorker()) {
							bw << "\t" << u->getType().getName() << std::to_string(u->getID()) << std::endl;
						}
					}
				}
				// Cleaning up pointers and arrays
				/*
				delete Reader1;
				delete Reader2;
				delete[] workerName;
				delete[] resourceID_c;
				delete[] resourceName;*/
				// clearing the receiver buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			//Building Refinary/Assimulator/Excavator
			else if (strncmp(buildGasHarvester,recvbuf,19) == 0) {
				//Setting up memory allocation and reading pointers
				char workerName[30];
				char xPos_c[10];
				char yPos_c[10];
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, space);

				//Getting worker name (case sensitive)
				int workerNameLength = Reader2 - Reader1;
				for (int i = 0; i < workerNameLength; i++) {
					workerName[i] = *Reader1++;
				}

				bw << "Worker for construction: " << std::endl;
				for (int i = 0; i < workerNameLength; i++) {
					bw << workerName[i];
				}
				bw << std::endl;
				
				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, space);
				
				//Getting x tile position
				int xPos_l = Reader2 - Reader1;
				for (int i = 0; i < xPos_l; i++) {
					xPos_c[i] = *Reader1++;
				}

				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);

				//Getting y tile position
				int yPos_l = Reader2 - Reader1;
				for (int i = 0; i < yPos_l; i++) {
					yPos_c[i] = *Reader1++;
				}

				// character arrays to strings
				//std::string workerName_s = workerName;
				std::string xPos_s = xPos_c;
				std::string yPos_s = yPos_c;

				bw << "Position for Gas Harvesting building" << std::endl;
				bw << "X: " + xPos_s << std::endl;
				bw << "Y: " + yPos_s << std::endl;

				// Generate desired Tile Position
				std::string::size_type sz;
				TilePosition futureBuildingPos(std::stoi(xPos_s,&sz), std::stoi(yPos_s,&sz));
				bw << "TilePosition: " << futureBuildingPos << std::endl;
				
				//Preallocate a gas harvester building type
				UnitType gasHarvester;

				for (auto u : Broodwar->self()->getUnits()) {
					if (u->getType().isWorker()) {
						std::string u_name = u->getType().getName() + std::to_string(u->getID());
						if (strncmp(u_name.c_str(),workerName,workerNameLength) == 0) {
							//Determine Race Specific Building Unit Type
							if (Broodwar->self()->getRace().getName() == "Terran")
							{
								gasHarvester = UnitType{ 110 }; //Terran_Refinary
							}
							else if (Broodwar->self()->getRace().getName() == "Protoss") {
								gasHarvester = UnitType{ 157 }; //Protoss_Assimilator
							}
							else if (Broodwar->self()->getRace().getName() == "Zerg") {
								gasHarvester = UnitType{ 149 }; //Zerg_Extractor
							}
							bw << "Worker Found: " << u->getType().getName() + std::to_string(u->getID()) << std::endl;
							bw << "Desired Structure: " << gasHarvester.getName() << std::endl;
							if (u->build(gasHarvester, futureBuildingPos)) {
								bw << "Building being constructed" << std::endl;
							}
							else {
								bw << "Command Failed to Execute" << std::endl;
							}
							//u->build(gasHarvester);
							break;
						}
						// deleting name string
						u_name.clear();
					}
				}
				// Cleaning up pointers, arrays, and strings
				/*
				delete Reader1;
				delete Reader2;
				delete[] workerName;
				delete[] xPos_c;
				delete[] yPos_c;
				xPos_s.clear();
				yPos_s.clear();*/
				// clearing the receiver buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			// Building additional supply units (Terran_Supply_Depot/Protoss_Pylon/Zerg_Overlord)
			else if (strncmp(buildAdditionalSupply, recvbuf, 23) == 0) {
				//Setting up memory allocation and reading pointers
				char workerName[30];
				char xPos_c[10];
				char yPos_c[10];
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, space);

				//Getting worker name (case sensitive)
				int workerNameLength = Reader2 - Reader1;
				for (int i = 0; i < workerNameLength; i++) {
					workerName[i] = *Reader1++;
				}

				bw << "Worker for construction: " << std::endl;
				for (int i = 0; i < workerNameLength; i++) {
					bw << workerName[i];
				}
				bw << std::endl;

				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, space);

				//Getting x tile position
				int xPos_l = Reader2 - Reader1;
				for (int i = 0; i < xPos_l; i++) {
					xPos_c[i] = *Reader1++;
				}

				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);

				//Getting y tile position
				int yPos_l = Reader2 - Reader1;
				for (int i = 0; i < yPos_l; i++) {
					yPos_c[i] = *Reader1++;
				}

				// character arrays to strings
				//std::string workerName_s = workerName;
				std::string xPos_s = xPos_c;
				std::string yPos_s = yPos_c;

				// Giving confirmation tile markers
				bw << "Position for Supply building" << std::endl;
				bw << "X: " + xPos_s << std::endl;
				bw << "Y: " + yPos_s << std::endl;

				// Generate desired Tile Position
				std::string::size_type sz;
				TilePosition futureBuildingPos(std::stoi(xPos_s, &sz), std::stoi(yPos_s, &sz));
				bw << "TilePosition: " << futureBuildingPos << std::endl;

				//Preallocate a supply building type
				UnitType supplyBuilding;
				
				for (auto u : Broodwar->self()->getUnits()) {
					if (u->getType().isWorker() || (u->getType().getName() == "Zerg_Larva")) {
						std::string u_name = u->getType().getName() + std::to_string(u->getID());
						if (strncmp(u_name.c_str(), workerName, workerNameLength) == 0) {
							//Determine Race Specific Building Unit Type
							if (Broodwar->self()->getRace().getName() == "Terran")
							{
								supplyBuilding = UnitType{ 109 }; //Terran_Supply_Depot
							}
							else if (Broodwar->self()->getRace().getName() == "Protoss") {
								supplyBuilding = UnitType{ 156 }; //Protoss_Pylon
							}
							else if (Broodwar->self()->getRace().getName() == "Zerg") {
								supplyBuilding = UnitType{ 42 }; //Zerg_Overlord
							}
							// Confirming worker and structure
							bw << "Worker Found: " << u->getType().getName() + std::to_string(u->getID()) << std::endl;
							bw << "Desired Structure: " << supplyBuilding.getName() << std::endl;
							// Terran and Protoss construction
							if (Broodwar->self()->getRace().getName() != "Zerg") {
								if (u->build(supplyBuilding, futureBuildingPos)) {
									bw << "Building being constructed" << std::endl;
								}
								else {
									bw << "Command Failed to Execute" << std::endl;
								}
							}
							// Zerg Construction
							else {
								if (u->morph(supplyBuilding)) {
									bw << "Larva being morphed" << std::endl;
								}
								else {
									bw << "Command Failed to Execute (2)" << std::endl;
								}
							}
							break;
						}
						// deleting name string
						u_name.clear();
					}
				}
				// clearing the receiver buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			// Build unit to allow for basic ground troops (Terran_Barracks/Protoss_Warpgate/Zerg_Spawning_Pools)
			else if (strncmp(buildGroundTroopBuilding, recvbuf, 27) == 0) {
				//Setting up memory allocation and reading pointers
				char workerName[30];
				char xPos_c[10];
				char yPos_c[10];
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, space);

				//Getting worker name (case sensitive)
				int workerNameLength = Reader2 - Reader1;
				for (int i = 0; i < workerNameLength; i++) {
					workerName[i] = *Reader1++;
				}

				bw << "Worker for construction: " << std::endl;
				for (int i = 0; i < workerNameLength; i++) {
					bw << workerName[i];
				}
				bw << std::endl;

				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, space);

				//Getting x tile position
				int xPos_l = Reader2 - Reader1;
				for (int i = 0; i < xPos_l; i++) {
					xPos_c[i] = *Reader1++;
				}

				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);

				//Getting y tile position
				int yPos_l = Reader2 - Reader1;
				for (int i = 0; i < yPos_l; i++) {
					yPos_c[i] = *Reader1++;
				}

				// character arrays to strings
				//std::string workerName_s = workerName;
				std::string xPos_s = xPos_c;
				std::string yPos_s = yPos_c;

				// Giving Tile Position
				bw << "Position for Ground Troop building" << std::endl;
				bw << "X: " + xPos_s << std::endl;
				bw << "Y: " + yPos_s << std::endl;

				// Generate desired Tile Position
				std::string::size_type sz;
				TilePosition futureBuildingPos(std::stoi(xPos_s, &sz), std::stoi(yPos_s, &sz));
				bw << "TilePosition: " << futureBuildingPos << std::endl;

				//Preallocate a ground troop building type
				UnitType groundTroopBuilding;
				
				for (auto u : Broodwar->self()->getUnits()) {
					if (u->getType().isWorker()) {
						std::string u_name = u->getType().getName() + std::to_string(u->getID());
						if (strncmp(u_name.c_str(), workerName, workerNameLength) == 0) {
							//Determine Race Specific Building Unit Type
							if (Broodwar->self()->getRace().getName() == "Terran")
							{
								groundTroopBuilding = UnitType{ 111 }; //Terran_Barracks
							}
							else if (Broodwar->self()->getRace().getName() == "Protoss") {
								groundTroopBuilding = UnitType{ 160 }; //Protoss_Gateway
							}
							else if (Broodwar->self()->getRace().getName() == "Zerg") {
								groundTroopBuilding = UnitType{ 142 }; //Zerg_Spawning_Pool
							}
							// Confirming Worker and Structure
							bw << "Worker Found: " << u->getType().getName() + std::to_string(u->getID()) << std::endl;
							bw << "Desired Structure: " << groundTroopBuilding.getName() << std::endl;
							// Conditional is ordering build, with confirmation/denial messages
							if (u->build(groundTroopBuilding, futureBuildingPos)) {
								bw << "Building being constructed" << std::endl;
							}
							else {
								bw << "Command Failed to Execute" << std::endl;
							}
							break;
						}
						// clearing unit name;
						u_name.clear();
					}
				}
				// clearing the receiver buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			// Make a worker unit (Terran_SCV/Protoss_Probe/Zerg_Drone)
			else if (strncmp(makeWorker, recvbuf, 10) == 0) {
				//Setting up memory allocation and reading pointers
				char trainerName[30];
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, endComm);

				//Getting worker name (case sensitive)
				int trainerNameLength = Reader2 - Reader1;
				for (int i = 0; i < trainerNameLength; i++) {
					trainerName[i] = *Reader1++;
				}
				// Cleaning up pointers
				delete Reader1;
				delete Reader2;
				bw << "Source of Worker: ";
				for (int i = 0; i < trainerNameLength; i++) {
					bw << trainerName[i];
				}
				bw << std::endl;

				UnitType workerType;
				if (Broodwar->self()->getRace().getName() == "Terran") {
					workerType = UnitType{ 7 }; //Terran_SCV
				}
				else if (Broodwar->self()->getRace().getName() == "Protoss") {
					workerType = UnitType{ 64 }; //Protoss_Probe
				}
				else if (Broodwar->self()->getRace().getName() == "Zerg") {
					workerType = UnitType{ 41 }; //Zerg_Drone
				}

				for (auto &u : Broodwar->self()->getUnits()) {
					std::string u_name = u->getType().getName() + std::to_string(u->getID());
					if (strncmp(u_name.c_str(), trainerName, trainerNameLength) == 0) {
						u->train(workerType);
						bw << "Sent command for worker to be trained" << std::endl;
						break;
					}
				}
				// Deleting character array
				//delete[] trainerName;
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			// Make a basic ground troop unit (Terran_Marine/Protoss_Zealot/Zerg_Zergling)
			else if (strncmp(makeGroundTroop, recvbuf, 17) == 0) {
				//Setting up memory allocation and reading pointers
				char trainerName[30];
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, endComm);

				//Getting worker name (case sensitive)
				int trainerNameLength = Reader2 - Reader1;
				for (int i = 0; i < trainerNameLength; i++) {
					trainerName[i] = *Reader1++;
				}

				bw << "Source of Ground Troop: ";
				for (int i = 0; i < trainerNameLength; i++) {
					bw << trainerName[i];
				}
				bw << std::endl;

				UnitType troopType;
				if (Broodwar->self()->getRace().getName() == "Terran") {
					troopType = UnitType{ 0 }; //Terran_Marine
				}
				else if (Broodwar->self()->getRace().getName() == "Protoss") {
					troopType = UnitType{ 65 }; //Protoss_Zealot
				}
				else if (Broodwar->self()->getRace().getName() == "Zerg") {
					troopType = UnitType{ 37 }; //Zerg_Zergling
				}

				bw << "Ground Troop Unit Selected: " << troopType.getName() << std::endl;

				
				for (auto &u : Broodwar->self()->getUnits()) {
					std::string u_name = u->getType().getName() + std::to_string(u->getID());
					if (strncmp(u_name.c_str(), trainerName, trainerNameLength) == 0) {
						if (u->getType().getName() == "Zerg") {
							u->morph(troopType);
						}
						else {
							u->train(troopType);
						}
						bw << "Commanded ground troop to be trained" << std::endl;
						break;
					}
				}
				// Cleanup
				/*
				delete Reader1;
				delete Reader2;
				delete[] trainerName;*/
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			// Attack a different troop
			else if (strncmp(attack, recvbuf, 6) == 0){
				
				//Debugging text
				bw << "Inside attack command\n" << std::endl;

				//preallocating char blocks for unit names
				char playerUnit[30];
				char enemyUnit[30];

				//Reading through Player's unit
				bw << "Player unit: ";
				char *Reader1 = strpbrk(recvbuf, space);
				char *Reader2 = strpbrk(++Reader1, space);
				int playerUnitNameLength = Reader2 - Reader1;
				for (int i = 0; i < playerUnitNameLength; i++) {
					playerUnit[i] = *Reader1++;
					bw << playerUnit[i];
				}

				//Reading through Enemy's unit
				bw << "\nEnemy unit: ";
				Reader1 = Reader2 + 1;
				Reader2 = strpbrk(Reader1, endComm);
				int enemyUnitNameLength = Reader2 - Reader1;
				for (int i = 0; i < enemyUnitNameLength; i++) {
					enemyUnit[i] = *Reader1++;
					bw << enemyUnit[i];
				}
				bw << " Length of " << std::to_string(enemyUnitNameLength) << std::endl;

				//Getting enemy unit pointer
				bool enemyUnitFound = false;
				PositionOrUnit enemyUnitPoU = PositionOrUnit(nullptr);
				for (auto p : Broodwar -> getPlayers()) {
					if ( (!(p->isAlly(Broodwar -> self()))) && (p != (Broodwar ->self())) ) {
						for (auto e : p->getUnits()) {
							//bw << "\tUnit: " + e->getType().getName() + std::to_string(e->getID()) << std::endl;
							std::string e_name = e->getType().getName() + std::to_string(e->getID());
							int e_nameLength = e_name.length();
							//bw << std::to_string(e_nameLength) << " ";
							if (e_nameLength == enemyUnitNameLength) {
								bw << e_name << std::endl;
								if (strncmp(e_name.c_str(), enemyUnit, enemyUnitNameLength) == 0) {
									bw << "Found enemy unit" << std::endl;
									enemyUnitFound = true;
									enemyUnitPoU = e; //assignment operator over for UnitCommand
								}
							}
							if (enemyUnitFound) { break; }
						}
					}
					if (enemyUnitFound) { break; }
				}

				if (!enemyUnitFound) { bw << "Enemy unit not found" << std::endl; }

				if (enemyUnitFound) {
					bw << "Looking for player's unit" << std::endl;
					//iterating through player units to execute unitcommand
					for (auto u : Broodwar->self()->getUnits()) {
						std::string u_name = u->getType().getName() + std::to_string(u->getID());
						int u_nameLength = u_name.length();
						if (u_nameLength == playerUnitNameLength) {
							if (strncmp(u_name.c_str(), playerUnit, playerUnitNameLength) == 0) {
								bw << "Player unit found" << std::endl;
								u->attack(enemyUnitPoU, true);//Shift queue
								break;
							}
						}
					}
				}

				//Wiping reciever buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			//No command found
			else{
				char* Reader = strpbrk(recvbuf, 0);
				int messageLen = Reader - recvbuf;
				bw << "Messge recieved:\n";
				for (int i = 0; i < messageLen; i++) { bw << recvbuf[i]; }
				bw << std::endl;
				//Wiping reciever buffer
				memset(recvbuf, 0, sizeof(recvbuf));
			}
			//Sending Failure
			if (iSendResult == SOCKET_ERROR) {
				bw << "send failed with error: " << WSAGetLastError() << std::endl;
				closesocket(ClientSocket);
				WSACleanup();
				return;
			}
			else {}
			// Processing commands from Icarus
			bw << "Bytes sent: " << iSendResult << std::endl;
		}
		else if (iResult == 0) {
			bw << "Connection closing ... " << std::endl;
		}
		//recv Error
		else 
		{
			bw << "recv failed with error: " << WSAGetLastError() << std::endl;
			closesocket(ClientSocket);
			WSACleanup();
			return;
		} 
	} while (iResult > 0);
	bw << "TCP Heartbeat" << std::endl;

	iResult = shutdown(ClientSocket, SD_SEND);
	if (iResult == SOCKET_ERROR) {
		printf("shutdown failed with error: %d\n", WSAGetLastError());
		closesocket(ClientSocket);
		WSACleanup();
		return;
	}

	//cleanup
	closesocket(ClientSocket);
	WSACleanup();
}
//End TCP server (CHOI)

void ExampleAIModule::onStart()
{
	//BWEM try-catch
	try
	{
		// Hello World!
		Broodwar->sendText("Initializing ICARUS connector ... ");//Choi modification of hello world

																 // Print the map name.
																 // BWAPI returns std::string when retrieving a string, don't forget to add .c_str() when printing!
		Broodwar << "The map is " << Broodwar->mapName() << "!" << std::endl;

		// Enable the UserInput flag, which allows us to control the bot and type messages.
		Broodwar->enableFlag(Flag::UserInput);

		// Uncomment the following line and the bot will know about everything through the fog of war (cheat).
		//Broodwar->enableFlag(Flag::CompleteMapInformation);

		// Set the command optimization level so that common commands can be grouped
		// and reduce the bot's APM (Actions Per Minute).
		Broodwar->setCommandOptimizationLevel(2);

		// Check if this is a replay
		if (Broodwar->isReplay())
		{

			// Announce the players in the replay
			Broodwar << "The following players are in this replay:" << std::endl;

			// Iterate all the players in the game using a std:: iterator
			Playerset players = Broodwar->getPlayers();
			for (auto p : players)
			{
				// Only print the player if they are not an observer
				if (!p->isObserver())
					Broodwar << p->getName() << ", playing as " << p->getRace() << std::endl;
			}

		}
		else // if this is not a replay
		{
			// Retrieve you and your enemy's races. enemy() will just return the first enemy.
			// If you wish to deal with multiple enemies then you must use enemies().
			if (Broodwar->enemy()) { // First make sure there is an enemy
				Broodwar << "The matchup is " << Broodwar->self()->getRace() << " vs " << Broodwar->enemy()->getRace() << std::endl;
			}
			//Starting TCP server
			server = new std::thread(startServer, std::ref(Broodwar));

			//BWEM Add-on
			Broodwar << "Map initialization..." << std::endl;

			theMap.Initialize();
			theMap.EnableAutomaticPathAnalysis();
			bool startingLocationsOK = theMap.FindBasesForStartingLocations();
			assert(startingLocationsOK);

			BWEM::utils::MapPrinter::Initialize(&theMap);
			BWEM::utils::printMap(theMap);      // will print the map into the file <StarCraftFolder>bwapi-data/map.bmp
			BWEM::utils::pathExample(theMap);   // add to the printed map a path between two starting locations

			Broodwar << "gg" << std::endl;
			//End BWEM Add-on
		}
	}
	catch (const std::exception & e)
	{
		Broodwar << "EXCEPTION: " << e.what() << std::endl;
	}
}

void ExampleAIModule::onEnd(bool isWinner)
{
	// Called when the game ends
	if (isWinner)
	{
		// Log your win here!
	}
	server->join();
	delete server;
}

void ExampleAIModule::onFrame()
{
	// Called once every game frame

	//BWEM Add-on and try-catch
	try {
		BWEM::utils::gridMapExample(theMap);
		BWEM::utils::drawMap(theMap);
		//End BWEM try Add-on

		// Display the game frame rate as text in the upper left area of the screen
		Broodwar->drawTextScreen(200, 0, "FPS: %d", Broodwar->getFPS());
		Broodwar->drawTextScreen(200, 20, "Average FPS: %f", Broodwar->getAverageFPS());

		// Return if the game is a replay or is paused
		if (Broodwar->isReplay() || Broodwar->isPaused() || !Broodwar->self())
			return;

		// Prevent spamming by only running our onFrame once every number of latency frames.
		// Latency frames are the number of frames before commands are processed.
		if (Broodwar->getFrameCount() % Broodwar->getLatencyFrames() != 0)
			return;

		// Iterate through all the units that we own
		for (auto &u : Broodwar->self()->getUnits())
		{
			// Ignore the unit if it no longer exists
			// Make sure to include this block when handling any Unit pointer!
			if (!u->exists())
				continue;

			// Ignore the unit if it has one of the following status ailments
			if (u->isLockedDown() || u->isMaelstrommed() || u->isStasised())
				continue;

			// Ignore the unit if it is in one of the following states
			if (u->isLoaded() || !u->isPowered() || u->isStuck())
				continue;

			// Ignore the unit if it is incomplete or busy constructing
			if (!u->isCompleted() || u->isConstructing())
				continue;


			// Finally make the unit do some stuff!


			// If the unit is a worker unit
			/*
			if (u->getType().isWorker())
			{
				// if our worker is idle
				if (u->isIdle())
				{
					// Order workers carrying a resource to return them to the center,
					// otherwise find a mineral patch to harvest.
					if (u->isCarryingGas() || u->isCarryingMinerals())
					{
						u->returnCargo();
					}
					else if (!u->getPowerUp())  // The worker cannot harvest anything if it
					{                             // is carrying a powerup such as a flag
												  // Harvest from the nearest mineral patch or gas refinery
						if (!u->gather(u->getClosestUnit(IsMineralField || IsRefinery)))
						{
							// If the call fails, then print the last error message
							Broodwar << Broodwar->getLastError() << std::endl;
						}

					} // closure: has no powerup
				} // closure: if idle

			}
			
			else if (u->getType().isResourceDepot()) // A resource depot is a Command Center, Nexus, or Hatchery
			{

				// Order the depot to construct more workers! But only when it is idle.
				if (u->isIdle() && !u->train(u->getType().getRace().getWorker()))
				{
					// If that fails, draw the error at the location so that you can visibly see what went wrong!
					// However, drawing the error once will only appear for a single frame
					// so create an event that keeps it on the screen for some frames
					Position pos = u->getPosition();
					Error lastErr = Broodwar->getLastError();
					Broodwar->registerEvent([pos, lastErr](Game*) { Broodwar->drawTextMap(pos, "%c%s", Text::White, lastErr.c_str()); },   // action
						nullptr,    // condition
						Broodwar->getLatencyFrames());  // frames to run

														// Retrieve the supply provider type in the case that we have run out of supplies
					UnitType supplyProviderType = u->getType().getRace().getSupplyProvider();
					static int lastChecked = 0;

					// If we are supply blocked and haven't tried constructing more recently
					if (lastErr == Errors::Insufficient_Supply &&
						lastChecked + 400 < Broodwar->getFrameCount() &&
						Broodwar->self()->incompleteUnitCount(supplyProviderType) == 0)
					{
						lastChecked = Broodwar->getFrameCount();

						// Retrieve a unit that is capable of constructing the supply needed
						Unit supplyBuilder = u->getClosestUnit(GetType == supplyProviderType.whatBuilds().first &&
							(IsIdle || IsGatheringMinerals) &&
							IsOwned);
						// If a unit was found
						if (supplyBuilder)
						{
							if (supplyProviderType.isBuilding())
							{
								TilePosition targetBuildLocation = Broodwar->getBuildLocation(supplyProviderType, supplyBuilder->getTilePosition());
								if (targetBuildLocation)
								{
									// Register an event that draws the target build location
									Broodwar->registerEvent([targetBuildLocation, supplyProviderType](Game*)
									{
										Broodwar->drawBoxMap(Position(targetBuildLocation),
											Position(targetBuildLocation + supplyProviderType.tileSize()),
											Colors::Blue);
									},
										nullptr,  // condition
										supplyProviderType.buildTime() + 100);  // frames to run

																				// Order the builder to construct the supply structure
									supplyBuilder->build(supplyProviderType, targetBuildLocation);
								}
							}
							else
							{
								// Train the supply provider (Overlord) if the provider is not a structure
								supplyBuilder->train(supplyProviderType);
							}
						} // closure: supplyBuilder is valid
					} // closure: insufficient supply
				} // closure: failed to train idle unit

			}
			*/
		} // closure: unit iterator
	}
	catch (const std::exception & e)
	{
		Broodwar << "EXCEPTION: " << e.what() << std::endl;
	}
}

void ExampleAIModule::onSendText(std::string text)
{
	//BWEM Add-on
	BWEM::utils::MapDrawer::ProcessCommand(text);
	//End BWEM Add-on

	// Send the text to the game if it is not being processed.
	Broodwar->sendText("%s", text.c_str());


	// Make sure to use %s and pass the text as a parameter,
	// otherwise you may run into problems when you use the %(percent) character!

}

void ExampleAIModule::onReceiveText(BWAPI::Player player, std::string text)
{
	// Parse the received text
	Broodwar << player->getName() << " said \"" << text << "\"" << std::endl;
}

void ExampleAIModule::onPlayerLeft(BWAPI::Player player)
{
	// Interact verbally with the other players in the game by
	// announcing that the other player has left.
	Broodwar->sendText("Goodbye %s!", player->getName().c_str());
}

void ExampleAIModule::onNukeDetect(BWAPI::Position target)
{

	// Check if the target is a valid position
	if (target)
	{
		// if so, print the location of the nuclear strike target
		Broodwar << "Nuclear Launch Detected at " << target << std::endl;
	}
	else
	{
		// Otherwise, ask other players where the nuke is!
		Broodwar->sendText("Where's the nuke?");
	}

	// You can also retrieve all the nuclear missile targets using Broodwar->getNukeDots()!
}

void ExampleAIModule::onUnitDiscover(BWAPI::Unit unit)
{
}

void ExampleAIModule::onUnitEvade(BWAPI::Unit unit)
{
}

void ExampleAIModule::onUnitShow(BWAPI::Unit unit)
{
}

void ExampleAIModule::onUnitHide(BWAPI::Unit unit)
{
}

void ExampleAIModule::onUnitCreate(BWAPI::Unit unit)
{
	if (Broodwar->isReplay())
	{
		// if we are in a replay, then we will print out the build order of the structures
		if (unit->getType().isBuilding() && !unit->getPlayer()->isNeutral())
		{
			int seconds = Broodwar->getFrameCount() / 24;
			int minutes = seconds / 60;
			seconds %= 60;
			Broodwar->sendText("%.2d:%.2d: %s creates a %s", minutes, seconds, unit->getPlayer()->getName().c_str(), unit->getType().c_str());
		}
	}
}

void ExampleAIModule::onUnitDestroy(BWAPI::Unit unit)
{
	//BWEM try-catch
	try
	{
		if (unit->getType().isMineralField())    theMap.OnMineralDestroyed(unit);
		else if (unit->getType().isSpecialBuilding()) theMap.OnStaticBuildingDestroyed(unit);
	}
	catch (const std::exception & e)
	{
		Broodwar << "EXCEPTION: " << e.what() << std::endl;
	}
	//End BWEM try-catch
}

void ExampleAIModule::onUnitMorph(BWAPI::Unit unit)
{
	if (Broodwar->isReplay())
	{
		// if we are in a replay, then we will print out the build order of the structures
		if (unit->getType().isBuilding() && !unit->getPlayer()->isNeutral())
		{
			int seconds = Broodwar->getFrameCount() / 24;
			int minutes = seconds / 60;
			seconds %= 60;
			Broodwar->sendText("%.2d:%.2d: %s morphs a %s", minutes, seconds, unit->getPlayer()->getName().c_str(), unit->getType().c_str());
		}
	}
}

void ExampleAIModule::onUnitRenegade(BWAPI::Unit unit)
{
}

void ExampleAIModule::onSaveGame(std::string gameName)
{
	Broodwar << "The game was saved to \"" << gameName << "\"" << std::endl;
}

void ExampleAIModule::onUnitComplete(BWAPI::Unit unit)
{
}