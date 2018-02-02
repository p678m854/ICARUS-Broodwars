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
			if (strcmp(preattend, recvbuf) == 0) {
				//Iterate all the players in the game using a std:: iterator
				Playerset players = bw->getPlayers();
				std::string percepts = "(";
				for (auto p : players) {
					// Only print the player if they are not an observer
					if (!p->isObserver()) {
						bw << p->getName() << ", playing as " << p->getRace() << std::endl;
					}
					Unitset playerUnits = p->getUnits();
					for (auto &u : playerUnits)
					{
						Position pos = u->getPosition();//{X,Y}
						TilePosition tpos = u->getTilePosition();
						const BWEM::Area *u_area = theMap.GetNearestArea(tpos);
						if (u->getType().isMineralField())
						{
							percepts += "(mineral mineral" + std::to_string(u->getID()) + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) 
								        + " area area" +std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isAddon())
						{
							percepts += "(addon add" + std::to_string(u->getID()) + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) 
								        + " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isBeacon())
						{
							percepts += "(beacon beacon" + std::to_string(u->getID()) + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) 
								        + " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isNeutral())
						{
							percepts += "(neutral neutral" + std::to_string(u->getID()) + " name " + u->getType().getName() 
								        + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) + " area area" + std::to_string(u_area->Id()) + ")\n";
						}
						else if (u->getType().isBuilding())
						{
							percepts += "(building building" + std::to_string(u->getID()) + " name " + u->getType().getName()
								        + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) 
								        + " area" + std::to_string(u_area->Id()) + " player " + p->getName() + ")\n";
						}
						else if (u->getType().isOrganic())
						{
							percepts += "(organic organic" + std::to_string(u->getID()) + " name " + u->getType().getName() 
								        + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) 
								        + " area area" + std::to_string(u_area->Id()) + " player " + p->getName() + ")\n";
						}
						else if (u->getType().isPowerup())
						{
							percepts += "(powerup powerup" + std::to_string(u->getID()) + " name " + u->getType().getName() 
								     + " x " + std::to_string(pos.x) + " y " + std::to_string(pos.y) + " area area" + std::to_string(u_area->Id()) + ")\n";
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
							percepts += "(ChokePoint CP" + std::to_string(cp->Index()) + " A1 Area" + std::to_string(cp_areas.first->Id())
								+ " A2 Area" + std::to_string(cp_areas.second->Id()) + " Access ";
							if ((cp->IsPseudo()) == false) {
								percepts += "Open)\n";                     //if chokepoint is open
							}
							else {										 //If chokepoint is blocked
								Neutral *blocker = cp->BlockingNeutral();//Neutral that's blocking
								Unit b_unit = blocker->Unit();           //Going from BWEM to BWAPI classes
								UnitType b_UnitType = blocker->Type();
								std::string b_ID = std::to_string(b_unit->getID());
								if (blocker->IsMineral() != nullptr) {
									percepts += " mineral";
								}
								else if (b_UnitType.isAddon()){
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
								percepts += b_ID + ")\n";
							}
						}
					}
				}
				percepts += ")/n";
				iSendResult = send(ClientSocket, percepts.c_str(), percepts.length(), 0);

				if (iSendResult == SOCKET_ERROR) {
					bw << "send failed with error: " << WSAGetLastError() << std::endl;
					closesocket(ClientSocket);
					WSACleanup();
					return;
				}
			}

			bw << "Bytes sent: " << iSendResult << std::endl;
		}
		else if (iResult == 0) {
			bw << "Connection closing ... " << std::endl;
		}
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
  if ( isWinner )
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
  if ( target )
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
  if ( Broodwar->isReplay() )
  {
    // if we are in a replay, then we will print out the build order of the structures
    if ( unit->getType().isBuilding() && !unit->getPlayer()->isNeutral() )
    {
      int seconds = Broodwar->getFrameCount()/24;
      int minutes = seconds/60;
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
  if ( Broodwar->isReplay() )
  {
    // if we are in a replay, then we will print out the build order of the structures
    if ( unit->getType().isBuilding() && !unit->getPlayer()->isNeutral() )
    {
      int seconds = Broodwar->getFrameCount()/24;
      int minutes = seconds/60;
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
