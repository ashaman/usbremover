/*
	ServiceDispatcher.h
	Started: 12.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Service dispatcher
*/
#ifndef _H_SERVDISPATCH
#define _H_SERVDISPATCH

#include "ApiWrapper.h"
#include "DeviceManager.h"
#include "MessageManager.h"
#include "ProcessManager.h"
#include "IMessageListener.h"
#include "Communication.h"
#include "SrvMain.h"

/*
	Description:
		This is the core of the service - it's an event dispatcher
		which handles all the queries and events
*/

//TODO: if the procedure and method calls from another threads
//are not suitable - use ManualResetEvent with the main dispatcher thread (loop do-while)

//TODO: implement KillProcess, KillAllProcesses, ForcedRemoval, cient stopping

class ServiceDispatcher: public IMessageListener, public IProgressCallbackListener
{
private:
	DeviceManager *devmgr; //device manager
	MessageManager *msgmgr; //message manager
	ProcessManager *procmgr; //process manager
	HANDLE hInPipeHandle; //incoming pipe handle
	HANDLE hOutPipeHandle; //outgoing pipe handle
	HANDLE hDispatcherThread; //dispatcher thread

	~ServiceDispatcher(); //destructor
	void CreateChannel(); //creating named pipe
	void EjectDevice(); //ejecting a device
	void SendDeviceInfo(); //sends device information to the client
	void SendLockInfo(LockInfo *lockers); //sends process-locker info to the client
	void ResolveQuery(POPINFO pOperInfo); //executes the query got from the client
	void WalkDeviceTree(DWORD level, SIZE_T index, SIZE_T parent, 
		SIZE_T top, Device *current); //subroutine of SendDeviceInfo
public:
	ServiceDispatcher(HANDLE hStatusHandle, DWORD flags); //constructor
	void DispatchEvent(LPVOID lpEventData, DWORD dwEventType); //device message handler
	void HandleRequests(SERVICE_STATUS &status); //handle requests
	void Pause(); //stopping service for some time
	void RefreshState(); //implements RefreshState method
	void ReportProgress(byte percentage); //reports progress
	void Resume(); //resuming the work
	static void TerminateDispatcherThread(ServiceDispatcher *dispatcher); //terminates the dispatcher
};

#endif