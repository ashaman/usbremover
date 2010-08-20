/*
	ProcessManager.h
	Started: 14.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Process manager class: declaration
*/
#ifndef _H_PROCESSMGR
#define _H_PROCESSMGR

#include <map>
#include "ApiWrapper.h"
#include "IMessageListener.h"

//Type definitions - for process and locking (what process keeps what files)
typedef std::pair<LPTSTR, StringList*> Process;
typedef std::map<DWORD, Process> LockInfo;

/*
	Description:
*/

//TODO: KillProcess, GetLockerInfo, KillAllLockers and so on
class ProcessManager
{
private:
	LockInfo *lockerList; //locker information
	DWORD fileHandleType; //file handle type on the current system
	IProgressCallbackListener *listener; //progress listener

	void Clear() const; //clears the lockers list
	void FireEvent(byte percentage) const; //fires a callback event
public:
	ProcessManager(); //constructor
	~ProcessManager(); //destructor
	void AttachListener(IProgressCallbackListener *listener); //attaches a listener
	void DetachListener(IProgressCallbackListener *listener); //detaches a listener
	LockInfo* GetLockers(StringPairList mountPoints) const; //main routine - get lockers
};

#endif