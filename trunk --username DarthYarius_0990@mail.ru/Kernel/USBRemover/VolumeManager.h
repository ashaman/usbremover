/*
	VolumeManager.h
	Started: 17.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Mount manager class declarations
*/

#ifndef _H_VOLMGR
#define _H_VOLMGR

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

//API headers
#include <windows.h>
#include <ddk/mountmgr.h>
#include <set>

#include "ApiWrapper.h"
#include "WinAPIException.h"
#include "IMessageListener.h"

/*
	Description:
		This class is for complex volume work, e.g. detection
		of mounting/remounting volumes using DeviceIoControl
		Note: to compile this, you should use Windows Driver Kit
		(or header <mountmgr.h> separately)
*/
class VolumeManager
{
private:
	DelegateSet *listeners; //event listeners
	HANDLE managerHandle; //mount manager handle
	HANDLE threadHandle; //background worker handle
	bool pause; //paused or not

	void FireEvent(); //notifies about the events
	friend DWORD WINAPI Monitor(LPVOID parameters); //observer thread function
	friend DWORD WINAPI Notify(LPVOID parameters); //refresher function
public:
	VolumeManager(); //constructor
	~VolumeManager(); //destructor
	
	void AttachListener(IMessageListener *listener); //attach a listener
	void DetachListener(IMessageListener *listener); //detach a listener
	void Pause(); //pauses the manager work
	void Resume(); //resumes the manager work
};


#endif