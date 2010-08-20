/*
	VolumeManager.cpp
	Started: 17.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Mount manager class implementation
*/
#include "VolumeManager.h"

/*
	Globals for the thread - to avoid memory leaks
*/
MOUNTMGR_CHANGE_NOTIFY_INFO ni1, ni2;
DWORD dummy;
//additional pointer to "this"
VolumeManager *vlmgrPtr;
//critical section for synchro
CRITICAL_SECTION csListeners;

/*
	Purpose:
		Default constructor. Initializes a handle pointing
		to the mount manager and starts the monitoring of it
	Parameters:
		None
*/
VolumeManager::VolumeManager()
{
	//initializing critical section
	InitializeCriticalSection(&csListeners);
	//the state is nonpaused
	this->pause = false;
	this->listeners = new DelegateSet();
	vlmgrPtr = this;
	//Opening mount manager
	try
	{
		//opening mount manager
		this->managerHandle = CreateFile(MOUNTMGR_DOS_DEVICE_NAME,
			GENERIC_READ, FILE_SHARE_READ| FILE_SHARE_WRITE, NULL,
			OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
		if (this->managerHandle == INVALID_HANDLE_VALUE)
		{
			throw WinAPIException(GetLastError());
		}
		else
		{
			//starting the observer thread
			this->threadHandle = CreateThread(NULL, DEFAULT_SIZE, &Monitor,
				&this->managerHandle, ZERO_FLAGS, ZERO_HANDLE);
		}
	}
	catch(...)
	{
		DELOBJ(this->listeners);
#ifdef DEBUG
		_ASSERT(this->listeners != NULL);
#endif
	}
}

/*
	Purpose:
		Default destructor
*/
VolumeManager::~VolumeManager()
{
	//Here we should terminate background thread
	if (!TerminateThread(this->threadHandle, NO_ERROR))
	{
		throw WinAPIException(GetLastError());
	}
	//releasing all resources
	CloseHandle(this->threadHandle);
	CloseHandle(this->managerHandle);
	DeleteCriticalSection(&csListeners);
	DELOBJ(this->listeners);
#ifdef DEBUG
	_ASSERT(this->listeners == NULL);
#endif
}

/*
	Purpose:
		Notifies about the device events
	Parameters:
		None
	Return value:
		None
*/
void VolumeManager::FireEvent()
{
	try
	{
		EnterCriticalSection(&csListeners);
		for (DelegateSet::iterator i = this->listeners->begin();
			i != this->listeners->end(); ++i)
		{
			(*i)->RefreshState();
		}
	}
	catch(...)
	{
		LeaveCriticalSection(&csListeners);
		throw;
	}
	LeaveCriticalSection(&csListeners);
}

/*
	Purpose:
		Attaches a listener
	Parameters:
		None
	Return value:
		None
*/
void VolumeManager::AttachListener(IMessageListener *listener)
{
	EnterCriticalSection(&csListeners);
	this->listeners->insert(listener);
	LeaveCriticalSection(&csListeners);
}

/*
	Purpose:
		Detaches a listener
	Parameters:
		None
	Return value:
		None
*/
void VolumeManager::DetachListener(IMessageListener *listener)
{
	EnterCriticalSection(&csListeners);
	this->listeners->erase(listener);
	LeaveCriticalSection(&csListeners);
}

/*
	Purpose:
		Pauses the manager work
	Parameters:
		None
	Return value:
		None
*/
void VolumeManager::Pause()
{
	this->pause = true;
}

/*
	Purpose:
		Resumes the manager work
	Parameters:
		None
	Return value:
		None
*/
void VolumeManager::Resume()
{
	this->pause = false;
}


/*
	Purpose:
		Fires a refresh event
	Parameters:
		parameters - thread parameters
	Return value:
		Status of the operation
*/
DWORD WINAPI Notify(LPVOID parameters)
{
	vlmgrPtr->FireEvent();
	return NO_ERROR;
}

/*
	Purpose:
		Monitors the mount manager database
	Parameters:
		parameters - thread parameters - handle to 
		the mount manager
	Return value:
		Status of the operation

	Verified 17.06.2010 by Asha'man
	Tested 17.06.2010 by Asha'man
*/
DWORD WINAPI Monitor(LPVOID parameters)
{
	HANDLE handle = *(PHANDLE)parameters;
	while (true)
	{
		//Call to DeviceIoControl
		if (!DeviceIoControl(handle, IOCTL_MOUNTMGR_CHANGE_NOTIFY,
			&ni1, sizeof(ni1),
			&ni2, sizeof(ni2),
			&dummy, ZERO_HANDLE))
		{
			throw WinAPIException(GetLastError());
		}
		else
		{
			//if the manager is paused, no report is done
			if (!vlmgrPtr->pause)
			{
				/*
					WARNING!!!
					IT SHOULD BE DONE QUICKLY BECAUSE MOUNT POINTS
					CAN CHANGE DURING THE REFRESH TIME 
				*/
				CreateThread(NULL, 0, &Notify, NULL, ZERO_FLAGS, NULL);
			}
			ni1.EpicNumber = ni2.EpicNumber;
		}
	}
	return NO_ERROR;
}