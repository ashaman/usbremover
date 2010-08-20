/*
	MessageManager.h
	Started: 12.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Message manager class: implementation
*/

#include "MessageManager.h"

//USB device path coming in the message
const LPTSTR USB_DEVICE_PATH = _T("\\\\?\\USB#");
//waiting timeout
const DWORD TIMEOUT = 2000;
//additional pointer for fucntion call
MessageManager *mmgrPtr;
//critical section for synchro
CRITICAL_SECTION csmListeners;

/*
	Purpose:
		Constructor. Registers device notification
	Parameters:
		hWnd - window/service receiver handle
		flags - flags for RegisterDeviceNotification
*/
MessageManager::MessageManager(HANDLE hWnd, DWORD flags)
{
	//initializing critical section
	InitializeCriticalSection(&csmListeners);
	//setting state to non-paused
	this->pause = false;
	//saving additional pointer
	mmgrPtr = this;
	//allocating memory for listener set
	this->listeners = new DelegateSet();
	//saving window handle
	this->wndHandle = hWnd;
	//setting default message filter
	this->SetMessageFilter();
	this->notificationHandle = RegisterDeviceNotification(this->wndHandle,
		this->messageFilter, flags);
	//registering notification handle failed!
	if (this->notificationHandle == NULL)
	{
		DELARRAY(this->messageFilter);
#ifdef DEBUG
		_ASSERT(this->messageFilter == NULL);
#endif
		throw WinAPIException(GetLastError());
	}
}

/*
	Purpose:
		Destructor
*/
MessageManager::~MessageManager()
{
	//unregistering device notification
	UnregisterDeviceNotification(this->notificationHandle);
	DeleteCriticalSection(&csmListeners);
	DELARRAY(this->messageFilter);
	DELOBJ(this->listeners);
#ifdef DEBUG
	_ASSERT(this->messageFilter == NULL);
	_ASSERT(this->listeners == NULL);
#endif
}

/*
	Purpose:
		Setting message filter.
		It will only get notifications about USB devices
	Parameters:
		None
	Return value:
		None
*/
void MessageManager::SetMessageFilter()
{
	//allocating memory
	this->messageFilter = (PDEV_BROADCAST_DEVICEINTERFACE)
		(new BYTE[sizeof(DEV_BROADCAST_DEVICEINTERFACE)]);
	//setting message filter sige
	this->messageFilter->dbcc_size = sizeof(DEV_BROADCAST_DEVICEINTERFACE);
	//setting notification type
	this->messageFilter->dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;
	//setting class guid for USB device
	this->messageFilter->dbcc_classguid = GUID_DEVINTERFACE_USB_DEVICE;
}

/*
	Purpose:
		Process device events using message codes
	Parameters:
		messageData - message LParam pointer
		messageCode - device message code
	Return value:
		None
*/
void MessageManager::ProcessEvent(LPVOID messageData, DWORD messageCode)
{
	//if a device has arrived
	if (messageCode == DBT_DEVICEARRIVAL)
	{
		//if the event belongs to the appropriate class
		if (((PDEV_BROADCAST_HDR)messageData)->dbch_devicetype 
			== DBT_DEVTYP_DEVICEINTERFACE)
		{
			//getting event description string
			LPTSTR buffer = ((PDEV_BROADCAST_DEVICEINTERFACE)messageData)->dbcc_name;
			//if it is a USB device (it is present in the header)
			if (_tcsstr(buffer, USB_DEVICE_PATH) != NULL)
			{
				//if the manager is not paused
				if (!this->pause)
				{
					//We should return from the message handler
					//until 30 seconds have passed. Otherwise,
					//the SCM will generate an error. See RSDN Magazine #1-2003
					//"Programming services"
					HANDLE hThread = CreateThread(NULL, 0, InvokeOnInstallation,
						NULL, ZERO_FLAGS, NULL);
					if (hThread == INVALID_HANDLE_VALUE)
					{
						throw WinAPIException(GetLastError());
					}
				}
			}
		}
	}
	//if a device was removed
	if (messageCode == DBT_DEVICEREMOVECOMPLETE)
	{
		this->FireEvent();
	}
}

/*
	Purpose:
		Notifies all the listeners about events
	Parameters:
		None
	Return value:
		None
*/
void MessageManager::FireEvent()
{
	try
	{
		EnterCriticalSection(&csmListeners);
		for (DelegateSet::iterator i = this->listeners->begin();
			i != this->listeners->end(); ++i)
		{
			(*i)->RefreshState();
		}
	}
	catch(...)
	{
		LeaveCriticalSection(&csmListeners);
		throw;
	}
	LeaveCriticalSection(&csmListeners);
}

/*
	Purpose:
		Attaches listener
	Parameters:
		listener - pointer to listener
	Return value:
		None
*/
void MessageManager::AttachListener(IMessageListener &listener)
{
	EnterCriticalSection(&csmListeners);
	this->listeners->insert(&listener);
	LeaveCriticalSection(&csmListeners);
}

/*
	Purpose:
		Detaches listener
	Parameters:
		listener - pointer to listener
	Return value:
		None
*/
void MessageManager::DetachListener(IMessageListener &listener)
{
	EnterCriticalSection(&csmListeners);
	this->listeners->erase(&listener);
	LeaveCriticalSection(&csmListeners);
}

/*
	Purpose:
		Pauses the manager
	Parameters:
		None
	Return value:
		None
*/
void MessageManager::Pause()
{
	this->pause = true;
}

/*
	Purpose:
		Resumes manager's work
	Parameters:
		None
	Return value:
		None
*/void MessageManager::Resume()
{
	this->pause = false;
}

/*
	Purpose:
		Thread function which is invocated when device installation starts.
		It waits until there are no pending install events
	Parameters:
		None
	Return value:
		Thread exit status
*/
DWORD WINAPI InvokeOnInstallation(LPVOID parameters)
{
	//waiting until the device is completely installed
	while (CMP_WaitNoPendingInstallEvents(TIMEOUT) == WAIT_TIMEOUT);
	//firing the event
	mmgrPtr->FireEvent();
	return NO_ERROR;
}
