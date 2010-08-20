/*
	ServiceDispatcher.h
	Started: 12.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Service dispatcher implementation
*/

#include "ServiceDispatcher.h"

//constants needed for named pipes - size of buffer and timeout
//number of pipe instances
const DWORD NUM_PIPE_INSTANCES = (DWORD)1;
//buffering array size
const DWORD BUF_SIZE = (DWORD)31;
//syncro objects
CRITICAL_SECTION csTermThread;

/*
	Purpose:
		Constructor. Initializes both device and message managers
		and creates pipes for communication
	Parameters:
		hStatusHandle - service status handle
		flags - creation flags
*/
ServiceDispatcher::ServiceDispatcher(HANDLE hStatusHandle, DWORD flags)
{
	//initializing critical sections
	InitializeCriticalSection(&csTermThread);
	//creating managers
	this->devmgr = new DeviceManager();
	this->msgmgr = new MessageManager(hStatusHandle, flags);
	this->procmgr = new ProcessManager();
	//attaching listeners
	this->msgmgr->AttachListener(*(this->devmgr));
	this->devmgr->AttachListener(this);
	this->procmgr->AttachListener(this);
	//duplicating thread handle
	DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(),
		&hDispatcherThread,	DUPLICATE_SAME_ACCESS, FALSE, DUPLICATE_SAME_ACCESS);
}

/*
	Purpose:
		Creates a communication channel
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::CreateChannel()
{
	//creating communication channels
	//this->hInPipeHandle = CreateNamedPipe(PIPE_INCOMING_NAME, PIPE_ACCESS_INBOUND /*|
	//	FILE_FLAG_OVERLAPPED*/, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 
	//	NUM_PIPE_INSTANCES, ZERO_BUFFER, ZERO_BUFFER, WAIT_PIPE_TIMEOUT, NULL);
	this->hOutPipeHandle = CreateNamedPipe(PIPE_OUTGOING_NAME, PIPE_ACCESS_OUTBOUND /*|
		FILE_FLAG_OVERLAPPED*/, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
		NUM_PIPE_INSTANCES, ZERO_BUFFER, ZERO_BUFFER, WAIT_PIPE_TIMEOUT, NULL);
	if ((this->hInPipeHandle == INVALID_HANDLE_VALUE) ||
		(this->hOutPipeHandle == INVALID_HANDLE_VALUE))
	{
		throw WinAPIException(GetLastError());
	}
	if (!ConnectNamedPipe(this->hOutPipeHandle, NULL /*&over2*/))
	{
		throw WinAPIException(GetLastError());
	}
	WaitNamedPipe(PIPE_INCOMING_NAME, NMPWAIT_WAIT_FOREVER);
	this->hInPipeHandle = CreateFile(PIPE_INCOMING_NAME, GENERIC_READ, FILE_SHARE_READ |
		FILE_SHARE_WRITE, NULL, OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
	//if (!ConnectNamedPipe(this->hInPipeHandle, NULL))
	//{
	//	throw WinAPIException(GetLastError());
	//}
	//if (!ImpersonateNamedPipeClient(this->hOutPipeHandle))
	//{
	//	throw WinAPIException(GetLastError());
	//}
}

/*
	Purpose:
		Destructor
*/
ServiceDispatcher::~ServiceDispatcher()
{	
	//destroying internal managers
	msgmgr->DetachListener(*(this->devmgr));
	DELOBJ(this->devmgr);
	DELOBJ(this->msgmgr);
	DELOBJ(this->procmgr);
	//closing communication channels
	DisconnectNamedPipe(this->hOutPipeHandle);
	DisconnectNamedPipe(this->hInPipeHandle);
	CloseHandle(this->hOutPipeHandle);
	CloseHandle(this->hInPipeHandle);
}

/*
	Purpose:
		Handles device events
	Parameters:
		lpEventData - device event data
		dwEventType - event code
	Return value:
		None
*/
void ServiceDispatcher::DispatchEvent(LPVOID lpEventData, DWORD dwEventType)
{
	this->msgmgr->ProcessEvent(lpEventData, dwEventType);
}

/*
	Purpose:
		"Pauses" the service's work
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::Pause()
{
	this->devmgr->VolumeManager().Pause();
	this->msgmgr->Pause();
}

/*
	Purpose:
		"Resumes" the service's work
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::Resume()
{
	this->devmgr->VolumeManager().Resume();
	this->msgmgr->Resume();
	this->devmgr->RefreshState();
}

/*
	Purpose:
		Main service routine. Handles incoming requests.
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::HandleRequests(SERVICE_STATUS &status)
{
	try
	{
		//creating the channel
		this->CreateChannel();
		//main service loop
		do
		{
			//buffer for the received information
			OPINFO buffer;
			ZeroMemory(&buffer,sizeof(OPINFO));
			//actual quantity of bytes read
			DWORD rdbytes = 0;
			//calling fileRead
			if (!ReadFile(this->hInPipeHandle, &buffer, sizeof(OPINFO), &rdbytes, NULL))
			{
				//error code
				DWORD errorCode = GetLastError();
				//if the pipe was broken (connection closed)
				if (errorCode == ERROR_BROKEN_PIPE)
				{
					//we try to reconnect
					RevertToSelf();
					DisconnectNamedPipe(this->hOutPipeHandle);
					CloseHandle(this->hInPipeHandle);
					CloseHandle(this->hOutPipeHandle);
					this->CreateChannel();
				}
				//in case if the handles were closed
				else if (errorCode == ERROR_INVALID_HANDLE)
				{
					break;
				}
				//on x64: ERROR_PIPE_LISTENING if the client was not connected
			}
			else
			{
				try
				{
					if (!ImpersonateNamedPipeClient(this->hOutPipeHandle))
					{
						throw WinAPIException(GetLastError());
					}
					//second step - this critical section protects from the 
					//destructor execution during the execution of method
					EnterCriticalSection(&csTermThread);
					//resolving codes to operations
					this->ResolveQuery(&buffer);
				}
				catch(...)
				{
				}
				LeaveCriticalSection(&csTermThread);
			}
		}
		while (status.dwCurrentState != SERVICE_STOPPED);
	}
	catch(...)
	{
		throw;
	}
}

/*
	Purpose:
		Gets the device information and sends it to the named pipe
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::RefreshState()
{
	this->SendDeviceInfo();
}

/*
	Purpose:
		Handles the query codes and calls the appropriate functions
	Parameters:
		pOperInfo - pointer to the OPINFO structure
	Return value:
		None
*/
void ServiceDispatcher::ResolveQuery(POPINFO pOperInfo)
{
	switch(pOperInfo->dwOpCode)
	{
		//refresh device list
	case DEVICE_REFRESH_REQUEST:
		{
			this->devmgr->RefreshState();
			break;
		}
		//try to eject device
	case DEVICE_EJECT_REQUEST:
		{
			this->EjectDevice();
			break;
		}
		//forced ejection
	case DEVICE_EJECT_FORCED:
		{
			//TODO: add handler
			break;
		}
	default:
		{
			break;
		}
	}
}

/*
	Purpose:
		Sends information about the devices to the client
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::SendDeviceInfo()
{
	//sending information about the device operation
	OPINFO info = {DEVICE_REFRESH_ANSWER, OPERATION_START};
	//number of written bytes
	DWORD numwr = 0;
	//starting the operation
	WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
	//sending device list information
	for (size_t i = 0; i < this->devmgr->Devices().size(); ++i)
	{
		this->WalkDeviceTree(1, i, MAXDWORD, i, this->devmgr->Devices()[i]);
	}
	//finishing the operation
	info.dwOpStatus = OPERATION_FINISH;
	WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
}

/*
	Purpose:
		Walks the device tree and saves the information
		about the devices in a structure, then sends it to the client
	Parameters:
		level - current level of the device (1 for USBDev, 2 for drive, 3 for volume)
		index - index of the device
		parent - index of the parent device
		top - top device index
		current - pointer to the current device
	Return value:
		None
*/
void ServiceDispatcher::WalkDeviceTree(DWORD level, DWORD index, DWORD parent,
									   DWORD top, Device *current)
{
	//buffer for mount points
	StringList *mpts;
	//number of written bytes
	DWORD numwr = 0;
	//buffer structure
	PDEVINFO result = new DEVINFO;
	//saving index information
	result->devIndex.dwDeviceLevel = level;
	result->devIndex.dwDeviceNumber = index;
	result->devIndex.dwTopIndex = top;
	//saving device information
	_tcscpy(result->description, current->Description());
	_tcscpy(result->name, current->FriendlyName());
	//saving device parent information
	result->dwParent = parent;
	//working with mount points on volume level
	if (level == 3)
	{
		mpts = dynamic_cast<Volume*>(current)->MountPoints();
		result->dwMountPtsCount = mpts->size();
	}
	else
	{
		result->dwMountPtsCount = 0;
	}
	//sending information about the device operation
	OPINFO info = {DEVICE_REFRESH_ANSWER, OPERATION_PROGRESS};
	//writing to the channel
	WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
	WriteFile(this->hOutPipeHandle, result, sizeof(DEVINFO), &numwr, NULL);
	//sending mount points
	if (level == 3)
	{
		for (size_t i = 0; i < mpts->size(); ++i)
		{
			//riting each mount point
			WriteFile(this->hOutPipeHandle, (*mpts)[i], MAX_PATH, &numwr, NULL);
		}
	}
	DELOBJ(result);
	//calling function for children
	for (size_t i = 0; i<current->Children().size(); ++i)
	{
		//deeper in recursion
		WalkDeviceTree(level+1, i, index, top, current->Children()[i]);
	}
}

/*
	Purpose:
		Performs an attempt to eject the device
	Parameters:
		None
	Return value:
		None
*/
void ServiceDispatcher::EjectDevice()
{
	//device index information
	DEVINDEX index;
	//number of read bytes
	DWORD numrd = 0;
	//reading index information
	ReadFile(this->hInPipeHandle, &index, sizeof(index), &numrd, NULL);
	//trying to remove the device
	if (this->devmgr->Devices()[index.dwTopIndex]->Eject())
	{
		//sending information about successful removal
		OPINFO info = {DEVICE_EJECT_ACCEPT, MAXDWORD};
		DWORD numwr = 0;
		WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
	}
	//the removal failed
	else
	{
		//sending information about failure
		OPINFO info = {DEVICE_EJECT_REJECT, MAXDWORD};
		DWORD numwr = 0;
		WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
		//starting to search processes-blockers
		info.dwOpCode = PROCESS_SEARCH_HANDLES;
		info.dwOpStatus = OPERATION_START;
		WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
		{
			//TODO: map DOS and common mount points (reverse!); maybe with DeviceIoControl
			//or GetMountPoints
			//getting DOS mount point aliases
			StringPairList dmpts = 
				this->devmgr->Devices()[index.dwTopIndex]->DosMountPoints();
			//getting lockers
			LockInfo *lockers = 
				this->procmgr->GetLockers(dmpts);
			this->SendLockInfo(lockers);
			CLRSTRPLIST(dmpts);
		}
		//finishing the search of blockers
		info.dwOpCode = PROCESS_SEARCH_HANDLES;
		info.dwOpStatus = OPERATION_FINISH;
		WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
	}
}

/*
	Purpose:
		Sends information about the process-lockers to the client
	Parameters:
		lockers - list of blocking processes and blocked files
	Return value:
		None
*/
void ServiceDispatcher::SendLockInfo(LockInfo *lockers)
{
	//sending start information
	OPINFO opinfo = {PROCESS_LOCK_INFO, OPERATION_START};
	DWORD numwr = 0;
	WriteFile(this->hOutPipeHandle, &opinfo, sizeof(OPINFO), &numwr, NULL);
	//starting to send locker information
	opinfo.dwOpStatus = OPERATION_PROGRESS;
	//preparing structures for each process
	for (LockInfo::iterator i = lockers->begin();
		i != lockers->end(); ++i)
	{
		//preparing buffer
		PPROC_INFO info = new PROC_INFO;
		//getting process id
		info->dwId = i->first;
		//getting blocked files count
		info->dwLockedFilesCount = i->second.second->size();
		//copying process name
		_tcscpy(info->name, i->second.first);
		//sending information
		WriteFile(this->hOutPipeHandle, &opinfo, sizeof(OPINFO), &numwr, NULL);
		WriteFile(this->hOutPipeHandle, info, sizeof(PROC_INFO), &numwr, NULL);
		//sending file strings
		for (size_t j = 0; j < info->dwLockedFilesCount; ++j)
		{
			WriteFile(this->hOutPipeHandle, (*i->second.second)[j], MAX_PATH, &numwr, NULL);
		}
		DELOBJ(info);
	}
	//sending finish information
	opinfo.dwOpCode = OPERATION_FINISH;
	WriteFile(this->hOutPipeHandle, &opinfo, sizeof(OPINFO), &numwr, NULL);
	//starting to search processes-blockers
}

/*
	Purpose:
		Sends information about the search progress to the client
	Parameters:
		percentage - progress value
	Return value:
		None
*/
void ServiceDispatcher::ReportProgress(byte percentage)
{
	//preparing progress informaton
	OPINFO info = {PROCESS_SEARCH_HANDLES, OPERATION_PROGRESS};
	DWORD numwr = 0;
	//sending information
	WriteFile(this->hOutPipeHandle, &info, sizeof(OPINFO), &numwr, NULL);
	//sending progress percentage
	WriteFile(this->hOutPipeHandle, &percentage, sizeof(byte), &numwr, NULL);
}

/*
	Purpose:
		Terminates the work of service correctly,
		allowing to finish all I/O operations. Call this method from another 
		thread to destroy the dispatcher
	Parameters:
		dispatcher - a pointer to the dispatcher
	Return value:
		None
*/
void ServiceDispatcher::TerminateDispatcherThread(ServiceDispatcher *dispatcher)
{
	//if we are the first, then the dispatcher is destroyed
	//else we wait until the successful execution
	EnterCriticalSection(&csTermThread);
	//terminating dispatcher's thread
	TerminateThread(dispatcher->hDispatcherThread, NO_ERROR);
	//closing thread handle
	CloseHandle(dispatcher->hDispatcherThread);
	//deleting the dispatcher
	DELOBJ(dispatcher);
	LeaveCriticalSection(&csTermThread);
	//deleting critical sections
	DeleteCriticalSection(&csTermThread);
}