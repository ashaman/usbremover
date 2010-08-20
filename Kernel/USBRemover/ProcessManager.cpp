/*
	ProcessManager.cpp
	Started: 14.08.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Process manager class: implementation
*/
#include "ProcessManager.h"

/*
	Purpose:
		Constructor. Gets the information about
		file type in system
	Parameters:
		None
*/
ProcessManager::ProcessManager()
{
	//allocating new process list
	this->lockerList = new LockInfo();
	//getting file handle type
	this->fileHandleType = ::GetFileHandleType();
	//NULLing the listener pointer
	this->listener = NULL;
}

/*
	Purpose:
		Destructor
*/
ProcessManager::~ProcessManager()
{
	this->Clear();
	DELOBJ(this->lockerList);
}

/*
	Purpose:
		Clears the locker list
	Parameters:
		None
	Return value:
		None
*/
void ProcessManager::Clear() const
{
	for (LockInfo::iterator i = this->lockerList->begin();
		i != this->lockerList->end(); ++i)
	{
		//cleaning file strings
		CLRSTRLIST(*i->second.second);
		//cleaning file name
		delete [] i->second.first;
	}
	this->lockerList->clear();
}

/*
	Purpose:
		Gets the processes keeping files on the device
	Parameters:
		mountPoints - list of device's mount points
	Return value:
		A list of LockInfo - process name, its handle and the list of files
*/
LockInfo* ProcessManager::GetLockers(StringPairList mountPoints) const
{
	//clearing old information
	this->Clear();
	//getting system handle table
	PSYSTEM_HANDLE_INFORMATION handleInfo = (PSYSTEM_HANDLE_INFORMATION)
		::GetSystemInformationTable(SystemHandleInformation);
	//getting system processes table
	PSYSTEM_PROCESS_INFORMATION processInformation = (PSYSTEM_PROCESS_INFORMATION)
		::GetSystemInformationTable(SystemProcessInformation);
	try
	{
		//iterating over system handles
		for (size_t i = 0; i < handleInfo->uCount; ++i)
		{
			//choosing only file handles
			if (handleInfo->aSH[i].ObjectType == this->fileHandleType)
			{
				//opening owner process with handle dupping
				HANDLE hProcess = OpenProcess(PROCESS_DUP_HANDLE,
					FALSE, handleInfo->aSH[i].uIdProcess);
				if (hProcess)
				{
					HANDLE hFile;

					//duplicating handle to obtain file handle
					if (DuplicateHandle(hProcess, (HANDLE)handleInfo->aSH[i].Handle,
						GetCurrentProcess(), &hFile, ZERO_FLAGS, FALSE, DUPLICATE_SAME_ACCESS))
					{
						//getting file name
						LPTSTR fileName = ::GetFileName(hFile);
						//if file name was got successfully, then we try
						//to find its process-owner
						if (fileName != NULL)
						{
							//trying to find the matching beginnings of file paths
							for (size_t j = 0; j < mountPoints.size(); ++j)
							{
								//if a drive path was found in the file name
								//we get the locker handle
								LPTSTR substrPtr = _tcsstr(fileName, mountPoints[j].first);
								if (substrPtr != NULL)
								{
									//getting process name
									LPTSTR processName = ::GetProcessName(processInformation,
										handleInfo->aSH[i].uIdProcess);

									//formatting file name
									StringList *mpbuf = 
										::GetVolumeMountPoints(mountPoints[j].second);
									LPTSTR newFileName = NULL;
									//counting the buffer size
									size_t dosnamelen = _tcslen(mountPoints[j].first);
									size_t len = _tcslen((*mpbuf)[0])+_tcslen(fileName)-dosnamelen+1;
									newFileName = new TCHAR[len];
									//formatting the string
									_stprintf(newFileName, _T("%s%s"), (*mpbuf)[0], 
										fileName + dosnamelen + 1);
									//cleaning memory
									CLRSTRLIST(*mpbuf);
									DELOBJ(mpbuf);
									
									//adding locker info
									//if the value already presents in the map
									//we add a new string to the list
									LockInfo::iterator sRes = 
										this->lockerList->find(handleInfo->aSH[i].uIdProcess);
									if (sRes != this->lockerList->end())
									{
										DELARRAY(processName);
										sRes->second.second->push_back(newFileName);
									}
									//otherwise, we insert a new pair
									else
									{
										StringList *tempList = new StringList();
										tempList->push_back(newFileName);
										Process process(processName, tempList);
										this->lockerList->insert(
											LockInfo::value_type(handleInfo->aSH[i].uIdProcess,
											process));
									}
									DELARRAY(fileName);
									break;
								}
							}
						}
						//closing file handle
						CloseHandle(hFile);
					}
					//closing process handle
					CloseHandle(hProcess);
				}
			}
			//reporting about the progress
			this->FireEvent((byte)(100*i/handleInfo->uCount));
		}
	}
	catch(...)
	{
		DELARRAY(handleInfo);
		DELARRAY(processInformation);
		throw;
	}
	DELARRAY(handleInfo);
	DELARRAY(processInformation);
	return this->lockerList;
}

/*
	Purpose:
		Fires a callback event
	Parameters:
		percentage - percentage of operation execution
	Return value:
		None
*/
void ProcessManager::FireEvent(byte percentage) const
{
	if (this->listener != NULL)
	{
		this->listener->ReportProgress(percentage);
	}
}

/*
	Purpose:
		Attaches a listener
	Parameters:
		listener - a pointer to the callback listener
	Return value:
		None
*/
void ProcessManager::AttachListener(IProgressCallbackListener *listener)
{
	this->listener = listener;
}

/*
	Purpose: 
		Detaches a listener
	Parameters:
		listener - a pointer to the callback listener
	Return value:
		None
*/
void ProcessManager::DetachListener(IProgressCallbackListener *listener)
{
	this->listener = NULL;
};
