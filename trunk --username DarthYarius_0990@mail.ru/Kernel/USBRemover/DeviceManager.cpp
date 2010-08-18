/*
	DeviceManager.cpp
	Started: 25.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Device manager base class implementation
*/


#include "DeviceManager.h"

//Internal constant for formatting drive name
const LPTSTR DRIVE_PATTERN = _T("\\\\.\\PhysicalDrive%d");

//Critical section for correct device refreshing
CRITICAL_SECTION csRefreshState;

/*
	Purpose:
		Constructor
	Parameters:
		None
*/
DeviceManager::DeviceManager()
{
	InitializeCriticalSection(&csRefreshState);
	this->listeners = new DelegateSet();
	//creating volume manager
	this->volmgr = new ::VolumeManager();
	//refreshing device state
	this->Refresh();
}


/*
	Purpose:
		Deletes and re-gets the information about devices
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::Refresh()
{
	// allocating containers
	this->volumes = new VolumeList();
	this->drives = new DriveList();
	this->sticks = new USBDeviceList();
	
	try
	{
		DeviceDataSet volumeDataSet; //mappings for volumes
		DeviceDataSet driveDataSet; //mappings for drives

		/*
			Fixed 11.08.2010 by Asha'man
			It is OBLIGATORY to DESTROY created device information sets!!!
			Use SetupDiDestroyDeviceInfoList(handle)
		*/

		//pausing volume manager to avoid refreshing
		this->volmgr->Pause();

		//getting device info set handle for volumes
		this->hVolumeInformationSet = 
			::GetDeviceInformationSet(GUID_DEVINTERFACE_VOLUME, DIGCF_PRESENT | DIGCF_DEVICEINTERFACE);
		//getting cache for volume data set
		volumeDataSet = ::GetDeviceInformationCache(GUID_DEVINTERFACE_VOLUME,
			this->hVolumeInformationSet);
		this->GetVolumes(volumeDataSet);
		//attaching device manager to volume manager listeners
		this->volmgr->AttachListener(this);
		//resuming volume manager
		this->volmgr->Resume();

		//getting device info set handle for drives
		this->hDriveInformationSet = 
			::GetDeviceInformationSet(GUID_DEVINTERFACE_DISK, DIGCF_PRESENT | DIGCF_DEVICEINTERFACE);
		//Getting cache for drive data set
		driveDataSet = ::GetDeviceInformationCache(GUID_DEVINTERFACE_DISK,
			this->hDriveInformationSet);
		this->GetDrives(driveDataSet);

		//Linking volumes and drives
		this->LinkVD();

		//getting info set for devices
		this->hDeviceInformationSet = 
			::GetDeviceInformationSet(GUID_DEVINTERFACE_USB_DEVICE, DIGCF_PRESENT | DIGCF_DEVICEINTERFACE);
		//final step - linking altogether
		this->GetDevices();
		//notifying about changes
		this->FireEvent();
	}
	catch (...)
	{
		throw;
	}
}


/*
	Purpose:
		Destructor
*/
DeviceManager::~DeviceManager()
{
	//deleting critical section
	DeleteCriticalSection(&csRefreshState);
	//calls Clean method
	this->Clean();
	//deleting volume manager
	//ONLY AFTER CLEANING!!!
	DELOBJ(this->volmgr);
	DELOBJ(this->listeners);
#ifdef DEBUG
	_ASSERT(this->volmgr == NULL);
#endif
}


/*
	Purpose:
		Cleans all the references in the class and prepares it for refreshing
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::Clean()
{
	/*
		Fixed 12.08.2010 - clean disk number set!!!!
		+ detach volumes from volume manager
	*/
	this->disknums.clear();
		
	//detaching listeners
	if (this->volumes != NULL)
	{
		for (VolumeList::iterator i = this->volumes->begin();
			i != this->volumes->end(); ++i)
		{
			this->volmgr->DetachListener(*i);
		}
	}

	// closing and destroying device information sets
	if (this->hVolumeInformationSet != INVALID_HANDLE_VALUE)
		SetupDiDestroyDeviceInfoList(this->hVolumeInformationSet);
	if (this->hDriveInformationSet != INVALID_HANDLE_VALUE)
		SetupDiDestroyDeviceInfoList(this->hDriveInformationSet);
	if (this->hDeviceInformationSet != INVALID_HANDLE_VALUE)
		SetupDiDestroyDeviceInfoList(this->hDeviceInformationSet);

	//deleting devices and container
	if (this->sticks != NULL)
	{
		for (USBDeviceList::iterator i = this->sticks->begin();
			i != this->sticks->end(); ++i)
		{
			DELOBJ(*i);
#ifdef DEBUG
			_ASSERT(*i == NULL);
#endif
		}
		DELOBJ(this->sticks);
#ifdef DEBUG
		_ASSERT(this->sticks == NULL);
#endif
	}
	//deleting only containers because each
	//device destroys its children
	DELOBJ(this->drives);
	DELOBJ(this->volumes);
#ifdef DEBUG
	_ASSERT(this->drives == NULL);
	_ASSERT(this->volumes == NULL);
#endif
}

/*
	Purpose:
		Implements IMessageListener::RefreshState method
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::RefreshState()
{
	try
	{
		//entering critical section
		EnterCriticalSection(&csRefreshState);
		//refreshing device list
		this->Clean();
		this->Refresh();
	}
	catch(...)
	{
		//leaving critical section in case of exception
		LeaveCriticalSection(&csRefreshState);
		throw;
	}
	LeaveCriticalSection(&csRefreshState);
}

/*
	Purpose:
		Gets all the removable volumes in the system
	Parameters:
		volumeDataSet - mapping between volumes and drives
	Return value:
		None
*/
void DeviceManager::GetVolumes(DeviceDataSet volumeDataSet)
{
	//cache for the volumes
	StringList volumeList;
	//buffer for the strings
	LPTSTR volumeName = new TCHAR[MAX_PATH+1];
	//get the first volume
	HANDLE handle = FindFirstVolume(volumeName, MAX_PATH);
	if (handle == INVALID_HANDLE_VALUE)
	{
		DELARRAY(volumeName);
#ifdef DEBUG
		_ASSERT(volumeName == NULL);
#endif
		throw WinAPIException(GetLastError());
	}
	else
	{
		//save it and get all the other volumes
		volumeList.push_back(volumeName);
		volumeName = new TCHAR[MAX_PATH+1];
		while (FindNextVolume(handle, volumeName, MAX_PATH))
		{
			volumeList.push_back(volumeName);
			volumeName = new TCHAR[MAX_PATH+1];
		}
		FindVolumeClose(handle);
	}
	DELARRAY(volumeName);
#ifdef DEBUG
	_ASSERT(volumeName == NULL);
#endif
	// filtering volumes. Set is for disk numbers
	for (size_t i = 0; i<volumeList.size(); ++i)
	{
		// checking if the volume is located on a
		// removable drive
		if (FilterRemovableVolume(volumeList[i]))
		{
			Volume *volume = new Volume(volumeList[i], volumeDataSet,
				this->hVolumeInformationSet);
			this->volumes->push_back(volume);
			//attaching volumes to mount manager
			this->volmgr->AttachListener(volume);
			// saving disk number to the set
			// if operation returned false then set
			// already contains such a number
			disknums.insert(volume->DiskNumber());
		}
	}
	//clearing volume list
	CLRSTRLIST(volumeList);
}

/*
	Purpose:
		Gets all the removable drives in the system
	Parameters:
		driveDataSet - mapping between volumes and drives
	Return value:
		None
*/
void DeviceManager::GetDrives(DeviceDataSet driveDataSet)
{
	//iterating over set with drive indexes
	for (std::set<DWORD>::iterator i = this->disknums.begin();
		i != this->disknums.end(); ++i)
	{
		//formatting drive name
		LPTSTR buffer = new TCHAR[MAX_PATH+1];
		_stprintf(buffer,DRIVE_PATTERN,*i); 
		//creating a drive
		DiskDrive *drive = new DiskDrive(*i, buffer, driveDataSet,
			this->hDriveInformationSet);
		//saving it
		this->drives->push_back(drive);
		DELARRAY(buffer);
#ifdef DEBUG
		_ASSERT(buffer == NULL);
#endif
	}
}

/*
	Purpose:
		Gets all the removable devices in the system
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::GetDevices()
{
	//buffer for acceleration of operation
	std::map<DWORD,USBDevice*> buffer;
	//parent handle
	DEVINST parentHandle;
	//iterating over drives
	for (DriveList::iterator i = this->drives->begin();
		i != this->drives->end(); ++i)
	{
		DiskDrive *drive = *i;
		//getting parent for each drive
		if (CM_Get_Parent(&parentHandle, drive->InstanceHandle(),
			ZERO_FLAGS) == CR_SUCCESS)
		{
			USBDevice *device;
			//checking if the parent already exists
			//if it is so, we make a link
			//otherwise, we create a new device
			std::map<DWORD,USBDevice*>::iterator dev =
				buffer.find(parentHandle);
			if (dev == buffer.end())
			{
				//creating new USBDevice
				device = new USBDevice((HANDLE)parentHandle, this->hDeviceInformationSet);
				//adding the child drive
				device->AddChild(drive);
				//putting to map
				buffer.insert(std::map<DWORD,USBDevice*>::value_type(parentHandle,device));
				//putting to list
				this->sticks->push_back(device);
			}
			else
			{
				device = dev->second;
				//linking with existing
				device->AddChild(drive);
			}
		}
	}
}

/*
	Purpose:
		Links volumes and drives with each other
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::LinkVD()
{
	//external loop for drives
	for (DriveList::iterator i = this->drives->begin();
		i != this->drives->end(); ++i)
	{
		DiskDrive *drive = *i;
		//internal loop for volumes
		for (VolumeList::iterator j = this->volumes->begin();
			j != this->volumes->end(); ++j)
		{
			Volume *volume = *j;
			/*
				THE KEY POINT OF USBREMOVER 2.0:
				DRIVES AND VOLUMES ARE LINKED BY DISK NUMBER
				Many thanks to Uwe Sieber and Simon Mourier
			*/
			if (volume->DiskNumber() == drive->DiskNumber())
			{
				drive->AddChild(volume);
			}
		}
	}
}


/*
	Purpose:
		Refreshes the managers state
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::RefreshVolumeState()
{
	try
	{
		//entering critical section
		EnterCriticalSection(&csRefreshState);
		//firing an event
		this->FireEvent();
	}
	catch(...)
	{
		//leaving critical section in case of exception
		LeaveCriticalSection(&csRefreshState);
		throw;
	}
	LeaveCriticalSection(&csRefreshState);
}

/*
	Purpose:
		Attaches a listener
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::AttachListener(IMessageListener *listener)
{
	this->listeners->insert(listener);
}


/*
	Purpose:
		Detaches a listener
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::DetachListener(IMessageListener *listener)
{
	this->listeners->erase(listener);
}


/*
	Purpose:
		Fires an event
	Parameters:
		None
	Return value:
		None
*/
void DeviceManager::FireEvent()
{
	for (DelegateSet::iterator i = this->listeners->begin();
		i != this->listeners->end(); ++i)
	{
		(*i)->RefreshState();
	}
}