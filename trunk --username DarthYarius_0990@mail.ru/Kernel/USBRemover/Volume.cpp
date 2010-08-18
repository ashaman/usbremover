/*
	Volume.cpp
	Started: 18.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Volume class: implementation
*/

#include "Volume.h"

CRITICAL_SECTION csVolumeAccess;


/*
	Constructor
*/
Volume::Volume(LPTSTR path, DeviceDataSet dataSet, HANDLE informationSet): 
	Device(GUID_DEVINTERFACE_VOLUME, path, dataSet, informationSet)
{
	InitializeCriticalSection(&csVolumeAccess);
	this->mountPoints = new StringList();
	RefreshMountPoints();
	GetVolumeInformation();
}

//Default destructor
Volume::~Volume()
{
	DeleteCriticalSection(&csVolumeAccess);
	//clear all C-strings
	CLRSTRLIST(*this->mountPoints);
	DELOBJ(this->mountPoints);
#ifdef DEBUG
	_ASSERT(this->mountPoints == NULL);
#endif
}

/*
	Purpose:
		Eject the device (with its hierarchy)
	Parameters:
		None
	Return value:
		true if the removal was successful,
		false otherwise
*/
bool Volume::Eject()
{
	if (this->parent != NULL)
	{
		return this->parent->Eject();
	}
	return false;
}

/*
	Purpose:
		Notifies the shell about the successful device removal
	Parameters:
		None
	Return value:
		None
*/
void Volume::NotifySystem()
{
	for (unsigned i = 0; i<this->mountPoints->size(); ++i)
	{
		SHChangeNotify(SHCNE_MEDIAREMOVED, SHCNF_PATH, (*this->mountPoints)[i], NULL);
	}
}

/*
	Purpose:
		Refreshes the list of volume mount points
	Parameters:
		None
	Return value:
		None
*/
void Volume::RefreshMountPoints()
{
	EnterCriticalSection(&csVolumeAccess);
	CLRSTRLIST(*this->mountPoints);
	DELOBJ(this->mountPoints);
#ifdef DEBUG
	_ASSERT(this->mountPoints == NULL);
#endif
	this->mountPoints =	GetVolumeMountPoints(this->path);
	LeaveCriticalSection(&csVolumeAccess);
}

/*
	Purpose:
		Gets the information about the volume
	Parameters:
		None
	Return value:
		None
*/
void Volume::GetVolumeInformation()
{
	//volume name
	LPTSTR nameBuf = new TCHAR[MAX_PATH+1];
	ZeroMemory(nameBuf, MAX_PATH+1);
	//file system name
	LPTSTR fsName = new TCHAR[MAX_PATH+1];
	ZeroMemory(fsName, MAX_PATH+1);
	//serial number, file system flags
	DWORD sn, fsFlags, dummy;
	if (!::GetVolumeInformation((*this->mountPoints)[0], nameBuf, MAX_PATH, &sn,
		&dummy, &fsFlags, fsName, MAX_PATH))
	{
		DELARRAY(nameBuf);
		DELARRAY(fsName);
#ifdef DEBUG
		_ASSERT(nameBuf == NULL);
		_ASSERT(fsName == NULL);
#endif
		//TODO: handle error
	}
	else
	{
		//copying volume label
		this->friendlyName = new TCHAR[_tcslen(nameBuf)+1];
		_tcscpy(this->friendlyName, nameBuf);
		DELARRAY(nameBuf);
		DELARRAY(fsName);
#ifdef DEBUG
		_ASSERT(nameBuf == NULL);
		_ASSERT(fsName == NULL);
#endif
	}
	this->diskNumber = GetDiskNumber(this->path);
}

/*
	Purpose:
		Gets DOS-style volume names
	Input parameters:
		None
	Return value:
		A C-string pairs list representing both DOS and GUID volume names
*/
StringPairList Volume::DosMountPoints() const
{
	StringPairList result;
	//formatting volume name
	LPTSTR buffer = ::FormatDOSVolumeName(this->path);
	//allocating result buffer
	LPTSTR resbuf = new TCHAR[MAX_PATH+1];
	ZeroMemory(resbuf, sizeof(TCHAR)*(MAX_PATH+1));
	//querying for DOS device name
	if (QueryDosDevice(buffer, resbuf, MAX_PATH))
	{
		//saving DOS and GUID names
		LPTSTR strpath = new TCHAR[_tcslen(this->path)+1];
		_tcscpy(strpath, this->path);
		result.push_back(StringPair(resbuf, strpath));
	}
	DELARRAY(buffer);
	return result;
}

/*
	Purpose:
		Refreshes the mount point list
	Parameters:
		None
	Return value:
		None
*/
void Volume::RefreshState()
{
	EnterCriticalSection(&csVolumeAccess);
	this->RefreshMountPoints();
	LeaveCriticalSection(&csVolumeAccess);
}