/*
	Drive.cpp
	Started: 24.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Disk drive class implementation
*/

#include "Drive.h"

/*
	Purpose:
		Initialize the disk representation
	Parameters:
		number - disk number in the system
		path - disk system path
		dataSet - container for "volume-disk" mapping
		informationSet - device's information set
*/
DiskDrive::DiskDrive(DWORD number, LPTSTR path, DeviceDataSet dataSet, HANDLE informationSet): 
	Device(GUID_DEVINTERFACE_DISK, path, dataSet, informationSet)
{
	this->diskNumber = number;
	PBYTE buffer = GetDeviceProperty(SPDRP_FRIENDLYNAME);
	this->friendlyName = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->friendlyName, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif
}

/*
	Purpose:
		Overrides base destructor
*/
DiskDrive::~DiskDrive()
{
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
bool DiskDrive::Eject()
{
	if (this->parent != NULL)
	{
		return this->parent->Eject();
	}
	return false;
}