/*
	USBDevice.cpp
	Started: 18.06.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>
	
	USBDevice class: implementation
*/

#include "USBDevice.h"

/*
	Purpose:
		Constructor
	Parameters:
		instanceHandle - device's instance handle in the device info set
		informationSet - device info set
*/
USBDevice::USBDevice(HANDLE instanceHandle, HANDLE informationSet):
	Device(GUID_DEVINTERFACE_USB_DEVICE, instanceHandle, informationSet)
{
	PBYTE buffer = GetDeviceProperty(SPDRP_LOCATION_INFORMATION);
	this->friendlyName = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->friendlyName, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif
	buffer = GetDeviceProperty(SPDRP_PHYSICAL_DEVICE_OBJECT_NAME);
	this->path = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->path, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif

}

/*
	Purpose:
		Destructor
*/
USBDevice::~USBDevice()
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
bool USBDevice::Eject()
{
	if (EjectDevice(this->InstanceHandle()))
	{
		this->NotifySystem();
		return true;
	}
	return false;
}