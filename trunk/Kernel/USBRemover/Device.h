/*
	Device.h
	Started: 21.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Device base class
*/
#ifndef _H_DEVICE
#define _H_DEVICE

#ifdef DEBUG
	#define _CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
#endif

//System and wrapper headers
#include <windows.h>
#include <objbase.h>
#include "ApiWrapper.h"
#include "WinAPIException.h"

#include <tchar.h>

//STL and classes headers
#include <vector>

//Forward declaration of the class
class Device;

//Type definition for the device list
typedef std::vector<Device*> DeviceList;

/*
	Abstract device class

	Purpose:
		Represents a logical node of a device tree.
		Wraps Win API values and functions
*/
class Device
{
protected:
	DeviceList *children; //child devices of the current device
	GUID classGuid; //device class GUID
	LPTSTR description; //device description string
	LPTSTR deviceClassName; //device class name
	SP_DEVINFO_DATA deviceInformationData; //device data form Setup API
	HANDLE deviceInformationSet; //device information set
	LPTSTR friendlyName; //for disks and USB devices - name, for volumes - volume label
	LPTSTR guidString; //GUID in a string
	LPTSTR id; //device identifier
	LPTSTR manufacturer; //device manufacturer
	Device *parent; //parent device
	LPTSTR path; //device path

	void GetDeviceInformation(); //get the SetupAPI information
	PBYTE GetDeviceProperty(DWORD propertyCode); //get the device property
	LPTSTR GuidToCString(GUID guid); //convert GUID to string
	void Initialize(GUID classGUID, HANDLE instanceHandle, HANDLE informationSet); //called from constructor
public:
	Device(GUID classGUID, LPTSTR devicePath,
		DeviceDataSet &dataSet, HANDLE informationSet); //for volumes and disks
	Device(GUID classGUID, HANDLE instanceHandle, HANDLE informationSet); //for devices
	virtual ~Device(); //destructor

	void AddChild(Device* device); //adds child device to the hierarchy
	DeviceList& Children() const {return *this->children;}; //child devices
	LPTSTR ClassName() const {return this->deviceClassName;}; //gets the device class name
	LPTSTR Description() const {return this->description;}; //gets the device description
	virtual StringPairList DosMountPoints() const; //DOS aliases for mount points
	virtual bool Eject() = NULL; //virtual eject procedure
	LPTSTR FriendlyName() const {return this->friendlyName;}; //friendly name
	LPTSTR Guid() const {return this->guidString;}; //string representation of GUID
	LPTSTR Id() const {return this->id;}; //string with the ID
	DWORD InstanceHandle() const {return this->deviceInformationData.DevInst;}; //instance handle
	virtual void NotifySystem(); //notofies about the successful removal
	//virtual StringList MountPoints() const; //virtual mount points procedure
	Device& Parent() const {return *this->parent;}; //parent device

	/*
		WARNING!!!
		MountPoints method can cause great problems when a device refresh occurs
	*/
};

#endif