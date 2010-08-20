/*
	Device.h
	Started: 22.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Device base class implementation
*/
#include "Device.h"

/*
	Base version of constructor
	Parameters:
		classGUID - device's class GUID
		devicePath - a string representing the name of the device
		dataSet - device data set containing mapping "volume-disk"
		informationSet - device information set handle
*/
Device::Device(GUID classGUID, LPTSTR devicePath, DeviceDataSet &dataSet, HANDLE informationSet)
{
	//copying the GUID
	this->classGuid = classGUID;
	//copy the device path
	this->path = new TCHAR[_tcslen(devicePath)+1];
	_tcscpy(this->path, devicePath);
	
	LPTSTR tmpName = ::FormatDeviceName(devicePath);

	HANDLE deviceHandle = CreateFile(tmpName, GENERIC_READ,
		FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
		ZERO_FLAGS, ZERO_HANDLE);
	if (deviceHandle == INVALID_HANDLE_VALUE)
	{
		DELARRAY(tmpName);
#ifdef DEBUG
		_ASSERT(tmpName == NULL);
#endif
		throw WinAPIException(GetLastError());
	}
	else
	{
		//getting device number
		STORAGE_DEVICE_NUMBER sdn = GetDeviceNumber(deviceHandle);
		//key for device data set
		std::pair<DWORD,DWORD> key(sdn.DeviceNumber, sdn.PartitionNumber);
		//getting data from hashtable
		this->deviceInformationData = dataSet[key];
		//initialize the device fields
		Initialize(this->classGuid, (HANDLE)this->deviceInformationData.DevInst,
			informationSet);
		//Getting SetupAPI information
		this->GetDeviceInformation();
		//close the opened handle
		CloseHandle(deviceHandle);
		//delete buffer
		DELARRAY(tmpName);
#ifdef DEBUG
		_ASSERT(tmpName == NULL);
#endif
	}
}

/*
	Second version of constructor
	Parameters:
		classGUID - device's class GUID
		instanceHandle - handle to the device's instance
		informationSet - device information set handle
*/
Device::Device(GUID classGUID, HANDLE instanceHandle, HANDLE informationSet)
{
	//copy the GUID
	this->classGuid = classGUID;
	//initialize the device fields
	Initialize(classGUID, instanceHandle, informationSet);
	//get the device information data
	this->deviceInformationData = ::GetDeviceInformation(this->id, this->deviceInformationSet);
	//Getting SetupAPI information
	this->GetDeviceInformation();
}

/*
	Purpose:
		Sets all the fields in the device class
	Parameters:
		classGUID - GUID
		instanceHandle - device's instance handle
	Return value:
		None
*/
void Device::Initialize(GUID classGUID, HANDLE instanceHandle, HANDLE informationSet)
{
	//save GUID to string
	this->guidString = GuidToCString(classGUID);
	//prepare the children array
	this->children = new DeviceList(0);
	//setting parent to NULL
	this->parent = NULL;
	//get device id
	this->id = GetDeviceId(instanceHandle);
	//save device info set
	this->deviceInformationSet = informationSet;
}

/*
	Purpose:
		Destructor implementation
		Each device destroys itself and its children
	Parameters:
		None
*/
Device::~Device()
{
	//Clear all the strings
	DELARRAY(this->description);
	DELARRAY(this->deviceClassName);
	DELARRAY(this->friendlyName);
	DELARRAY(this->guidString);
	DELARRAY(this->manufacturer);
	DELARRAY(this->id);
	DELARRAY(this->path);
#ifdef DEBUG
	_ASSERT(this->description == NULL);
	_ASSERT(this->deviceClassName == NULL);
	_ASSERT(this->friendlyName == NULL);
	_ASSERT(this->guidString == NULL);
	_ASSERT(this->manufacturer == NULL);
	_ASSERT(this->id == NULL);
	_ASSERT(this->path == NULL);
#endif
	if (this->children != NULL)
	{
		//Clear all the children devices
		for (size_t i = 0; i<this->children->size(); ++i)
		{
			DELOBJ((*this->children)[i]);
#ifdef DEBUG
			_ASSERT((*this->children)[i] == NULL);
#endif
		}
		DELOBJ(this->children);
#ifdef DEBUG
		_ASSERT(this->children == NULL);
#endif
	}
}

/*
	Purpose:
		Link two devices by the "parent-child" relation
	Parameters:
		device - Pointer to the child device
	Return value:
		None
*/
void Device::AddChild(Device *device)
{
	device->parent = this;
	this->children->push_back(device);
}

/*
	Purpose:
		Converts GUID structure to a string
	Parameters:
		guid - A GUID value
	Return value:
		C-string representing GUID
*/
LPTSTR Device::GuidToCString(GUID guid)
{
	//+1 for sz - string-zero
	LPTSTR result = new TCHAR[GUID_LENGTH+1];
	ZeroMemory(result, sizeof(TCHAR)*(GUID_LENGTH+1));
	if (StringFromGUID2(guid, result, GUID_LENGTH) == 0)
	{
		// error - buffer too small
		throw WinAPIException(ERROR_INSUFFICIENT_BUFFER);
	}
	return result;
}

/*
	Purpose:
		Get the device's property by its code
	Parameters:
		propertyCode - DWORD code of property to get
	Return value:
		PBYTE array of data
*/
PBYTE Device::GetDeviceProperty(DWORD propertyCode)
{
	PBYTE result = new BYTE[2*MAX_PATH];
	ZeroMemory(result, 2*MAX_PATH*sizeof(BYTE));
	DWORD dataType = 0, requiredSize = 0;
	
	if (!SetupDiGetDeviceRegistryProperty(this->deviceInformationSet, &this->deviceInformationData,
		propertyCode, &dataType, result, 2*MAX_PATH*sizeof(BYTE), &requiredSize))
	{
		//throw WinAPIException(GetLastError());
	}
	return result;
}

/*
	Purpose:
		Get the most important device information from the registry
	Parameters:
		None
	Return value:
		None
*/
void Device::GetDeviceInformation()
{
	//copying the device description
	PBYTE buffer = GetDeviceProperty(SPDRP_DEVICEDESC);
	this->description = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->description, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif
	//copying the device class name
	buffer = GetDeviceProperty(SPDRP_CLASS);
	this->deviceClassName = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->deviceClassName, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif
	//copying the manufacturer's name
	buffer = GetDeviceProperty(SPDRP_MFG);
	this->manufacturer = new TCHAR[_tcslen((TCHAR*)buffer)+1];
	_tcscpy(this->manufacturer, (TCHAR*)buffer);
	DELARRAY(buffer);
#ifdef DEBUG
	_ASSERT(buffer == NULL);
#endif
}


/*
	Purpose:
		Get the list of device mount points
	Parameters:
		None
	Return value:
		A list of the mount points belonging to the device
*/
//StringList Device::MountPoints() const
//{
//	StringList result;
//	for (size_t i = 0; i < this->children->size(); ++i)
//	{
//		StringList childMp = (*this->children)[i]->MountPoints();
//		//copying all mount points from the device's children
//		for (size_t j = 0; j < childMp.size(); ++j)
//		{
//			LPTSTR buffer = new TCHAR[_tcslen(childMp[j])+1];
//			_tcscpy(buffer,childMp[j]);
//			result.push_back(buffer);
//		}
//		CLRSTRLIST(childMp);
//	}
//	return result;
//}

/*
	Purpose:
		Notifies the system about the successful removal
	Parameters:
		None
	Return value:
		None
*/
void Device::NotifySystem()
{
	for (unsigned i = 0; i<this->children->size(); ++i)
	{
		(*this->children)[i]->NotifySystem();
	}
}

/*
	Purpose:
		Get the list of device names in DOS style
	Parameters:
		None
	Return value:
		A list of pairs - the DOS name and the unique GUID name
*/
StringPairList Device::DosMountPoints() const
{
	StringPairList result;
	for (size_t i = 0; i < this->children->size(); ++i)
	{
		StringPairList childMp = (*this->children)[i]->DosMountPoints();
		//copying all DOS mount points from the device's children
		for (size_t j = 0; j < childMp.size(); ++j)
		{
			LPTSTR dosbuffer = new TCHAR[_tcslen(childMp[j].first)+1];
			LPTSTR ntbuffer = new TCHAR[_tcslen(childMp[j].second)+1];
			_tcscpy(dosbuffer,childMp[j].first);
			_tcscpy(ntbuffer,childMp[j].second);
			result.push_back(StringPair(dosbuffer, ntbuffer));
		}
		CLRSTRPLIST(childMp);
	}
	return result;
}