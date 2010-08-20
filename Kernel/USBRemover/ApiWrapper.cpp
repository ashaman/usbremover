/*
	ApiWrapper.cpp
	Started: 21.05.2010
	Author: Asha'man
	Email: DarthYarius_0990@mail.ru
	License: LGPL v3 <?>

	Implementation of wrapper for Windows API functions
*/

#include "ApiWrapper.h"

//Internal constants
const DWORD SIZE_MAGIC_VALUE = (DWORD)5; //magic value needed in some functions
const LPTSTR VOLNAME_MASK = _T("\\\\.\\%s"); //mask for the volume name
const TCHAR QUESTION_TAG = _T('?'); //question tag used as separator
const TCHAR VOLUME_SEPARATOR = _T('.'); //subsitution for question tag in volume name
const int SEPARATOR_POS = (int)2; //separator position
const int VOLUME_OFFSET = (int)4; //offset for the volume name
const LPTSTR FLOPPY_DOS = _T("\\Device\\Floppy"); //floppy device pattern
const LPTSTR FLOPPY_WIN = _T("\\\\?\\fdc#"); //floppy windows manager pattern
const TCHAR EOLN = _T('\0'); //string separator

/*
	Purpose:
		Get the handle for the device information set
	Parameters:
		classGUID - the GUID of the device class
		flags - filter for the device
	Return value:
		Handle to the device information set (see MSDN for further info)

	Verified 27.05.2010 by Asha'man
*/
HANDLE WINAPI GetDeviceInformationSet(GUID classGUID, DWORD flags)
{
	return SetupDiGetClassDevs(&classGUID, NULL, 0, flags);
} //GetDeviceInformationSet

/*
	Purpose:
		Get the device string identifier by the device's instance handle
	Parameters:
		instanceHandle - handle of the device in the device information set
	Return value:
		A C-string which represents the device's ID in the system
	Throws:
		WinAPI exception

	Verified 27.05.2010 by Asha'man
*/
LPTSTR WINAPI GetDeviceId(HANDLE instanceHandle)
{
	//allocate new memory + 1 for zero
	LPTSTR idBuffer = NULL;
	//get device ID by its device handle in the device information set
	try
	{
		idBuffer = new TCHAR[MAX_PATH+1];
		if (CM_Get_Device_ID((DEVINST)instanceHandle, idBuffer, MAX_PATH, ZERO_FLAGS) != CR_SUCCESS)
		{
			throw WinAPIException(GetLastError());
		}
	}
	catch (...)
	{
		DELARRAY(idBuffer);
#ifdef DEBUG
		_ASSERT(idBuffer == NULL);
#endif
		throw;
	}
	return idBuffer;	
}

/*
	Purpose:
		Get the device information by its instance handle in the device
		information set
	Parameters:
		idString - string which stores the device ID
		setHandle - handle of the device information set
	Return value:
		An SP_DEVINFO_DATA structure which defines the device instance that
		is a member of the specified device information set.

	Verified 27.05.2010 by Asha'man
*/
SP_DEVINFO_DATA WINAPI GetDeviceInformation(PTCHAR idString, HANDLE setHandle)
{
	SP_DEVINFO_DATA result;
	//set result size
	result.cbSize = sizeof(result);
	//opening device information
	try
	{
		if (!SetupDiOpenDeviceInfo(setHandle, idString, 0, ZERO_FLAGS, &result))
		{
			throw WinAPIException(GetLastError());
		}
	}
	catch(...)
	{
		throw;
	}
	return result;
} //GetDeviceInformation

/*
	Purpose:
		Get the device information by its instance handle in the device
		information set and save it to the cache
	Parameters:
		instanceHandle - handle of the device in the device information set
		setHandle - handle of the device information set
	Return value:
		A DeviceDataSet cache with all the necessary devices

	Verified 27.05.2010 by Asha'man
	Fixed 11.08.2010 by Asha'man
*/
DeviceDataSet WINAPI
GetDeviceInformationCache(GUID classGUID, HANDLE setHandle)
{
	//TO BE FOUND
	SP_DEVINFO_DATA deviceData; //information about the device
	DeviceDataSet dataSet; //device data set to be returned
	
	//AUXILIARY DATA
	DWORD index = 0; //index of the current element
	SP_DEVICE_INTERFACE_DATA deviceInterfaceData; //information about the device interface
	PSP_DEVICE_INTERFACE_DETAIL_DATA deviceInterfaceDetailData; //interface details
	DWORD difaceBufferSize; //size of the device interface detail structure
	DWORD dwSize = 0; //variable needed for function call. Indicates the size required for
				//the structure
	try
	{
		deviceInterfaceData.InterfaceClassGuid = classGUID;
		deviceInterfaceData.cbSize = sizeof(deviceInterfaceData);

		//iteration over the device information set
		while (SetupDiEnumDeviceInterfaces((HDEVINFO)setHandle, NULL, &classGUID,
			index, &deviceInterfaceData))
		{
			//prepare for the function call
			deviceData.cbSize = sizeof(deviceData);
			/* 
				fixed 11.08.2010 - from MSDN pages 
			*/
			deviceInterfaceDetailData = (PSP_DEVICE_INTERFACE_DETAIL_DATA)
				(new BYTE[sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA)]);
			//deviceInterfaceDetailData->cbSize = 
			//	offsetof(SP_DEVICE_INTERFACE_DETAIL_DATA, DevicePath) + sizeof(TCHAR);

			difaceBufferSize = sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA);
			deviceInterfaceDetailData->cbSize = difaceBufferSize;	
			
			/*
				Short description of SetupDiGetDeviceInterfaceDetail parameters
				Taken from MSDN

				DeviceInfoSet
				A pointer to the device information set that contains the
				interface for which to retrieve details

				DeviceInterfaceData 
				A pointer to an SP_DEVICE_INTERFACE_DATA structure that specifies
				the interface in DeviceInfoSet for which to retrieve details

				DeviceInterfaceDetailData 
				A pointer to an SP_DEVICE_INTERFACE_DETAIL_DATA structure
				to receive information about the specified interface

				DeviceInterfaceDetailDataSize 
				The size of the DeviceInterfaceDetailData buffer. The buffer must be
				at least (offsetof(SP_DEVICE_INTERFACE_DETAIL_DATA, DevicePath) + sizeof(TCHAR))
				bytes

				RequiredSize 
				A pointer to a variable of type DWORD that receives the required size of the
				DeviceInterfaceDetailData buffer

				DeviceInfoData 
				A pointer buffer to receive information about the device that supports the
				requested interface

			*/
			if (!SetupDiGetDeviceInterfaceDetail((HDEVINFO)setHandle, &deviceInterfaceData,
				deviceInterfaceDetailData, difaceBufferSize, &dwSize, &deviceData))
			{
				//buffer is too small to contain data
				//calling function again
				if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
				{
					/*
						Fixed 11.08.2010 by Asha'man
						Magic value is no more needed
					*/
					difaceBufferSize = dwSize;
					//reallocating memory for data
					DELARRAY(deviceInterfaceDetailData);
#ifdef DEBUG
					_ASSERT(deviceInterfaceDetailData == NULL);
#endif
					deviceInterfaceDetailData = (PSP_DEVICE_INTERFACE_DETAIL_DATA)
						(new BYTE[dwSize]);
					deviceInterfaceDetailData->cbSize = sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA);
						//offsetof(SP_DEVICE_INTERFACE_DETAIL_DATA, DevicePath) + sizeof(TCHAR);

					//call it again
					if (!SetupDiGetDeviceInterfaceDetail((HDEVINFO)setHandle, &deviceInterfaceData,
						deviceInterfaceDetailData, difaceBufferSize, &dwSize, &deviceData))
					{
						throw WinAPIException(GetLastError());
					}
				}
				else
				{
					throw WinAPIException(GetLastError());
				}
			}
			//Fixed 11.08.2010 by Asha'man - avoid floppy drives
			if (_tcsstr(deviceInterfaceDetailData->DevicePath, FLOPPY_WIN) == NULL)
			{
				//open the device as a file
				HANDLE deviceHandle = CreateFile(deviceInterfaceDetailData->DevicePath,
					GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
					ZERO_FLAGS, ZERO_HANDLE);
				if (deviceHandle == INVALID_HANDLE_VALUE)
				{
					throw WinAPIException(GetLastError());
				}
				else
				{
					//get device DeviceNumber and PartitionNumber in order to
					//obtain a key for the map
					//cache the obtained information
					STORAGE_DEVICE_NUMBER deviceNumber;
					try
					{
						deviceNumber = GetDeviceNumber(deviceHandle);
						dataSet.insert(DeviceDataSet::value_type(
							std::make_pair(deviceNumber.DeviceNumber,
							deviceNumber.PartitionNumber),deviceData));
					}
					//if WinAPIExcwption - that means that some problems occured
					//with getting device number
					catch(WinAPIException&)
					{
					}
					catch(...)
					{
						throw;
					}
					//close the handle after all
					CloseHandle(deviceHandle);
				}
			}
			/*
				Fixed by Asha'man 11.08.2010
			*/
			DELARRAY(deviceInterfaceDetailData);
#ifdef DEBUG
			_ASSERT(deviceInterfaceDetailData == NULL);
#endif
			++index;
		}
	}
	catch (...)
	{
		DELARRAY(deviceInterfaceDetailData);
#ifdef DEBUG
		_ASSERT(deviceInterfaceDetailData == NULL);
#endif
		throw;
	}
	return dataSet;
}


/*
	Purpose:
		Get the device number via DeviceIoControl
	Parameters:
		fileHandle - handle of the device (got by CreateFile)
	Return value:
		The STORAGE_DEVICE_NUMBER structure. It is used in conjunction
		with the IOCTL_STORAGE_GET_DEVICE_NUMBER request to retrieve
		the FILE_DEVICE_XXX device type, the device number, and,
		for a device that can be partitioned, the partition number
		assigned to a device by the driver when the device is started. 

	See MSDN for further information

	Verified 27.05.2010 by Asha'man
	Tested 27.05.2010 - Works correctly
*/
STORAGE_DEVICE_NUMBER WINAPI GetDeviceNumber(HANDLE fileHandle)
{
	STORAGE_DEVICE_NUMBER result;
	DWORD dummy;
	try
	{
		//call DeviceIoControl with the parameter IOCTL_STORAGE_GET_DEVICE_NUMBER
		if (!DeviceIoControl(fileHandle, IOCTL_STORAGE_GET_DEVICE_NUMBER, NULL, 0,
			(LPVOID)&result, sizeof(result), &dummy, NULL))
		{
			throw WinAPIException(GetLastError());
		}
	}
	catch(...)
	{
		throw;
	}
	return result;
} //GetDeviceNumber

/*
	Purpose:
		Get the bus type for the specified device
	Parameters:
		path - Path do the device
	Return value
		Member of the enumeration STORAGE_BUS_TYPE

	Verified 27.05.2010 by Asha'man
	Tested 27.05.2010 - Works correctly
*/
STORAGE_BUS_TYPE WINAPI GetBusType(LPTSTR name)
{
	//open device as file
	LPTSTR path = NULL;
	STORAGE_BUS_TYPE result = BusTypeUnknown;
	try
	{
		path = ::FormatDeviceName(name);
		HANDLE handle = CreateFile(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
		if (handle == INVALID_HANDLE_VALUE)
		{
			throw WinAPIException(GetLastError());
		}
		//define all the necessary parameters for the function call
		STORAGE_DEVICE_DESCRIPTOR deviceDescriptor;
		STORAGE_PROPERTY_QUERY propertyQuery;
		//free memory
		ZeroMemory(&deviceDescriptor, sizeof(deviceDescriptor));
		ZeroMemory(&propertyQuery, sizeof(propertyQuery));
		deviceDescriptor.Size = sizeof(deviceDescriptor);
		DWORD dummy;
		if (!DeviceIoControl(handle, IOCTL_STORAGE_QUERY_PROPERTY, &propertyQuery, sizeof(propertyQuery),
			&deviceDescriptor, sizeof(deviceDescriptor), &dummy, ZERO_FLAGS))
		{
			CloseHandle(handle);
			throw WinAPIException(GetLastError());
		}
		result = deviceDescriptor.BusType;
		//close the handle
		CloseHandle(handle);
		DELARRAY(path);
#ifdef DEBUG
		_ASSERT(path == NULL);
#endif
	}
	catch(...)
	{
		DELARRAY(path);
#ifdef DEBUG
		_ASSERT(path == NULL);
#endif
		throw;
	}
	return result;
}

/*
	Purpose:
		Ejection of the device
		WARNING: USE ONLY FOR THE USB TOP DEVICE!!!
	Parameters:
		devInst - device instance handle
	Return value:
		true

	Verified 27.05.2010 by Asha'man
	Updated 10.08.2010 by Asha'man
*/
bool WINAPI EjectDevice(DEVINST devInst)
{
	//Prepare for the function call
	TCHAR vetoName[MAX_PATH+1];
	ZeroMemory(&vetoName, sizeof(TCHAR)*(MAX_PATH+1));
	//call ejection
	CONFIGRET result = CM_Request_Device_Eject(devInst, NULL, vetoName, MAX_PATH, ZERO_FLAGS);
	return (result == CR_SUCCESS) && (vetoName[0] == EOLN);
}

/*
	Purpose:
		Formats the device name to be compatible with the
		CreateFile function
	Parameters:
		devName - C-string representing device name.
		It is not affected inside the function
	Return value:
		A C-string repesenting formatted volume name

	Verified 27.05.2010 by Asha'man
	Tested 27.05.2010 - Works correctly
*/
LPTSTR FormatDeviceName(LPTSTR devName)
{
	LPTSTR result = NULL;
	//length of the input
	size_t len = _tcslen(devName);
	//for volume - copy and format
	if (devName[SEPARATOR_POS] == QUESTION_TAG)
	{
		//allocate new string with length len-1.
		//Note: the last is for zero(sz)
		result = new TCHAR[len];
		//clean memory
		ZeroMemory(result, sizeof(TCHAR)*len);
		//copy all sympols except the last one
		_tcsncpy(result, devName, len-1);
		//substitute question tag by the dot
		result[SEPARATOR_POS] = VOLUME_SEPARATOR;
	}
	//for drive - only copy
	else
	{
		result = new TCHAR[len+1];
		_tcscpy(result, devName);
	}
	return result;
}

/*
	Purpose:
		Formats the volume name to be compatible with
		the QueryDOSDevice function
	Parameters:
		volName - C-string representing volume name
		It is not affected inside the function
	Return value:
		A C-string representing DOS formatted volume name
		
	Verified 27.05.2010 by Asha'man
	Tested 27.05.2010 - Works correctly
*/
LPTSTR FormatDOSVolumeName(LPTSTR volName)
{
	//string length
	size_t len = _tcslen(volName);
	//allocate new memory for result
	//1 symbol for '\0' - sz
	LPTSTR result = new TCHAR[len-VOLUME_OFFSET];
	ZeroMemory(result, sizeof(TCHAR)*(len-VOLUME_OFFSET));
	//copy the reslut. The last -1 is for trailing backslash
	_tcsncpy(result, volName+VOLUME_OFFSET, len-VOLUME_OFFSET-1);
	return result;
}

/*
	Purpose:
		Filters only removable volumes.
		Excludes floppies
	Parameters:
		path - Path to the volume
	Return value:
		true, if the volume is removable
		false, otherwise

	Verified 27.05.2010 by Asha'man
	Tested 27.05.2010 - Works correctly
*/
bool FilterRemovableVolume(LPTSTR path)
{
	//result
	bool result = false;
	LPTSTR dospath = NULL;
	LPTSTR buffer = NULL;

	//checking if the volume belongs to the removable drive
	try
	{
		if (GetDriveType(path) == DRIVE_REMOVABLE)
		{
			//formatting device path
			dospath = FormatDOSVolumeName(path);
			//allocate buffer
			buffer = new TCHAR[MAX_PATH+1];
			ZeroMemory(buffer, sizeof(TCHAR)*(MAX_PATH+1));
			//get DOS device name to filter floppies
			if (!QueryDosDevice(dospath, buffer, MAX_PATH))
			{
				throw WinAPIException(GetLastError());
			}
			else
			{
				//check if it is a floppy drive
				if (_tcsstr(buffer, FLOPPY_DOS) == NULL)
				{
					result = true; //not a floppy drive
				}
			}
			DELARRAY(buffer);
			DELARRAY(dospath);
#ifdef DEBUG
			_ASSERT(buffer == NULL);
			_ASSERT(dospath == NULL);
#endif
		}
	}
	catch(...)
	{
		DELARRAY(buffer);
		DELARRAY(dospath);
#ifdef DEBUG
		_ASSERT(buffer == NULL);
		_ASSERT(dospath == NULL);
#endif
		throw;
	}
	return result;
}

/*
	Purpose:
		Gets the volume size (in bytes)
	Parameters:
		path - Path to the volume
	Return value:
		A LARGE_INTEGER value representing the volume size

	Verified 18.06.2010 by Asha'man
*/
LARGE_INTEGER WINAPI GetVolumeSize(LPTSTR path)
{
	GET_LENGTH_INFORMATION lengthInfo;
	LPTSTR volName = ::FormatDeviceName(path);
	try
	{
		//Opening device as a file
		HANDLE handle = CreateFile(volName, GENERIC_READ, FILE_SHARE_READ |
			FILE_SHARE_WRITE, NULL, OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
		if (handle == INVALID_HANDLE_VALUE)
		{
			throw WinAPIException(GetLastError());
		}
		else
		{
			//prepare for the DeviceIoControl call
			DWORD dummy;
			ZeroMemory(&lengthInfo, sizeof(lengthInfo));
			//calling DeviceIoControl for getting disk size
			if (!DeviceIoControl(handle, IOCTL_DISK_GET_LENGTH_INFO,
				NULL, 0, &lengthInfo, sizeof(lengthInfo), &dummy, ZERO_HANDLE))
			{
				CloseHandle(handle);
				throw WinAPIException(GetLastError());
			}
			else
			{
				DELARRAY(volName);
#ifdef DEBUG
				_ASSERT(volName == NULL);
#endif
				CloseHandle(handle);
			}
		}
	}
	catch(...)
	{
		DELARRAY(volName);
#ifdef DEBUG
		_ASSERT(volName == NULL);
#endif
		throw;
	}
	return lengthInfo.Length;
}

/*
	Purpose:
		Gets the disk number for a volume
	Parameters:
		path - Path to the volume
	Return value:
		A DWORD value representing the number of a disk

	Verified 18.06.2010 by Asha'man
	Tested 18.06.2010 - Works correctly
*/
DWORD WINAPI GetDiskNumber(LPTSTR path)
{
	DWORD result;
	LPTSTR volName = ::FormatDeviceName(path);
	PVOLUME_DISK_EXTENTS extents = (PVOLUME_DISK_EXTENTS)new BYTE[sizeof(VOLUME_DISK_EXTENTS)];
	try
	{
		HANDLE handle = CreateFile(volName, GENERIC_READ, FILE_SHARE_READ |
			FILE_SHARE_WRITE, NULL, OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
		if (handle == INVALID_HANDLE_VALUE)
		{
			throw WinAPIException(GetLastError());
		}
		else
		{
			//prepare to call DeviceIoControl
			DWORD dummy;
			/*
				WARNING!!!
				There may be some problems with the disk-device extents
				because someone can organize a RAID massive on flash 
				drives - the extents may not be filled
			*/
			if (!DeviceIoControl(handle, IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
				NULL, 0, extents, sizeof(VOLUME_DISK_EXTENTS), &dummy, 0))
			{
				CloseHandle(handle);
				throw WinAPIException(GetLastError());
			}
			else
			{
				//return the result
				result = extents->Extents[0].DiskNumber;
				DELARRAY(extents);
				DELARRAY(volName);
#ifdef DEBUG
				_ASSERT(extents == NULL);
				_ASSERT(volName == NULL);
#endif
				CloseHandle(handle);
			}
		}
	}
	catch(...)
	{
		DELARRAY(extents);
		DELARRAY(volName);
#ifdef DEBUG
		_ASSERT(extents == NULL);
		_ASSERT(volName == NULL);
#endif
		throw;
	}
	return result;
}

/*
	Purpose:
		Gets the volume mount points
	Parameters:
		path - Path to the volume
	Return value:
		A C-string vector containing all mount points for the volume

	Verified 18.06.2010 by Asha'man
	Tested 18.06.2010 - Works correctly
*/
StringList* GetVolumeMountPoints(LPTSTR path)
{
	StringList *result = new StringList();
	LPTSTR mountPoints = new TCHAR[MAX_PATH+1];
	ZeroMemory(mountPoints, sizeof(TCHAR)*(MAX_PATH+1));
	DWORD numPoints;
	try
	{
		if (!GetVolumePathNamesForVolumeName(path, mountPoints, MAX_PATH, &numPoints))
		{
			throw WinAPIException(GetLastError());
		}
		else
		{
			LPTSTR pmp = mountPoints;
			while (mountPoints[0] != EOLN)
			{
				LPTSTR buffer = new TCHAR[_tcslen(mountPoints)+1];
				_tcscpy(buffer, mountPoints);
				result->push_back(buffer);
				mountPoints += _tcslen(mountPoints)+1;
			}
			DELARRAY(pmp);
#ifdef DEBUG
			_ASSERT(pmp == NULL);
#endif
		}
	}
	catch(...)
	{
		DELARRAY(mountPoints);
#ifdef DEBUG
		_ASSERT(mountPoints == NULL);
#endif
		throw;
	}
	return result;
}

/*
	Purpose:
		Gets the specified system information table
	Parameters:
		tableType - type of the table to get
	Return value:
		A pointer to the system information table
*/
PVOID GetSystemInformationTable(DWORD tableType)
{
	//result buffer
	PVOID result = NULL;
	//initial buffer size is 32
	DWORD bufSize = 32;
	//function status
	NTSTATUS status;
	do
	{
		//allocating the buffer
		result = (PVOID)new BYTE[bufSize];
		ZeroMemory(result, bufSize);
		//trying to call the function
		status = NtQuerySystemInformation((SYSTEMINFOCLASS)tableType, result, bufSize, NULL);
		//if buffer is too small, we realloc it
		if (status == STATUS_INFO_LENGTH_MISMATCH)
		{
			DELARRAY(result);
			bufSize *= 2;
		}
	}
	while(status == STATUS_INFO_LENGTH_MISMATCH);
	if (!NT_SUCCESS(status))
	{
		DELARRAY(result);
		throw WinAPIException(status);
	}
	return result;
}

/*
	Purpose:
		Gets file handle type using the NUL device
		It solves the first problem - getting file type on different systems
	Parameters:
		None
	Return value:
		A DWORD value repsesenting file type
*/
DWORD GetFileHandleType()
{
	HANDLE hFile;
	PSYSTEM_HANDLE_INFORMATION systemHandleInfo = NULL;
	DWORD result = 0;
	try
	{
		//opening "NUL" device in order to obtain its handle
		hFile = CreateFile(_T("NUL"), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, ZERO_FLAGS, ZERO_HANDLE);
		if (hFile == INVALID_HANDLE_VALUE)
		{
			throw WinAPIException(GetLastError());
		}
		//getting system handle information table
		systemHandleInfo = (PSYSTEM_HANDLE_INFORMATION)
			GetSystemInformationTable(SystemHandleInformation);
		//if the table is ready
		if (systemHandleInfo != NULL)
		{
			//iteration over table items
			for (size_t i = 0; i<systemHandleInfo->uCount; ++i)
			{
				//if the handle is for "NUL" device and if it belongs 
				//to the application, we get the object file type
				if (systemHandleInfo->aSH[i].Handle == (USHORT)hFile &&
					systemHandleInfo->aSH[i].uIdProcess == GetCurrentProcessId())
				{
					result = systemHandleInfo->aSH[i].ObjectType;
					break;
				}
			}
		}
	}
	catch(...)
	{
		CloseHandle(hFile);
		DELARRAY(systemHandleInfo);
		throw;
	}
	CloseHandle(hFile);
	DELARRAY(systemHandleInfo);
	return result;
}

//Thread parameters for getting the file name
typedef struct _FN_INFO
{
	FILE_NAME_INFORMATION fileNameInfo;
	TCHAR Name[2*MAX_PATH+1];
} FN_INFO, *PFN_INFO;

typedef struct _ON_INFO
{
	OBJECT_NAME_INFORMATION objectNameInfo;
	TCHAR Device[2*MAX_PATH+1];
} ON_INFO, *PON_INFO;

typedef struct _FILE_INFO
{
	HANDLE  hFile;
	FN_INFO fnInfo;
	ON_INFO onInfo;
} FILE_INFO, *PFILE_INFO;

/*
	Purpose:
		Gets file name in a separate thread
	Parameters:
		lpParameters - thread parameters
	Return value:
		A DWORD value repsesenting thread status
*/
DWORD WINAPI GetFileNameThread(LPVOID lpParameters)
{
	//converting data
	PFILE_INFO info = (PFILE_INFO)lpParameters;
	//input-output status
	IO_STATUS_BLOCK ioStatus;
	ZeroMemory(&ioStatus, sizeof(ioStatus));
	//name info parameters
	ZeroMemory(&info->fnInfo, sizeof(FILE_NAME_INFO));
	//calling the function in order to identify if the file is "hanging"
	if (NtQueryInformationFile(info->hFile, &ioStatus, &info->fnInfo.fileNameInfo,
		sizeof(FILE_INFO)-sizeof(HANDLE), FileNameInformation) == STATUS_SUCCESS)
	{
		DWORD retLength;
		//getting object name info - full path
		NtQueryObject(info->hFile, ObjectNameInformation, &info->onInfo.objectNameInfo,
			sizeof(FILE_INFO)-sizeof(HANDLE)-sizeof(FN_INFO), &retLength);
	}
	return NO_ERROR;
}

/*
	Purpose:
		Gets file name by its handle
	Parameters:
		hFile - file handle
	Return value:
		A C-string with file name or NULL if the call was timeouted
*/
LPTSTR GetFileName(HANDLE hFile)
{
	//result buffer
	PWCHAR result = NULL;
	//allocating the buffer
	PFILE_INFO info = (PFILE_INFO)new BYTE[sizeof(FILE_INFO)];
	ZeroMemory(info, sizeof(FILE_INFO));
	//saving file handle in parameters
	info->hFile = hFile;
	//creating a new thread for getting the file name
	HANDLE hThread = CreateThread(NULL, ZERO_BUFFER, &GetFileNameThread,
		info, ZERO_FLAGS, NULL);
	//waiting for thread to finish
	if (WaitForSingleObject(hThread, WAIT_PIPE_TIMEOUT) == WAIT_TIMEOUT)
	{
		//if the thread timeouted, we kill it
		TerminateThread(hThread, 0);
		//closing thread handle
		CloseHandle(hThread);
	}
	else
	{
		//closing thread handle
		CloseHandle(hThread);
		//if the buffer was filled
		if (info->onInfo.Device[0] != EOLN)
		{
			//copying file name
			result = new WCHAR[_tcslen(info->onInfo.Device)+1];
			_tcscpy(result,info->onInfo.Device);
		}
	}
	DELARRAY(info);
	return result;
}

/*
	Purpose:
		Gets the process name from the process info table
	Parameters:
		processInfo - pointer to the process info table
		processId - process ID to find
	Return value:
		A C-string with the process name
*/
LPTSTR GetProcessName (PSYSTEM_PROCESS_INFORMATION processInfo, DWORD processId)
{
	//copying pointer
	PSYSTEM_PROCESS_INFORMATION pProcessInfo = processInfo;
	//result buffer
	LPTSTR result = NULL;
	//loop until offset is zero or result is found
	do
	{
		//going to the next element
		pProcessInfo = (PSYSTEM_PROCESS_INFORMATION)((PCHAR)pProcessInfo + pProcessInfo->NextEntryOffset);
		if (pProcessInfo->ProcessId == (HANDLE)processId)
		{
			//if the process ID's are matching,
			//we copy the result
			result = new TCHAR[pProcessInfo->ImageName.Length+1];
			_tcscpy(result, pProcessInfo->ImageName.Buffer);
			break;
		}
	}
	while(pProcessInfo->NextEntryOffset != 0);
	return result;
}