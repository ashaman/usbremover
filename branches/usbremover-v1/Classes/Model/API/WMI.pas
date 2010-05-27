{
  This unit contains SetupAPI function prototypes
}
unit WMI;

interface
uses
  Windows;

{Aliases for standard system DLL's}
const
  setupapi = 'SetupApi.dll';
  cfgmgr = 'cfgmgr32.dll';
  SetupApiModuleName = 'SETUPAPI.DLL';
  GUID_DEVINTERFACE_VOLUME: TGUID = '{53F5630D-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVCLASS_DISKDRIVE: TGUID = '{4D36E967-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_USB: TGUID = '{36FC9E60-C465-11CF-8056-444553540000}';
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_DEVINTERFACE_DISK: TGUID = '{53F56307-B6BF-11D0-94F2-00A0C91EFB8B}';

{
  STRUCTURES, CONSTANTS AND FUNCTIONS FROM SETUPAPI.H
  Partially translated by Alexander Bagel
  Copyright : © Fangorn Wizards Lab 1998 - 2009.
  Constants partially translated by J.L.Blackrow
  Some functions translated by rpy3uH (aka ahilles)
}

const
  ANYSIZE_ARRAY = 1024;

  {
    THESE CONSTANTS TRANSLATED BY J.L.BLACKROW
  }
  DIGCF_DEFAULT = $00000001;  // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_PROFILE = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

  SPDRP_DEVICEDESC = $00000000;  // DeviceDesc (R/W)
  SPDRP_HARDWAREID = $00000001;  // HardwareID (R/W)
  SPDRP_COMPATIBLEIDS = $00000002;  // CompatibleIDs (R/W)
  SPDRP_UNUSED0 = $00000003;  // unused
  SPDRP_SERVICE = $00000004;  // Service (R/W)
  SPDRP_UNUSED1 = $00000005;  // unused
  SPDRP_UNUSED2 = $00000006;  // unused
  SPDRP_CLASS = $00000007;  // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID = $00000008;  // ClassGUID (R/W)
  SPDRP_DRIVER = $00000009;  // Driver (R/W)
  SPDRP_CONFIGFLAGS = $0000000A;  // ConfigFlags (R/W)
  SPDRP_MFG = $0000000B;  // Mfg (R/W)
  SPDRP_FRIENDLYNAME = $0000000C;  // FriendlyName (R/W)
  SPDRP_LOCATION_INFORMATION = $0000000D;  // LocationInformation (R/W)
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E;  // PhysicalDeviceObjectName (R)
  SPDRP_CAPABILITIES = $0000000F;  // Capabilities (R)
  SPDRP_UI_NUMBER = $00000010;  // UiNumber (R)
  SPDRP_UPPERFILTERS = $00000011;  // UpperFilters (R/W)
  SPDRP_LOWERFILTERS = $00000012;  // LowerFilters (R/W)
  SPDRP_BUSTYPEGUID = $00000013;  // BusTypeGUID (R)
  SPDRP_LEGACYBUSTYPE = $00000014;  // LegacyBusType (R)
  SPDRP_BUSNUMBER = $00000015;  // BusNumber (R)
  SPDRP_ENUMERATOR_NAME = $00000016;  // Enumerator Name (R)
  SPDRP_SECURITY = $00000017;  // Security (R/W, binary form)
  SPDRP_SECURITY_SDS = $00000018;  // Security (W, SDS form)
  SPDRP_DEVTYPE = $00000019;  // Device Type (R/W)
  SPDRP_EXCLUSIVE = $0000001A;  // Device is exclusive-access (R/W)
  SPDRP_CHARACTERISTICS = $0000001B;  // Device Characteristics (R/W)
  SPDRP_ADDRESS = $0000001C;  // Device Address (R)
  SPDRP_UI_NUMBER_DESC_FORMAT = $0000001D;  // UiNumberDescFormat (R/W)
  SPDRP_DEVICE_POWER_DATA = $0000001E;  // Device Power Data (R)
  SPDRP_REMOVAL_POLICY = $0000001F;  // Removal Policy (R)
  SPDRP_REMOVAL_POLICY_HW_DEFAULT = $00000020;  // Hardware Removal Policy (R)
  SPDRP_REMOVAL_POLICY_OVERRIDE = $00000021;  // Removal Policy Override (RW)
  SPDRP_INSTALL_STATE = $00000022;  // Device Install State (R)
  SPDRP_LOCATION_PATHS = $00000023;  // Device Location Paths (R)
  SPDRP_MAXIMUM_PROPERTY = $00000024;  // Upper bound on ordinals


type
  HDEVINFO = THandle;

  PSPDevInfoData = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: PULONG;
  end;

  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: PULONG;
  end;

  TSPDeviceInterfaceData = SP_DEVICE_INTERFACE_DATA;

  PSPDeviceInterfaceDetailDataA = ^TSPDeviceInterfaceDetailDataA;
  PSPDeviceInterfaceDetailData = PSPDeviceInterfaceDetailDataA;
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    DevicePath: array [0..ANYSIZE_ARRAY - 1] of AnsiChar;
  end;
  TSPDeviceInterfaceDetailDataA = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  TSPDeviceInterfaceDetailData = TSPDeviceInterfaceDetailDataA;

  function CMP_WaitNoPendingInstallEvents(Timeout: Cardinal): Cardinal; stdcall;
    external cfgmgr;

  function SetupDiGetClassDevsA(ClassGuid: PGUID; const Enumerator: PAnsiChar;
    hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall; external setupapi;

  function SetupDiDestroyDeviceInfoList(
    DeviceInfoSet: HDEVINFO): LongBool; stdcall; external setupapi;

  function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
    MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData):
    LongBool; stdcall; external setupapi;

  function SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSPDeviceInterfaceData;
    DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
    DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
    Device: PSPDevInfoData): LongBool; stdcall; external setupapi;

  function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD;
    var DeviceInfoData: TSPDevInfoData): BOOL; stdcall; external SetupApiModuleName
    name 'SetupDiEnumDeviceInfo';

  function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    var RequiredSize: DWORD): BOOL; stdcall; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyA';

  function SetupDiGetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    var RequiredSize: DWORD): BOOL; stdcall; external SetupApiModuleName name 'SetupDiGetDeviceRegistryPropertyA';

  function SetupDiOpenDeviceInfoA(DeviceInfoSet: HDEVINFO;
    const DeviceInstanceId: PAnsiChar; hwndParent: HWND; OpenFlags: DWORD;
    DeviceInfoData: PSPDevInfoData): LongBool; stdcall; external SetupApiModuleName name 'SetupDiOpenDeviceInfoA'

{
  CONSTANTS AND TYPES FROM CFGMGR32.H
  Partially translated by Alexander Bagel
  Copyright : © Fangorn Wizards Lab 1998 - 2009.
}

const
  CR_SUCCESS = 0;

  PNP_VetoTypeUnknown          = 0;
  PNP_VetoLegacyDevice         = 1;
  PNP_VetoPendingClose         = 2;
  PNP_VetoWindowsApp           = 3;
  PNP_VetoWindowsService       = 4;
  PNP_VetoOutstandingOpen      = 5;
  PNP_VetoDevice               = 6;
  PNP_VetoDriver               = 7;
  PNP_VetoIllegalDeviceRequest = 8;
  PNP_VetoInsufficientPower    = 9;
  PNP_VetoNonDisableable       = 10;
  PNP_VetoLegacyDriver         = 11;
  PNP_VetoInsufficientRights   = 12;

type
  DEVINST = DWORD;
  CONFIGRET = DWORD;

  PPNP_VETO_TYPE = ^PNP_VETO_TYPE;
  PNP_VETO_TYPE = DWORD;

  function CM_Get_Parent(var dnDevInstParent: DEVINST;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
    external cfgmgr;

  function CM_Request_Device_EjectA(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE; pszVetoName: PWideChar;
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
    external setupapi;

  function CM_Get_Device_IDA(dnDevInst: DEVINST; Buffer: PAnsiChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall;
    external CfgMgr name 'CM_Get_Device_IDA';

implementation

end.


