# Introduction #

Current version of this program has some limitations due to its design.


# Details #

<p>During the re-reading of Uwe Sieber's perfect <a href='http://www.codeproject.com/KB/system/RemoveDriveByLetter.aspx'>article</a>, I have found a<br>
very important sentence which I unfortunately skipped last time. It says that on operation systems like Vista and later some SetupAPI's functions doesn't work in the same way as on XP. For example, <i>CM_Get_Parent</i> called for a volume will no more return the disk associated with it - it will give us link to the volume manager. The solution is to link a volume and a<br>
disk with the <i>DeviceNumber</i>. It is made in my program, but I've a bug somewhere else in my <i>DeviceManager</i> class because it fails under admin profile on Vista.<br>
</p>