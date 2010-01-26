; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{CCFB3E4F-C655-416E-B172-C637C9D6480F}
AppName=Lambda Interpreter
AppVerName=Lambda Interpreter 3.6
AppPublisher=Victor Nazarov
AppPublisherURL=asviraspossible@gmail.com
AppSupportURL=asviraspossible@gmail.com
AppUpdatesURL=asviraspossible@gmail.com
DefaultDirName={pf}\Lambda Interpreter
DefaultGroupName=Lambda Interpreter
AllowNoIcons=yes
LicenseFile=C:\no-uninstall\lambda-interpreter-3-latest-bin\doc\LambdaInterpreter-3.6\COPYING.txt
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes

[Languages]
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "C:\no-uninstall\lambda-interpreter-3-latest-bin\bin\lambda-interpreter.exe"; DestDir: "{app}\bin"; Flags: ignoreversion
Source: "C:\no-uninstall\lambda-interpreter-3-latest-bin\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Lambda Interpreter"; Filename: "{app}\bin\lambda-interpreter.exe"
Name: "{commondesktop}\Lambda Interpreter"; Filename: "{app}\bin\lambda-interpreter.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\bin\Lambda Interpreter"; Filename: "{app}\bin\lambda-interpreter.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\bin\lambda-interpreter.exe"; Description: "{cm:LaunchProgram,Lambda Interpreter}"; Flags: nowait postinstall skipifsilent

