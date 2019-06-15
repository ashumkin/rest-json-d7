program rest_json_test;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF CONSOLE_TESTRUNNER}

uses
  SysUtils,
  TestFramework,
  TestFrameworkRunner, {dUnitEx}
  TestAPI in 'TestAPI.pas';

begin
  ExitCode := RunRegisteredTestsGUIOrConsole(IsConsole);
end.

