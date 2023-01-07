program bundler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'FL:SR Update Bundler';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

