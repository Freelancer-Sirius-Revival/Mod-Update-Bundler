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
  Forms, UMainForm, ULZMACommon, ULZMAEncoder, ULZMABase, UBitTreeEncoder, URangeEncoder, URangeDecoder, ULZBinTree, ULZInWindow, UEncoding,
  UBundling;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'FL:SR Update Bundler';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

