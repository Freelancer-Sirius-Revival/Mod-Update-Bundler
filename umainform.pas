unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  UProcessing;

type
  TMainForm = class(TForm)
    BeginBundlingButton: TButton;
    ProcessRunningBar: TProgressBar;
    SelectInputPathButton: TButton;
    SelectInputPathEdit: TEdit;
    SelectInputPathDialog: TSelectDirectoryDialog;
    ProcessTimer: TTimer;
    procedure BeginBundlingButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ProcessTimerTimer(Sender: TObject);
    procedure SelectInputPathButtonClick(Sender: TObject);
    procedure SelectInputPathEditChange(Sender: TObject);
  private
  var
    CurrentProcessResult: TProcessResult;
    procedure DetermineBundlingButtonEnabled;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CurrentProcessResult := nil;
end;

procedure TMainForm.SelectInputPathButtonClick(Sender: TObject);
begin
  if SelectInputPathDialog.Execute then
  begin
    SelectInputPathEdit.Text := SelectInputPathDialog.FileName;
    BeginBundlingButton.Enabled := True;
  end;
end;

procedure TMainForm.SelectInputPathEditChange(Sender: TObject);
begin
  DetermineBundlingButtonEnabled;
end;

procedure TMainForm.DetermineBundlingButtonEnabled;
var
  Path: String;
begin
  Path := SelectInputPathEdit.Text;
  BeginBundlingButton.Enabled := (not Path.Trim.IsEmpty) and (not Assigned(CurrentProcessResult) or CurrentProcessResult.ProcessingDone);
end;

procedure TMainForm.BeginBundlingButtonClick(Sender: TObject);
begin
  CurrentProcessResult := ProcessBundling(SelectInputPathEdit.Text);
  ProcessRunningBar.Enabled := True;
  ProcessTimer.Enabled := True;
  DetermineBundlingButtonEnabled;
end;

procedure TMainForm.ProcessTimerTimer(Sender: TObject);
begin
  if Assigned(CurrentProcessResult) and CurrentProcessResult.ProcessingDone then
  begin
    ProcessTimer.Enabled := False;
    ProcessRunningBar.Enabled := False;
    FreeAndNil(CurrentProcessResult);
    DetermineBundlingButtonEnabled;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Assigned(CurrentProcessResult) or CurrentProcessResult.ProcessingDone;
end;

end.
