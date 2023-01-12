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
    SelectOutputPathDialog: TSelectDirectoryDialog;
    SelectOutputPathEdit: TEdit;
    SelectOutputPathButton: TButton;
    SelectInputPathEdit: TEdit;
    SelectInputPathDialog: TSelectDirectoryDialog;
    ProcessTimer: TTimer;
    procedure BeginBundlingButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ProcessTimerTimer(Sender: TObject);
    procedure SelectInputPathButtonClick(Sender: TObject);
    procedure SelectInputPathEditChange(Sender: TObject);
    procedure SelectOutputPathButtonClick(Sender: TObject);
    procedure SelectOutputPathEditChange(Sender: TObject);
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

procedure TMainForm.DetermineBundlingButtonEnabled;
var
  InputPath: String;       
  OutputPath: String;
begin
  InputPath := SelectInputPathEdit.Text;         
  InputPath := InputPath.Trim;
  OutputPath := SelectOutputPathEdit.Text;
  OutputPath := OutputPath.Trim;
  BeginBundlingButton.Enabled := (not InputPath.IsEmpty) and DirectoryExists(InputPath) and
                                 (not OutputPath.IsEmpty) and DirectoryExists(OutputPath) and
                                 (not Assigned(CurrentProcessResult) or CurrentProcessResult.ProcessingDone);
end;

procedure TMainForm.SelectInputPathButtonClick(Sender: TObject);
begin
  if SelectInputPathDialog.Execute then
  begin
    SelectInputPathEdit.Text := SelectInputPathDialog.FileName;
    DetermineBundlingButtonEnabled;
  end;
end;

procedure TMainForm.SelectInputPathEditChange(Sender: TObject);
begin
  DetermineBundlingButtonEnabled;
end;

procedure TMainForm.SelectOutputPathButtonClick(Sender: TObject);
begin
  if SelectOutputPathDialog.Execute then
  begin
    SelectOutputPathEdit.Text := SelectOutputPathDialog.FileName;
    DetermineBundlingButtonEnabled;
  end;
end;

procedure TMainForm.SelectOutputPathEditChange(Sender: TObject);
begin
  DetermineBundlingButtonEnabled;
end;

procedure TMainForm.BeginBundlingButtonClick(Sender: TObject);
begin
  CurrentProcessResult := ProcessBundling(SelectInputPathEdit.Text, SelectOutputPathEdit.Text);
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
