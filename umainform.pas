unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    BeginBundlingButton: TButton;
    SelectInputPathButton: TButton;
    SelectInputPathEdit: TEdit;
    SelectInputPathDialog: TSelectDirectoryDialog;
    procedure BeginBundlingButtonClick(Sender: TObject);
    procedure SelectInputPathButtonClick(Sender: TObject);
    procedure SelectInputPathEditChange(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  UFiles;

const
  IgnoredPathsFileName = 'ignoredPaths.txt';

procedure ProcessBundling(const BasePath: String);
var
  FileList: TStrings;
  Path: String;
  ExcludedPaths: TStrings = nil;
begin
  if DirectoryExists(BasePath) then
  begin
    if FileExists(IgnoredPathsFileName) then
    begin
      ExcludedPaths := TStringList.Create;
      ExcludedPaths.LoadFromFile(IgnoredPathsFileName);
    end;

    FileList := UFiles.FindRelevantFiles(BasePath, ExcludedPaths);

    if Assigned(ExcludedPaths) then
      ExcludedPaths.Free;

    for Path in FileList do
      WriteLn(Path);
    FileList.Free;
  end;
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
var
  Path: String;
begin
  Path := SelectInputPathEdit.Text;
  BeginBundlingButton.Enabled := not Path.Trim.IsEmpty;
end;

procedure TMainForm.BeginBundlingButtonClick(Sender: TObject);
begin
  ProcessBundling(SelectInputPathEdit.Text);
end;

end.
