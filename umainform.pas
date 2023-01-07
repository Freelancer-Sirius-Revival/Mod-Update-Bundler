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
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  UFiles;

procedure TMainForm.SelectInputPathButtonClick(Sender: TObject);
begin
  if SelectInputPathDialog.Execute then
    SelectInputPathEdit.Text := SelectInputPathDialog.FileName;
end;

procedure TMainForm.BeginBundlingButtonClick(Sender: TObject);
var
  FileList: TStrings;
  Path: String;
begin
  if DirectoryExists(SelectInputPathEdit.Text) then
  begin
    FileList := UFiles.FindRelevantFiles(SelectInputPathEdit.Text, nil);
    for Path in FileList do
      WriteLn(Path);
    FileList.Free;
  end;
end;

end.
