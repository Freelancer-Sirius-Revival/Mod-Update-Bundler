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
  UFiles,
  UBundling;

const
  IgnoredPathsFileName = 'ignoredPaths.txt';
  BundleFileName = 'bundle.flsr';

procedure ProcessBundling(const BasePath: String);
var
  FileList: TStrings;
  ExcludedPaths: TStrings = nil;
  FilesChunks: TFilesChunks;
  Bundle: TStream;
  FileMode: Int32;
begin
  if DirectoryExists(BasePath) then
  begin
    if FileExists(IgnoredPathsFileName) then
    begin
      try
        ExcludedPaths := TStringList.Create;
        ExcludedPaths.LoadFromFile(IgnoredPathsFileName);
      finally
        FreeAndNil(ExcludedPaths);
      end;
    end;

    FileList := UFiles.FindRelevantFiles(BasePath, ExcludedPaths);

    if Assigned(ExcludedPaths) then
      ExcludedPaths.Free;

    FilesChunks := UFiles.ComputeChunkedFiles(FileList, ['.ini']);
    if FileExists(BundleFileName) then
      FileMode := fmOpenWrite
    else
      FileMode := fmCreate;
    try
      Bundle := TFileStream.Create(BundleFileName, FileMode);
      UBundling.BundleFiles(FilesChunks, BasePath, Bundle);
    finally
      Bundle.Free;
    end;
    SetLength(FilesChunks, 0);

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
