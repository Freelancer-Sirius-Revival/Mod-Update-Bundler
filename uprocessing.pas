unit UProcessing;

{$mode ObjFPC}{$H+}

interface

type
  TProcessResult = class
  private
    FValue: Int64;
    function GetProcessingDone: Boolean;
    procedure SetProcessingDone(const NewProcessingDone: Boolean);
  public
    property ProcessingDone: Boolean read GetProcessingDone write SetProcessingDone;
  end;

function ProcessBundling(const BasePath: String): TProcessResult;

implementation

uses
  Classes,
  SysUtils,
  UFiles,
  UBundling;

const
  IgnoredPathsFileName = 'ignoredPaths.txt';
  BundleFileName = 'bundle.flsr';

function TProcessResult.GetProcessingDone: Boolean;
begin
  Result := Boolean(FValue);
end;

procedure TProcessResult.SetProcessingDone(const NewProcessingDone: Boolean);
begin
  InterlockedExchange64(FValue, Int64(NewProcessingDone));
end;

type
  TProcessThread = class(TThread)
  private
    FBasePath: String;
    FProcessResult: TProcessResult;
  protected
    procedure Execute; override;
  public
    constructor Create(const BasePath: String; const ProcessResult: TProcessResult);
  end;

constructor TProcessThread.Create(const BasePath: String; const ProcessResult: TProcessResult);
begin
  inherited Create(False);
  FBasePath := BasePath;
  FProcessResult := ProcessResult;
end;

procedure TProcessThread.Execute;
var
  FileList: TStrings;
  ExcludedPaths: TStrings = nil;
  FilesChunks: TFilesChunks;
  Bundle: TStream;
  FileMode: Int32;
  Index: ValSInt;
begin
  if DirectoryExists(FBasePath) then
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

    FileList := UFiles.FindRelevantFiles(FBasePath, ExcludedPaths);

    if Assigned(ExcludedPaths) then
      ExcludedPaths.Free;

    FilesChunks := UFiles.ComputeChunkedFiles(FileList, ['.ini']);
    if FileExists(BundleFileName) then
      FileMode := fmOpenWrite
    else
      FileMode := fmCreate;
    try
      Bundle := TFileStream.Create(BundleFileName, FileMode);
      UBundling.BundleFiles(FilesChunks, FBasePath, Bundle);
    finally
      Bundle.Free;
    end;

    for Index := 0 to High(FilesChunks) do
      SetLength(FilesChunks[Index], 0);
    SetLength(FilesChunks, 0);

    FileList.Free;
  end;

  FProcessResult.SetProcessingDone(True);
end;

function ProcessBundling(const BasePath: String): TProcessResult;
var
  ProcessThread: TProcessThread;
begin
  Result := TProcessResult.Create;
  Result.SetProcessingDone(False);
  ProcessThread := TProcessThread.Create(BasePath, Result);
  ProcessThread.FreeOnTerminate := True;
end;

end.

