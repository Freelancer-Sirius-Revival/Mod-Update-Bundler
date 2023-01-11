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
  UEncoding,
  UBundle,
  md5;

const
  IgnoredPathsFileName = 'ignoredPaths.txt';

function TProcessResult.GetProcessingDone: Boolean;
begin
  Result := Boolean(FValue);
end;

procedure TProcessResult.SetProcessingDone(const NewProcessingDone: Boolean);
begin
  InterlockedExchange64(FValue, Int64(NewProcessingDone));
end;

function GetFilteredFilesList(const BasePath: String): TStrings;
var
  ExcludedPaths: TStrings = nil;
begin
  if DirectoryExists(BasePath) then
  begin
    if FileExists(IgnoredPathsFileName) then
    begin
      try
        ExcludedPaths := TStringList.Create;
        ExcludedPaths.LoadFromFile(IgnoredPathsFileName);
      finally
      end;
    end;
    Result := FindRelevantFiles(BasePath, ExcludedPaths);
    if Assigned(ExcludedPaths) then
      ExcludedPaths.Free;
  end
  else
    Result := nil;
end;

procedure CreateChecksumFileForFile(const FileName: String);
var
  Checksum: TStrings;
begin
  if FileExists(FileName) then
  begin
    try
      Checksum := TStringList.Create;
      Checksum.Append(MD5Print(MD5File(FileName)));
      Checksum.SaveToFile(FileName + ChecksumFileExtension);
    finally
      Checksum.Free;
    end;
  end;
end;

procedure CreateBundle(const ContentVersion: Uint32; const BundleType: TBundleType; const FilesChunks: TFilesChunks; const BasePath: String; const FileName: String);
var
  Bundle: TStream;
begin
  if Assigned(FilesChunks) and (FileName.Length > 0) then
  begin
    try
      Bundle := TFileStream.Create(FileName, fmCreate);
      WriteMetaData(ContentVersion, BundleType, FilesChunks, BasePath, Bundle);
      EncodeFilesChunks(FilesChunks, Bundle);
    finally
      Bundle.Free;
    end;
  end;
end;

procedure Process(const BasePath: String);
var
  FileList: TStrings;
  FilesChunks: TFilesChunks;
begin
  FileList := GetFilteredFilesList(BasePath);
  FilesChunks := ComputeChunkedFiles(FileList, ['.ini']);
  FileList.Free;

  CreateBundle(0, TFullBundle, FilesChunks, BasePath, FullBundleFileName + BundleFileExtension);

  CreateChecksumFileForFile(FullBundleFileName + BundleFileExtension);
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
begin
  Process(FBasePath);
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
