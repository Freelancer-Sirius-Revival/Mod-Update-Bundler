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
  UInputFiles,
  UEncoding,
  UBundle,
  FileUtil,
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

type
  TBundle = record
    FileEntries: TFileEntries;
    ContentVersion: Uint32;
    BundleType: TBundleType
  end;

function GetBundleMetaData(const FileName: String): TBundle;
var
  Stream: TStream;
begin
  Result.BundleType := TUnknownBundle;
  if FileExists(FileName) then
  begin
    try
      Stream := TFileStream.Create(FileName, fmOpenRead);
      Stream.Position := 0;
      Result.ContentVersion := ReadContentVersion(Stream);
      Stream.Position := 0;
      Result.BundleType := ReadBundleType(Stream);
      Stream.Position := 0;
      Result.FileEntries := ReadFilesMetaData(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

function RemoveEqualFileEntriesFromFilesChunks(const InputFileList: TStrings; const ExistingFileEntries: TFileEntries; const BasePath: String): TStrings;
var
  InputFileName: String;
  OtherFileEntry: TFileEntry;
begin
  Result := TStringList.Create;
  for InputFileName in InputFileList do
    for OtherFileEntry in ExistingFileEntries do
      // First it must be the same path name. After that the size is being compared directly. And then the MD5 checksum.
      if (InputFileName.Remove(0, BasePath.Length).Trim.Replace('\', '/') = OtherFileEntry.Path) and
        ((GetFileSize(InputFileName) <> OtherFileEntry.Size) or (CompareByte(MD5File(InputFileName), OtherFileEntry.Checksum, SizeOf(TMD5Digest)) <> 0)) then
      begin
        Result.Append(InputFileName);
      end;
end;

procedure Process(const BasePath: String);
var
  CompleteFileList: TStrings;
  CompleteFilesChunks: TFilesChunks;
  PreviousFullBundleMetaData: TBundle;
  NextContentVersion: Uint32 = 0;
  UpdateFileList: TStrings = nil;
  UpdateFilesChunks: TFilesChunks;
  UpdateFileName: String;
begin
  CompleteFileList := GetFilteredFilesList(BasePath);
  CompleteFilesChunks := ComputeChunkedFiles(CompleteFileList, ['.ini']);

  if FileExists(FullBundleFileName + BundleFileExtension) then
  begin
    PreviousFullBundleMetaData := GetBundleMetaData(FullBundleFileName + BundleFileExtension);
    if PreviousFullBundleMetaData.BundleType = TFullBundle then
    begin
      UpdateFileList := RemoveEqualFileEntriesFromFilesChunks(CompleteFileList, PreviousFullBundleMetaData.FileEntries, BasePath);
      if UpdateFileList.Count <> 0 then
      begin                                                                       
        NextContentVersion := PreviousFullBundleMetaData.ContentVersion + 1;
        UpdateFilesChunks := ComputeChunkedFiles(UpdateFileList, ['.ini']);
        UpdateFileName := UpdateBundleFileName + '.' + IntToStr(NextContentVersion) + BundleFileExtension;
        CreateBundle(NextContentVersion, TUpdateBundle, UpdateFilesChunks, BasePath, UpdateFileName);
        CreateChecksumFileForFile(UpdateFileName);
      end;
    end;
  end;

  // There must be either no previous bundle file, or at least one change since it.
  if not Assigned(UpdateFileList) or (UpdateFileList.Count > 0) then
  begin
    CreateBundle(NextContentVersion, TFullBundle, CompleteFilesChunks, BasePath, FullBundleFileName + BundleFileExtension);
    CreateChecksumFileForFile(FullBundleFileName + BundleFileExtension);
  end;

  CompleteFileList.Free;
  if Assigned(UpdateFileList) then
    UpdateFileList.Free;
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
