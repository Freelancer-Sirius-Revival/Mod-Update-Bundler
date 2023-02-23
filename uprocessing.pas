unit UProcessing;

{$mode ObjFPC}{$H+}

interface

const
  ChecksumFileExtension = '.md5';

type
  TProcessProgress = class
  private
    FTotalBytes: Int64;
    FPercentage: Single;
    FDone: Int32;
    function GetDone: Boolean;
    procedure SetDone(const Done: Boolean);
    procedure SetPercentage(const NewPercentage: Single);
    procedure OnEncodingProgress(const BytesWritten: Int64);
  public
    constructor Create;
    property Percentage: Single read FPercentage write SetPercentage;
    property Done: Boolean read GetDone write SetDone;
  end;

function ProcessBundling(const InputPath: String; const OutputPath: String): TProcessProgress;

implementation

uses
  Classes,
  SysUtils,
  UInputFiles,
  UEncoding,
  UBundle,
  UMeta,
  FileUtil,
  md5;

const
  IgnoredPathsFileName = 'ignoredPaths.txt';
  ChunkedFileExtensions: array [0..7] of String = ('.ini', '.thn', '.wav', '.utf', '.bmp', '.tga', '.mat', '.txm');

function TProcessProgress.GetDone: Boolean;
begin
  Result := Boolean(FDone);
end;

procedure TProcessProgress.SetDone(const Done: Boolean);
begin
  InterlockedExchange(FDone, Int32(Done));
end;

procedure TProcessProgress.SetPercentage(const NewPercentage: Single);
begin
  InterlockedExchange(Int32(FPercentage), Int32(NewPercentage));
end;

procedure TProcessProgress.OnEncodingProgress(const BytesWritten: Int64);
begin
  SetPercentage(BytesWritten / FTotalBytes);
end;

constructor TProcessProgress.Create;
begin
  inherited Create;
  FTotalBytes := 0;
  FPercentage := 0;
  Done := False;
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

procedure CreateBundle(const ContentVersion: Uint32; const BundleType: TBundleType; const FilesChunks: TFilesChunks; const BasePath: String; const FileName: String; const OnEncodingProgress: TEncodingProgressCallback);
var
  Bundle: TStream;
begin
  if Assigned(FilesChunks) and (FileName.Length > 0) then
  begin
    try
      Bundle := TFileStream.Create(FileName, fmCreate);
      WriteMetaData(ContentVersion, BundleType, FilesChunks, BasePath, Bundle);
      EncodeFilesChunks(FilesChunks, Bundle, OnEncodingProgress);
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

function RemoveEqualFileEntriesFromFilesChunks(const InputFileList: TStrings; const ExistingFileEntries: TFileEntries; const InputBasePath: String): TStrings;
var
  InputFileName: String;
  OtherFileEntry: TFileEntry;
begin
  Result := TStringList.Create;
  for InputFileName in InputFileList do
    for OtherFileEntry in ExistingFileEntries do
      // First it must be the same path name. After that the size is being compared directly. And then the MD5 checksum.
      if (InputFileName.Remove(0, InputBasePath.Length).Trim.Replace('\', '/') = OtherFileEntry.Path) and
        ((GetFileSize(InputFileName) <> OtherFileEntry.Size) or (CompareByte(MD5File(InputFileName), OtherFileEntry.Checksum, SizeOf(TMD5Digest)) <> 0)) then
      begin
        Result.Append(InputFileName);
      end;
end;

function GetTotalSizeOfFiles(const FilesChunks: TFilesChunks): Int64;
var
  Files: TFileInfoArray;
  FileEntry: TFileInfo;
begin
  Result := 0;
  for Files in FilesChunks do
    for FileEntry in Files do
      Result := Result + FileEntry.Size;
end;

procedure Process(const InputPath: String; OutputPath: String; const ProcessProgress: TProcessProgress);
var
  CompleteBundlePath: String;
  CompleteFileList: TStrings;
  CompleteFilesChunks: TFilesChunks;
  PreviousFullBundleMetaData: TBundle;
  NextContentVersion: Uint32 = 0;
  UpdateFileList: TStrings = nil;
  UpdateFilesChunks: TFilesChunks = nil;
  UpdateFileName: String;
  TotalBytesToEncode: Int64 = 0;
begin
  OutputPath := OutputPath.Replace('\', '/');
  if not OutputPath.EndsWith('/') then
    OutputPath := OutputPath + '/';

  CompleteBundlePath := OutputPath + FullBundleFileName + BundleFileExtension;

  CompleteFileList := GetFilteredFilesList(InputPath);
  CompleteFilesChunks := ComputeChunkedFiles(CompleteFileList, ChunkedFileExtensions);

  // Gather any updates files, if anything was there to update.
  if FileExists(CompleteBundlePath) then
  begin
    PreviousFullBundleMetaData := GetBundleMetaData(CompleteBundlePath);
    if PreviousFullBundleMetaData.BundleType = TFullBundle then
    begin
      UpdateFileList := RemoveEqualFileEntriesFromFilesChunks(CompleteFileList, PreviousFullBundleMetaData.FileEntries, InputPath);
      if UpdateFileList.Count > 0 then
      begin
        NextContentVersion := PreviousFullBundleMetaData.ContentVersion + 1;
        UpdateFilesChunks := ComputeChunkedFiles(UpdateFileList, ChunkedFileExtensions);
      end;
    end;
  end;

  // Get the total amount of bytes to write.
  if Length(UpdateFilesChunks) > 0 then
    TotalBytesToEncode := TotalBytesToEncode + GetTotalSizeOfFiles(UpdateFilesChunks);
  if not Assigned(UpdateFileList) or (UpdateFileList.Count > 0) then
    TotalBytesToEncode := TotalBytesToEncode + GetTotalSizeOfFiles(CompleteFilesChunks);
  ProcessProgress.FTotalBytes := TotalBytesToEncode;

  // Write the update file if there is anything to update.
  if Length(UpdateFilesChunks) > 0 then
  begin
    UpdateFileName := UpdateBundleFileName + '.' + IntToStr(NextContentVersion) + BundleFileExtension;
    CreateBundle(NextContentVersion, TUpdateBundle, UpdateFilesChunks, InputPath, OutputPath + UpdateFileName, @ProcessProgress.OnEncodingProgress);
    CreateMetaFileForFile(UpdateFileName);
    CreateChecksumFileForFile(UpdateFileName);
  end;

  // Create the complete mod file if there was none before or at least one change happened since.
  if not Assigned(UpdateFileList) or (UpdateFileList.Count > 0) then
  begin
    CreateBundle(NextContentVersion, TFullBundle, CompleteFilesChunks, InputPath, CompleteBundlePath, @ProcessProgress.OnEncodingProgress);
    CreateMetaFileForFile(CompleteBundlePath);
    CreateChecksumFileForFile(CompleteBundlePath);
  end;

  CompleteFileList.Free;
  if Assigned(UpdateFileList) then
    UpdateFileList.Free;
end;

type
  TProcessThread = class(TThread)
  private
    FInputPath: String;
    FOutputPath: String;
    FProcessProgress: TProcessProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(const InputPath: String; const OutputPath: String; const ProcessResult: TProcessProgress);
  end;

constructor TProcessThread.Create(const InputPath: String; const OutputPath: String; const ProcessResult: TProcessProgress);
begin
  inherited Create(False);
  FInputPath := InputPath;
  FOutputPath := OutputPath;
  FProcessProgress := ProcessResult;
end;

procedure TProcessThread.Execute;
begin
  Process(FInputPath, FOutputPath, FProcessProgress);
  FProcessProgress.Done := True;
end;

function ProcessBundling(const InputPath: String; const OutputPath: String): TProcessProgress;
var
  ProcessThread: TProcessThread;
begin
  Result := TProcessProgress.Create;
  ProcessThread := TProcessThread.Create(InputPath, OutputPath, Result);
  ProcessThread.FreeOnTerminate := True;
end;

end.
