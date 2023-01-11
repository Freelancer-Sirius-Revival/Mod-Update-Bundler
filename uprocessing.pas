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
  md5;

const
  FlsrFileVersion = 1;
  FlsrModVersion = 0;
  FlsrFileMagicNumbers = 'FLSR';
  IgnoredPathsFileName = 'ignoredPaths.txt';
  BundleFileName = 'bundle.flsr';
  ChecksumFileExtension = '.md5';

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

procedure WriteMetaData(const FilesChunks: TFilesChunks; const BasePath: String; const Stream: TStream);
var
  FilesChunk: TFileInfoArray;
  ChunkFile: TFileInfo;
begin
  // Magic number of file format.
  Stream.Write(FlsrFileMagicNumbers, SizeOf(FlsrFileMagicNumbers));
  // Version of file format.
  Stream.WriteByte(FlsrFileVersion);

  // Version of file contents.
  Stream.WriteQWord(FlsrModVersion);

  // Count of Chunks.
  Stream.WriteQWord(Length(FilesChunks));

  for FilesChunk in FilesChunks do
  begin
    // Count of files in this chunk.
    Stream.WriteQWord(Length(FilesChunk));

    for ChunkFile in FilesChunk do
    begin
      // First write a normalized relative path of the file.
      Stream.WriteAnsiString(ChunkFile.Path.Remove(0, BasePath.Length).Trim.Replace('\', '/'));
      // Second write the uncompressed size of the file.
      Stream.WriteQWord(ChunkFile.Size);
      // Third write an MD5 hash of the file's contents.
      Stream.Write(MD5File(ChunkFile.Path), SizeOf(TMD5Digest));
    end;
  end;
end;

procedure CreateBundle(const FilesChunks: TFilesChunks; const BasePath: String; const FileName: String);
var
  Bundle: TStream;
begin
  if Assigned(FilesChunks) and (FileName.Length > 0) then
  begin
    try
      Bundle := TFileStream.Create(FileName, fmCreate);
      WriteMetaData(FilesChunks, BasePath, Bundle);
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

  CreateBundle(FilesChunks, BasePath, BundleFileName);

  CreateChecksumFileForFile(BundleFileName);
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
