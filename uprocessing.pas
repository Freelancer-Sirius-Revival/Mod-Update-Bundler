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
  IgnoredPathsFileName = 'ignoredPaths.txt';
  BundleFileName = 'bundle.flsr';
  BundleFileChecksumExtension = '.md5';

function TProcessResult.GetProcessingDone: Boolean;
begin
  Result := Boolean(FValue);
end;

procedure TProcessResult.SetProcessingDone(const NewProcessingDone: Boolean);
begin
  InterlockedExchange64(FValue, Int64(NewProcessingDone));
end;

procedure WriteMetaData(const FilesChunks: TFilesChunks; const BasePath: String; const Stream: TStream);
var
  FilesChunk: TFileInfoArray;
  ChunkFile: TFileInfo;
begin
  // Magic number of file format.
  Stream.Write('FLSR', SizeOf(Char) * 4);
  // Version of file format.
  Stream.WriteByte(1);

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
  Checksum: TStrings;
begin
  if DirectoryExists(FBasePath) then
  begin
    if FileExists(IgnoredPathsFileName) then
    begin
      ExcludedPaths := TStringList.Create;
      try
        ExcludedPaths.LoadFromFile(IgnoredPathsFileName);
      finally
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
      WriteMetaData(FilesChunks, FBasePath, Bundle);
      EncodeFilesChunks(FilesChunks, Bundle);
    finally
      Bundle.Free;
    end;

    for Index := 0 to High(FilesChunks) do
      SetLength(FilesChunks[Index], 0);
    SetLength(FilesChunks, 0);

    FileList.Free;

    if FileExists(BundleFileName) then
    begin
      try
        Checksum := TStringList.Create;
        Checksum.Append(MD5Print(MD5File(BundleFileName)));
        Checksum.SaveToFile(BundleFileName + BundleFileChecksumExtension);
      finally
        Checksum.Free;
      end;
    end;
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
