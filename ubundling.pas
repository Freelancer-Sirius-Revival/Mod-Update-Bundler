unit UBundling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UFiles;

procedure BundleFiles(const FilesChunks: TFilesChunks; const BasePath: String; const TargetStream: TStream);

implementation

uses
  SysUtils,
  Math,
  Contnrs,
  UEncoding;

procedure BundleFilesChunk(const FilesChunk: TFileInfoArray; const BasePath: String; const TargetStream: TStream);
var
  InputStream: TStream;
  OutputStream: TStream;
  FileInfo: TFileInfo;
  FileStream: TStream;
  UncompressedChunkSize: Int64 = 0;
begin
  // Read all file contents of this chunk.
  InputStream := TMemoryStream.Create;
  for FileInfo in FilesChunk do
  begin
    try
      FileStream := TFileStream.Create(FileInfo.Path, fmOpenRead, fmShareDenyWrite);
      InputStream.CopyFrom(FileStream, 0);
    finally
      FileStream.Free;
    end;
  end;

  // Encode all file contents.
  InputStream.Position := 0;
  OutputStream := TMemoryStream.Create;
  UEncoding.Encode(InputStream, OutputStream);
  InputStream.Free;

  // Write size of whole decompressed chunk.
  for FileInfo in FilesChunk do
    UncompressedChunkSize := UncompressedChunkSize + FileInfo.Size;
  TargetStream.WriteQWord(UncompressedChunkSize);

  // Write count of files in this chunk.
  TargetStream.WriteQWord(Length(FilesChunk));

  // Write individual file path and size information.
  for FileInfo in FilesChunk do
  begin
    TargetStream.WriteAnsiString(FileInfo.Path.Remove(0, BasePath.Length).Trim.Replace('\', '/'));
    TargetStream.WriteQWord(FileInfo.Size);
  end;

  // Copy encoded stream contents over.
  TargetStream.CopyFrom(OutputStream, 0);
  OutputStream.Free;
end;

type
  // This class contains all file chunks that will be processed by the bundler threads.
  TFilesChunksManager = class
  private
    FFilesBasePath: String;
    FFilesChunks: TFilesChunks;
    FFilesChunkIndex: ValSInt;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create(const FilesBasePath: String; FilesChunks: TFilesChunks);
    destructor Destroy; override;
    function GetNextFilesChunk: TFileInfoArray;
    property FilesBasePath: String read FFilesBasePath;
  end;

constructor TFilesChunksManager.Create(const FilesBasePath: String; FilesChunks: TFilesChunks);
begin
  inherited Create;
  FFilesBasePath := FilesBasePath;
  FFilesChunks := FilesChunks;
  FFilesChunkIndex := 0;
  InitCriticalSection(FCriticalSection);
end;

destructor TFilesChunksManager.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TFilesChunksManager.GetNextFilesChunk: TFileInfoArray;
begin
  EnterCriticalSection(FCriticalSection);
  if FFilesChunkIndex < Length(FFilesChunks) then
  begin
    Result := FFilesChunks[FFilesChunkIndex];
    Inc(FFilesChunkIndex);
  end
  else
    Result := nil;
  LeaveCriticalSection(FCriticalSection);
end;

type
  // The accumulation thread gathers each stream that was finished by the bundler threads and writes it into the destination stream.
  // Due the nature of multi threading, the order of streams written into the destination may vary each time.
  TAccumulationThread = class(TThread)
  private
    FDestinationStream: TStream;
    FCriticalSection: TRTLCriticalSection;
    FDataStack: TObjectStack;
  protected
    procedure Execute; override;
  public
    constructor Create(const DestinationStream: TStream);
    destructor Destroy; override;
    procedure AddDataToWrite(const Stream: TStream);
    function GetUnprocessedCount: Int64;
  end;

constructor TAccumulationThread.Create(const DestinationStream: TStream);
begin
  inherited Create(True);
  FDestinationStream := DestinationStream;
  InitCriticalSection(FCriticalSection);
  FDataStack := TObjectStack.Create;
end;

destructor TAccumulationThread.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  while FDataStack.Count > 0 do
    FDataStack.Pop.Free;
  FDataStack.Free;
  inherited Destroy;
end;

procedure TAccumulationThread.AddDataToWrite(const Stream: TStream);
begin
  EnterCriticalSection(FCriticalSection);
  FDataStack.Push(Stream);
  LeaveCriticalSection(FCriticalSection);
end;

function TAccumulationThread.GetUnprocessedCount: Int64;
begin
  EnterCriticalSection(FCriticalSection);
  Result := FDataStack.Count;
  LeaveCriticalSection(FCriticalSection);
end;

procedure TAccumulationThread.Execute;
var
  StreamToWrite: TStream = nil;
begin
  if not Assigned(FDestinationStream) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    EnterCriticalSection(FCriticalSection);
    if FDataStack.Count > 0 then
      StreamToWrite := TStream(FDataStack.Pop);
    LeaveCriticalSection(FCriticalSection);

    if Assigned(StreamToWrite) then
    begin
      FDestinationStream.CopyFrom(StreamToWrite, 0);
      FreeAndNil(StreamToWrite);
    end;

    Sleep(50);
  end;
end;

type
  // The bundling thread gathers file chunks from the file chunk manager and writes the encoded result into the accumulation thread.
  TBundlingThread = class(TThread)
  private
    FChunksManager: TFilesChunksManager;
    FAccumulationThread: TAccumulationThread;
  protected
    procedure Execute; override;
  public
    constructor Create(const ChunksManager: TFilesChunksManager; const AccumulationThread: TAccumulationThread);
  end;

constructor TBundlingThread.Create(const ChunksManager: TFilesChunksManager; const AccumulationThread: TAccumulationThread);
begin
  inherited Create(True);
  FChunksManager := ChunksManager;
  FAccumulationThread := AccumulationThread;
end;

procedure TBundlingThread.Execute;
var
  FilesChunk: TFileInfoArray;
  Stream: TStream;
begin
  if not Assigned(FChunksManager) or not Assigned(FAccumulationThread) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    FilesChunk := FChunksManager.GetNextFilesChunk;
    if not Assigned(FilesChunk) then
    begin
      Self.Terminate;
      Break;
    end;

    Stream := TMemoryStream.Create;
    BundleFilesChunk(FilesChunk, FChunksManager.FilesBasePath, Stream);
    FAccumulationThread.AddDataToWrite(Stream);
  end;
end;

procedure BundleFiles(const FilesChunks: TFilesChunks; const BasePath: String; const TargetStream: TStream);
var
  FilesChunksManager: TFilesChunksManager;
  AccumulationThread: TAccumulationThread;
  BundlingThreads: array of TBundlingThread;
  Index: ValSInt;
  BundlersFinished: Boolean;
begin
  // Prepare all threads and file chunks.
  FilesChunksManager := TFilesChunksManager.Create(BasePath, FilesChunks);

  AccumulationThread := TAccumulationThread.Create(TargetStream);
  AccumulationThread.FreeOnTerminate := True;
  AccumulationThread.Start;

  BundlingThreads := nil;
  SetLength(BundlingThreads, Math.Min(GetCPUCount, Length(FilesChunks)));
  for Index := 0 to High(BundlingThreads) do
  begin
    BundlingThreads[Index] := TBundlingThread.Create(FilesChunksManager, AccumulationThread);
    BundlingThreads[Index].FreeOnTerminate := True;
    // Let the games begin!
    BundlingThreads[Index].Start;
  end;

  // Wait for all bundler threads to finish work by polling every now and then.
  repeat
    BundlersFinished := True;
    for Index := 0 to High(BundlingThreads) do
      BundlersFinished := BundlersFinished and BundlingThreads[Index].Finished;
    Sleep(50);
  until BundlersFinished;
  SetLength(BundlingThreads, 0);

  // Wait for the accumulation thread to have written every remaining piece of data before shutting it down.
  while AccumulationThread.GetUnprocessedCount > 0 do
    Sleep(50);
  AccumulationThread.Terminate;

  FilesChunksManager.Free;
end;

end.
