unit UBundling;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  UFiles;

procedure BundleFiles(const FilesChunks: TFilesChunks; const TargetStream: TStream);

implementation

uses
  SysUtils,
  Math,
  Contnrs,
  UEncoding;

procedure BundleFilesChunk(const FilesChunkId: Int64; const FilesChunk: TFileInfoArray; const TargetStream: TStream);
var
  InputStream: TStream;
  OutputStream: TStream;
  FileInfo: TFileInfo;
  FileStream: TStream;
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

  try
    // Encode all file contents.
    InputStream.Position := 0;
    OutputStream := TMemoryStream.Create;
    UEncoding.Encode(InputStream, OutputStream);

    // Write ID of this chunk.
    TargetStream.WriteQWord(FilesChunkId);

    // Copy encoded stream contents over.
    TargetStream.CopyFrom(OutputStream, 0);
  finally                     
    InputStream.Free;
    OutputStream.Free;
  end;
end;

type
  // This class contains all file chunks that will be processed by the bundler threads.
  TFilesChunksManager = class
  private
    FFilesChunks: TFilesChunks;
    FFilesChunkIndex: ValSInt;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create(const FilesChunks: TFilesChunks);
    destructor Destroy; override;
    procedure GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray);
  end;

constructor TFilesChunksManager.Create(const FilesChunks: TFilesChunks);
begin
  inherited Create;
  FFilesChunks := FilesChunks;
  FFilesChunkIndex := 0;
  InitCriticalSection(FCriticalSection);
end;

destructor TFilesChunksManager.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TFilesChunksManager.GetNextFilesChunk(out FilesChunkId: ValSInt; out FilesChunk: TFileInfoArray);
begin
  EnterCriticalSection(FCriticalSection);
  if FFilesChunkIndex < Length(FFilesChunks) then
  begin
    FilesChunkId := FFilesChunkIndex;
    FilesChunk := FFilesChunks[FFilesChunkIndex];
    Inc(FFilesChunkIndex);
  end
  else
  begin
    FilesChunkId := -1;
    FilesChunk := nil;
  end;
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
  FilesChunkId: ValSInt;
  FilesChunk: TFileInfoArray;
  Stream: TStream;
begin
  if not Assigned(FChunksManager) or not Assigned(FAccumulationThread) then
    Self.Terminate;

  while not Self.Terminated do
  begin
    FChunksManager.GetNextFilesChunk(FilesChunkId, FilesChunk);
    if (FilesChunkId < 0) or not Assigned(FilesChunk) then
    begin
      Self.Terminate;
      Break;
    end;

    Stream := TMemoryStream.Create;
    BundleFilesChunk(FilesChunkId, FilesChunk, Stream);
    FAccumulationThread.AddDataToWrite(Stream);
  end;
end;

procedure BundleFiles(const FilesChunks: TFilesChunks; const TargetStream: TStream);
var
  FilesChunksManager: TFilesChunksManager;
  AccumulationThread: TAccumulationThread;
  BundlingThreads: array of TBundlingThread;
  Index: ValSInt;
  BundlersFinished: Boolean;
begin
  // Prepare all threads and file chunks.
  FilesChunksManager := TFilesChunksManager.Create(FilesChunks);

  AccumulationThread := TAccumulationThread.Create(TargetStream);
  AccumulationThread.Start;

  BundlingThreads := nil;
  SetLength(BundlingThreads, Math.Min(GetCPUCount, Length(FilesChunks)));
  for Index := 0 to High(BundlingThreads) do
  begin
    BundlingThreads[Index] := TBundlingThread.Create(FilesChunksManager, AccumulationThread);
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

  for Index := 0 to High(BundlingThreads) do
  begin
    BundlingThreads[Index].WaitFor;
    BundlingThreads[Index].Free;
  end;
  SetLength(BundlingThreads, 0);

  // Wait for the accumulation thread to have written every remaining piece of data before shutting it down.
  while AccumulationThread.GetUnprocessedCount > 0 do
    Sleep(50);
  AccumulationThread.Terminate;
  AccumulationThread.WaitFor;
  AccumulationThread.Free;

  FilesChunksManager.Free;
end;

end.
